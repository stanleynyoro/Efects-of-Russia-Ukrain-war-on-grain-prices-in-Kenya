library(shiny)
library(rvest)
library(tidyverse)
library(readr)
library(stringr)
library(lubridate)
#############################Data#####################################

maize.data1=read_html("http://amis.co.ke/site/market?product=1&per_page=2400") %>%
    html_table() %>%
    as.data.frame()

maize.data2=read_html("http://amis.co.ke/site/market/2400?product=&per_page=2400") %>%
    html_table() %>%
    as.data.frame()

maize.data3=read_html("http://amis.co.ke/site/market/4800?product=&per_page=2400") %>%
    html_table() %>%
    as.data.frame()

maize.data<-rbind(maize.data1,maize.data2,maize.data3)
##################################Wheat Data##################################

wheat.data1=read_html("http://amis.co.ke/site/market?product=3&per_page=2400") %>%
    html_table() %>%
    as.data.frame()


wheat.data2=read_html("http://amis.co.ke/site/market/2400?product=3&per_page=2400") %>%
    html_table() %>%
    as.data.frame()

wheat.data3=read_html("http://amis.co.ke/site/market/4800?product=3&per_page=2400") %>%
    html_table() %>%
    as.data.frame()

wheat.data<-rbind(wheat.data1,wheat.data2,wheat.data3)

##############################Food price data################

food.price.data=rbind(maize.data,wheat.data)

food.price.data$Retail=str_replace(food.price.data$Retail,"-"," ")
food.price.data$Retail=str_replace(food.price.data$Retail,"NA"," ")
food.price.data$Retail=parse_number(food.price.data$Retail)

food.price.data$Wholesale=str_replace(food.price.data$Wholesale,"-"," ")
food.price.data$Wholesale=str_replace(food.price.data$Wholesale,"NA"," ")
food.price.data$Wholesale=parse_number(food.price.data$Wholesale)

food.price.data$Commodity=as.factor(food.price.data$Commodity)
food.price.data$Classification=as.factor(food.price.data$Classification)

#######################Removing outliers######################################
food.price.data=food.price.data %>%
    filter(Wholesale>=0) %>%
    filter(Retail>=0)

maize_outliers=filter(food.price.data,Commodity=="Dry Maize")
wheat_outliers=filter(food.price.data,Commodity=="Wheat")

quartile.w_m<- quantile(maize_outliers$Wholesale, probs=c(.01, .99), na.rm = FALSE)
quartile1.w_w<-quantile(wheat_outliers$Wholesale, probs=c(.01, .99), na.rm = FALSE)

quartile.r_m<- quantile(maize_outliers$Retail, probs=c(.01, .99), na.rm = FALSE)
quartile1.r_w<-quantile(wheat_outliers$Retail, probs=c(.01, .99), na.rm = FALSE)


food.price.data=mutate(food.price.data,maize_w.out=ifelse(Wholesale>=quartile.w_m[1]& Wholesale<=quartile.w_m[2],1,0))
food.price.data=mutate(food.price.data,wheat_w.out=ifelse(Wholesale>=quartile1.w_w[1]& Wholesale<=quartile1.w_w[2],1,0))

food.price.data=food.price.data %>%
    mutate(wholesale.out=maize_w.out+wheat_w.out) %>%
    filter(wholesale.out>0)

food.price.data=mutate(food.price.data,maize_r.out=ifelse(Retail>=quartile.r_m[1]& Retail<=quartile.r_m[2],1,0))
food.price.data=mutate(food.price.data,wheat_r.out=ifelse(Retail>=quartile1.r_w[1]& Retail<=quartile1.r_w[2],1,0))

food.price.data=food.price.data %>%
    mutate(Retail.out=maize_r.out+wheat_r.out) %>%
    filter(Retail.out>0) %>%
    select(Commodity,Classification,Market,Wholesale,Retail,Supply.Volume,County,Date)

######################################Date variable##############################
food.price.data$Date<-as.POSIXct(food.price.data$Date,format="%Y-%m-%d")
food.price.data$Date<-as.Date(food.price.data$Date)
food.price.data$Year=format(food.price.data$Date, format="%Y")
food.price.data$Month=months(food.price.data$Date)

############################Effect of war#######################################

mydate="2022-02-24"
war.d<-as.POSIXct(mydate,format="%Y-%m-%d")
war.d<-as.Date(war.d)

food.price.data =food.price.data %>%
    mutate(Period=ifelse(Date<war.d,0,1)) %>%
    mutate(Period=factor(Period,labels=c("Before war","After war"),levels=c(0,1)))

#################################R shiny App####################################
ui <- fluidPage(
    theme=shinythemes::shinytheme('cerulean'),
    titlePanel("Price changes of grains as a result of Russia-Ukraine War"),
       HTML("<p> The dashboard represents retail & wholesale prices of grains in Kenya. The data has been mined from
       <a href='http://amis.co.ke/site/market'>Ministry of Agriculture</a> website and is published daily.
       A before-after analysis has been done on price changes of maize and wheat in Kenya a result of Russia-Ukraine War. The reference
            date is 24th February 2022 when war began. 
            Notably, no complex causality analysis has been done on whether the changes are attributed to the war: That said,
            Kenya does import a significant amount of wheat from Ukraine.
            </p>"),
    sidebarLayout(
        sidebarPanel(
    selectInput("Commodity","Select Commodity",unique(food.price.data$Commodity)),
    selectInput("Classification","Classification",unique(food.price.data$Classification)),
    selectInput("County","County",unique(food.price.data$County)),
    dateRangeInput("Date", "Date range:",
                   start  = min(food.price.data$Date),
                   end    = max(food.price.data$Date))),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Whole data",DT::DTOutput("data.t")),
                     tabPanel("Monthly Average price (Per Kg)",tableOutput("table1")),
                     tabPanel("Price changes (Per Kg)",tableOutput("table2"))
    )
    )
)
)
server <- function(input, output) {
    filter.d<-reactive({
        food.price.data %>%
        subset(Date>=input$Date[1] & Date<=input$Date[2]) %>%
            filter(Commodity==input$Commodity) %>%
            filter(Classification==input$Classification) %>%
            filter(County==input$County)
            
    })
    prices.f<-reactive({
        filter.d() %>%
            filter(Year==2022) %>%
            group_by(Month) %>%
            summarise("Average Wholesale price"=mean(Wholesale),
                      "Average Retail price"=mean(Retail)) %>%
            as.data.frame()

    })
    price.22<-reactive({
        prices.f() %>%
            mutate(
                Month=match(Month,month.name),
                Year="2022") %>%
            arrange(Month) %>%
            mutate(Month=month.abb[Month]) %>%
            as.data.frame()
    
    })
    prices.l<-reactive({
        filter.d() %>%
            filter(Year==2021) %>%
            group_by(Month) %>%
            summarise("Average Wholesale price"=mean(Wholesale),
                      "Average Retail price"=mean(Retail)) %>%
            as.data.frame()
        
    })
    price.21<-reactive({
        prices.l() %>%
            mutate(
                Month=match(Month,month.name),
                Year="2021") %>%
            arrange(Month) %>%
            mutate(Month=month.abb[Month]) %>%
            as.data.frame()
        
    })
    combined.d<-reactive({
        rbind(price.21(),price.22())
    })
    period.w<-reactive({
        filter.d() %>%
            group_by(Period) %>%
            summarise("Avg wholesale price"=mean(Wholesale),
                      "Avg retail price"=mean(Retail)) %>%
            as.data.frame()
            
    })
    output$data.t<-DT::renderDT({
        options(digits=2)
        filter.d()
        
    })
    output$table1<-renderTable({
        options(digits=2)
        combined.d()
    })
    output$table2<-renderTable({
        options(digits=2)
        period.w()
    })

   
}

# Run the application 
shinyApp(ui = ui, server = server)
