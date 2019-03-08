#load the required packages

#install.packages( "ggplot2" )
library("ggplot2")


#install.packages( "mapproj" )
library("mapproj")
# 

#install.packages( "maps" )
library("maps")


# setwd("D://Dashboard_BI//sample_revenue_dashboard_shiny-master//sample_revenue_dashboard_shiny-master")
# getwd()
# 
#install.packages("shiny")
 library(shiny)
# 
# 
# install.packages("shinydashboard")
 library(shinydashboard)
# 
# install.packages("ggplot2")
 library(ggplot2)
# 
# install.packages("dplyr")
 library(dplyr)

recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)


df <- aggregate( chickwts$weight, by=list( chickwts$feed ), FUN=mean )
names( df ) <- c( "feed", "weight" )
df <- df[ order( -df$weight ), ]



  
Resolvtime <- data.frame(Months = c('2018-10','2018-11','2018-12','2019-01','2019-02','2019-03','2019-04','2019-05','2019-06'), Cal_days= c(6,5.5,5,6,4,5.5,4.5,5,6.5))
  
#ggplot( df, aes( x="", y=weight, fill=feed ) ) + geom_bar( stat="identity", width=0.25 ) + ggtitle( "Mean Chicken Weighty by Feed Type" )


#ggplot( df, aes( x="", y=weight, fill=feed ) ) + geom_bar( stat="identity" ) + coord_polar( "y", start=0 ) + ggtitle( "Mean Chicken Weighty by Feed Type" )

#head(recommendation)
#library( ggplot2 )

# states <- map_data( "state" )
# coords <- read.csv( "cities-coords.csv", header=TRUE, sep="," )
# data <- read.csv( "cities-data.csv", header=TRUE, sep="," )
# data <- data[ data$City != "Houston" & data$Year == 2012, ]
# points <- merge( coords, data, by.x = c( "City", "State" ), by.y = c( "City", "State" ) )
# points$Size <- pmax( points$Population / 500000.0, rep( 5.0, 6 ) )
# map <- ggplot() + geom_map( data=states, map=states, aes( x=long, y=lat, map_id=region ), fill="white", colour="black" )
# map <- map + geom_point( data=points, aes( x=Longitude, y=Latitude ), colour="blue", size=points$Size, alpha=0.7 )
# map <- map + coord_map( "albers", lat0=29.5, lat1=49.5 )



 states <- map_data( "state" )
 choropleth <- data.frame( ID = tolower( rownames( state.x77 ) ), pop = state.x77[ , 1 ] )
 map <- ggplot() + geom_map( data=states, map=states, aes( x=long, y=lat, map_id=region ), fill="white", colour="black" )
 map <- map + geom_map( data=choropleth, map=states, aes( fill=pop, map_id=ID ) )
 map <- map + coord_map( "albers", lat0=29.5, lat1=49.5 )
#print( map )

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Critical Statistics")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Machine Learning", tabName = "new_dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.salesforce.com")
  )
)


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
  
  box(
    title = "Ticket Queue Bar Graph"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyPrd", height = "200px")
  )
  
  ,box(
    title = "SSL Certifcates Pie Graph"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyRegion", height = "200px")
  ) 
  
  ,box(
    title = "Monthly Tickets Bar Graph"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyRegionnew", height = "200px")
  ) 
  
  ,box(
    title = "Ticket Priority Bar Graph"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyRegionnewtwo", height = "200px")
  ) 
  
  
  
)



# creating fraw for 2nd page



frowA2 <- fluidRow(
  valueBoxOutput("valueA")
  ,valueBoxOutput("valueB")
  ,valueBoxOutput("valueC")
)

frowB2 <- fluidRow(
  
  box(
    title = "Resolution Time Trend"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyPrd2", height = "200px")
  )
  
  ,box(
    title = "Resolution Time Analysis"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyRegion2", height = "200px")
  ) 
  
  ,box(
    title = "Revenue per Product new"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyRegionnew2", height = "200px")
  ) 
  
  ,box(
    title = "Revenue per Product new"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyRegionnewtwo2", height = "200px")
  ) 
  
  
  
)


# combine the two fluid rows to make the body
#body <- dashboardBody(frow1, frow2)


body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            frow1, frow2
    ),
    
    # Second tab content
    tabItem(tabName = "new_dashboard",
            frowA2, frowB2
    )
  )
)


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values of KPI boxes
  total.revenue <- sum(recommendation$Revenue)
  sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sales.account$value, format="d", big.mark=',')
      ,paste('Critical Events:',sales.account$Account)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total.revenue, format="d", big.mark=',')
      ,'Critical Devices'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")
    
  })
  
  
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      ,paste('Healthy Devices:',prof.prod$Product)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  # CREATING FOR SECOND Page
  
  #creating the valueBoxOutput content
  output$valueA <- renderValueBox({
    valueBox(
      formatC(sales.account$value, format="d", big.mark=',')
      ,paste('Top Account new:',sales.account$Account)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  
  
  output$valueB <- renderValueBox({
    
    valueBox(
      formatC(total.revenue, format="d", big.mark=',')
      ,'Total Expected Revenue'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")
    
  })
  
  
  
  output$valueC <- renderValueBox({
    
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      ,paste('Top Product:',prof.prod$Product)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  
  
  #creating the plotOutput content
  
  output$revenuebyPrd <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Product, y=Revenue, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Product") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Product") + labs(fill = "Region")
  })
  
  
  
  output$revenuebyRegion <- renderPlot({ggplot( df, aes( x="", y=weight, fill=feed ) ) + geom_bar( stat="identity", width=0.25 ) + ggtitle( "Mean Chicken Weighty by Feed Type" )
  })
  
  output$revenuebyRegionnew <- renderPlot({ggplot( df, aes( x="", y=weight, fill=feed ) ) + geom_bar( stat="identity" ) + coord_polar( "y", start=0 ) + ggtitle( "Mean Chicken Weighty by Feed Type" )
  })
  
  output$revenuebyRegionnewtwo <- renderPlot({ggplot( data=df, aes( x=weight ) ) + geom_histogram( binwidth=8, color="black", fill="lightblue", alpha=0.7 ) + ylab( "" ) + ggtitle( "Chicken Weight Counts" )
  })
  
  
  
  #creating the plotOutput content FOR SECOND PAGE
  
  output$revenuebyPrd2 <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Product, y=Revenue, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Product") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Product") + labs(fill = "Region")
  })
  

  
#x_lbl <- row.names( Resolvtime )
x_lbl=c('2018-10','2018-11','2018-12','2019-01','2019-02','2019-03','2019-04','2019-05','2019-06')

x_lbl <- factor( x_lbl, levels=unique( x_lbl ) )

 


 
 # output$revenuebyRegion2 <- renderPlot({ggplot( df, aes( x="", y=weight, fill=feed ) ) + geom_bar( stat="identity", width=0.25 ) + ggtitle( "Mean Chicken Weighty by Feed Type" )
 # })
  
   output$revenuebyRegion2 <- renderPlot({ggplot( data=Resolvtime, aes(x=x_lbl, y=Cal_days, group=1 ) ) + geom_line( colour="red", linetype="dashed", size=1.0 ) + geom_point( color="red", size=3.0, shape=1 ) + xlab( "Year-Months" ) + ggtitle( "Resolution Time Trend by ALL" ) +ylim(0,7)
  })
  
  
  output$revenuebyRegionnew2 <- renderPlot({ggplot( df, aes( x="", y=weight, fill=feed ) ) + geom_bar( stat="identity" ) + coord_polar( "y", start=0 ) + ggtitle( "Mean Chicken Weighty by Feed Type" )
  })
  
#  output$revenuebyRegionnewtwo2 <- renderPlot({ggplot( data=df, aes( x=weight ) ) + geom_histogram( binwidth=8, color="black", fill="lightblue", alpha=0.7 ) + ylab( "" ) + ggtitle( "Chicken Weight Counts" )
 # })
  
  #print( map )
  
  output$revenuebyRegionnewtwo2 <- renderPlot({map
  }) 
  
}


shinyApp(ui, server)


#install.packages('rsconnect')

#rsconnect::setAccountInfo(name='myfirstwebapp', token='A7FD17142F39B5AD53BB625328CF61C0', secret='d6nUrGDYvs7rL8zImpdZ23ohccC6agZQeVWvXmpy')


#library(rsconnect)
#rsconnect::deployApp( 'D://Dashboard_BI//sample_revenue_dashboard_shiny-master//sample_revenue_dashboard_shiny-master')


