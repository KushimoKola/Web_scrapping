library(tidyverse)
library(shiny)
library(rvest)
library(shinydashboard)
library(RColorBrewer)

# 
# tidyverse_conflicts()
# 
# if (!require (dplyr)) install.packages("dplyr")
# if (!require (tidyr)) install.packages("tidyr")
# if (!require (tibble)) install.packages("tibble")
# if (!require (readr)) install.packages("readr")
# ##if (!require (broom)) install.packages("broom")
# if (!require (ggplot2)) install.packages("ggplot2")
# if (!require (purrr)) install.packages("purrr")
# if (!require (stringr)) install.packages("stringr")
# if (!require (forcats)) install.packages("forcats")
# 
# install.packages("xml2")
# 
# install.packages("readr")
# 
# install.packages("rvest")
# 
library(xml2)
# library(rvest)

url <- read_html("https://coinmarketcap.com/gainers-losers/")
xml_name(url)
url<- html_table(url)
url
url[[1]]
url
remove(url)


get.data <- function(x){
  myurl <- read_html("https://coinmarketcap.com/gainers-losers/") # read our webpage as html
  myurl <- html_table(myurl)  # convert to an html table for ease of use
  to.parse <- myurl[[3]]  # pull the first item in the list
  to.parse$`% 24h` <- gsub("%","",to.parse$`% 24h`) # cleanup - remove non-characters
  to.parse$`% 24h`<- as.numeric(to.parse$`% 24h`) #cleanup - convert percentages column to numeric so we can sort
  to.parse$Symbol <- as.factor(to.parse$Symbol) # cleanup - convert coin symbol to factor
  
  to.parse$Symbol <- factor(to.parse$Symbol,
                            levels = to.parse$Symbol[order(to.parse$`% 24h`)])  # sort by gain value
  to.parse # return the finished data.frame
}

remove(get.data)
get.data()

get.infobox.val <- function(x){
  
  df1 <- get.data() # run the scraping function above and assign that data.frame to a variable
  df1 <- df1$`% 24h`[1]  # assign the first value of the % gain column to same variable
  df1   # return value
} 

  get.infobox.coin <- function(x){
  
  df <- get.data()  # run the scraping function above and assign that data.frame to a variable
  df <- df$Name[1]  # assign the first value of the name column to same variable
  df   # return value
  
  }
  
  get.infobox.price <- function(x){
    
    df <- get.data()  # run the scraping function above and assign that data.frame to a variable
    df <- df$Price[1]  # assign the first value of the name column to same variable
    df   # return value
    
  }
  

get.infobox.val()

get.infobox.coin()

get.infobox.price ()


#### UI WITH SHINY

ui <- dashboardPage(
  
  
  # H E A D E R
  
  dashboardHeader(title = "Coin Top Gainers"),
  # S I D E B A R
  
  dashboardSidebar(
    
    h5("This slightly interactive dashboard pulls the top gainers from the last 24 hours from
       coinmarketcap.com, The list is refreshed every 60 seconds."),
    
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    h6("Built by Kolawole Kushimo in the R computing language"),
    h6("Octave Analytics Data Analytics Team (2020)"),
     h6("Octave Analytics: An Analytics and end-to-end Customer Value Management Process Outsourcing Services company, which deals with the application
of computing tools, statistics and mathematical models to solve business and industry problem."),
    br(),
    br(),
    br(),
    h6("Octave Analytics: 'Make Your Data Sing'"),
    h6 ("URL https://www.octaveanalytics.com/"),
    br(),
    br(),
    a("kolawole@octaveanalytics.com", href="kolawole@octaveanalytics.com")
    
  ),
  
  # B O D Y
  dashboardBody(
    
    fluidRow(
      
      # InfoBox
      infoBoxOutput("top.coin",
                    width = 4),
      
      # InfoBox
      infoBoxOutput("top.name",
                    width = 4),
      
      # InfoBox
      infoBoxOutput("top.price",
                    width = 4)
      
    ),
    
    fluidRow(
      column(
        # Datatable
        box(
          status = "primary",
          headerPanel("Data Table"),
          solidHeader = F,
          br(),
          
        ## install package DT
          DT::dataTableOutput("table", height = "270px"),
          width = 10,
          height = "180px"
        ),
        
        # Chart
        box(
          status = "primary",
          headerPanel("Chart"),
          solidHeader = F,
          br(),
          plotOutput("plot", height = "270px"),
          width = 10,
          height = "230px"
        ),
        width = 12
      )
      
    )
  )
  
)

#install.packages("RColorBrewer")
#library(RColorBrewer)

server <- function(input, output) {
  # R E A C T I V E 
  liveish_data <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get.data()                # call our function from above
  })
  
  
  live.infobox.val <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get.infobox.val()         # call our function from above
  })
  
  
  live.infobox.coin <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get.infobox.coin()        # call our function from above
  })
  
  live.infobox.price <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get.infobox.price()        # call our function from above
  })
  
  # D A T A   T A B L E   O U T P U T
  output$table <- DT::renderDataTable(DT::datatable({
    data <- liveish_data()}))
  
  # P L O T   O U T P U T
  output$plot <- renderPlot({ (ggplot(data=liveish_data(),       aes(x=Symbol, y=`% 24h`)) +
                                 geom_bar(stat="identity", fill = "springgreen3") +
                                 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                 ggtitle("Gainers from the 24 Last Hours"))
  })
  
  # I N F O B O X   O U T P U T - V A L
  output$top.coin <- renderInfoBox({
    infoBox(
      "Gain in Last 24 Hour",
      paste0(live.infobox.val(), "%"),
      icon = icon("signal"),
      color = "purple",
      fill = TRUE)
  })
  
  # I N F O B O X   O U T P U T - N A M E
  output$top.name <- renderInfoBox({
    infoBox(
      "Coin Name",
      live.infobox.coin(),
      icon = icon("bitcoin"),
      color = "blue",
      fill = TRUE)
  })
  
  # I N F O B O X   O U T P U T - PRICE
  output$top.price <- renderInfoBox({
    infoBox(
      "Coin Price",
      live.infobox.price(),
      icon = icon("dollar"),
      color = "light-blue",
      fill = TRUE)
  })
  
  
}

## Let's Deploy our App

shinyApp(ui = ui, server = server)
