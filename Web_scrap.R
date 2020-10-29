# WEB SCRAPPING #####

library(rvest)
url <- 'http://books.toscrape.com/catalogue/category/books/travel_2/index.html'
titles <- read_html(url) %>% 
  html_nodes('h3') %>%
  html_nodes('a') %>% 
  html_text()

titles
library(stringr)
titles <- read_html(url) %>% 
  html_nodes('h3') %>%
  html_nodes('a') %>% 
  html_text(
  );    
urls <- read_html(url) %>%
  html_nodes('.image_container') %>% 
  html_nodes('a') %>% 
  html_attr('href') %>% 
  str_replace_all('../../../', '/')
imgs <- read_html(url) %>% 
  html_nodes('.image_container') %>%
  html_nodes('img') %>%
  html_attr('src') %>%
  str_replace_all('../../../../', '/')
ratings <- read_html(url) %>% 
  html_nodes('p.star-rating') %>% 
  html_attr('class') %>% 
  str_replace_all('star-rating ', '')
prices <- read_html(url) %>% 
  html_nodes('.product_price') %>% 
  html_nodes('.price_color') %>% 
  html_text()
availability <- read_html(url) %>% 
  html_nodes('.product_price') %>% 
  html_nodes('.instock') %>% 
  html_text() %>% 
  str_trim()
scraped <- data.frame(
  Title = titles, 
  URL = urls, 
  SourceImage = imgs, 
  Rating = ratings, 
  Price = prices, 
  Availability = availability
)
scraped


#### WEB APPLICATION #####

library(shiny);
library(rvest);
library(stringr);
library(glue);

ui <- fluidPage()
server <- function(input, output) {}

shinyApp(ui=ui, server=server)

ui <- fluidPage(
  column(12, tags$h2('Dynamic web scraper with R')),
  sidebarPanel(
    width=3,
    selectInput(
      inputId='genreSelect',
      label='Genre',
      choices=c('Business', 'Classics', 'Fiction', 'Horror', 'Music'),
      selected='Business',
    )
  ),
  mainPanel(
    width=9,
    tableOutput('table')
  )
)


#### SERVER FUNCTION ###

server <- function(input, output) {
  output$table <- renderTable({
    mappings <- c('Business' = 'business_35', 'Classics' = 'classics_6', 'Fiction' = 'fiction_10',
                  'Horror' = 'horror_31', 'Music' = 'music_14') 
    
    url <- glue('http://books.toscrape.com/catalogue/category/books/', mappings[input$genreSelect], '/index.html')
    
    titles <- read_html(url) %>% 
      html_nodes('h3') %>%
      html_nodes('a') %>% 
      html_text()
    
    urls <- read_html(url) %>%
      html_nodes('.image_container') %>% 
      html_nodes('a') %>% 
      html_attr('href') %>% 
      str_replace_all('../../../', '/')
    
    imgs <- read_html(url) %>% 
      html_nodes('.image_container') %>%
      html_nodes('img') %>%
      html_attr('src') %>%
      str_replace_all('../../../../', '/')
    
    ratings <- read_html(url) %>% 
      html_nodes('p.star-rating') %>% 
      html_attr('class') %>% 
      str_replace_all('star-rating ', '')
    
    prices <- read_html(url) %>% 
      html_nodes('.product_price') %>% 
      html_nodes('.price_color') %>% 
      html_text()
    
    availability <- read_html(url) %>% 
      html_nodes('.product_price') %>% 
      html_nodes('.instock') %>% 
      html_text() %>% 
      str_trim()
    
    data.frame(
      Title = titles, 
      URL = urls, 
      SourceImage = imgs, 
      Rating = ratings, 
      Price = prices, 
      Availability = availability
    )
  })
};

shinyApp(ui=ui, server=server)
