library(tidyverse)
library(shiny)
library(rvest)
library(shinydashboard)

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
myurl
remove(url)


get.data <- function(x){
  myurl <- read_html("https://coinmarketcap.com/gainers-losers/") # read our webpage as html
  myurl <- html_table(myurl)  # convert to an html table for ease of use
  to.parse <- myurl[[1]]  # pull the first item in the list
  to.parse$`% 1h` <- gsub("%","",to.parse$`% 1h`) # cleanup - remove non-characters
  to.parse$`% 1h`<- as.numeric(to.parse$`% 1h`) #cleanup - convert percentages column to numeric so we can sort
  to.parse$Symbol <- as.factor(to.parse$Symbol) # cleanup - convert coin symbol to factor
  
  to.parse$Symbol <- factor(to.parse$Symbol,
                            levels = to.parse$Symbol[order(to.parse$'% 1h')])  # sort by gain value
  to.parse # return the finished data.frame
}

remove(get.data)
get.data()
