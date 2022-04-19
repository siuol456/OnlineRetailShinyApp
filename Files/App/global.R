library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(plotly)
library(ClusterR)
library(cluster)
library(factoextra)
library(DT)
library(webr)
library(lubridate)

df_net    <- read.csv('www/data/Netpurchase.csv')
df_cancel <- read.csv('www/data/Cancelation.csv')
source("www/functions/Top_8.R")
source("www/functions/TimeSeries.R")
source("www/functions/Country.R")
source("www/functions/Customer.R")

parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("Specific Customer",
           textInput("CID","Customer ID:",value="18102",placeholder = "18102")),
  tabPanel("Top N Customers",
           numericInput("TopC", "Top N Customer:",value = 5,min= 3,max=15),
           radioButtons("det", "Specific Rank",
                        choices = c("Yes"=T,"No"=F),selected = F),
           uiOutput("ind_det"))
 )

parameter_tabs2 <- tabsetPanel(
  id = "outR",
  type = "hidden",
  tabPanel("Specific Customer",
           dataTableOutput("CustomerT"),
           textOutput("totalP"),
           tags$head(tags$style("#totalP{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
           )
           )),
  tabPanel("Top N Customers",
           plotOutput("TopCP"),
           dataTableOutput("SRC")
           )
)




