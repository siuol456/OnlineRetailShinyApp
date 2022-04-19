#Define UI
ui <- 
  navbarPage("Online Retail", collapsible = TRUE, inverse = TRUE, 
             theme = shinytheme("sandstone"),
             tags$head(tags$style(HTML('.navbar-static-top {background-color: black;}', '.navbar-default .navbar-nav>.active>a {background-color: black;}'))),
             tabPanel("About",
                      fluidRow(
                        column(6,
                               includeMarkdown("www/About.md")),
                        column(3,
                               img(src="about.jpg"))
                        )),
             navbarMenu("Reports",
                        tabPanel("Customers",
                                 fluidRow(
                                   column(2,
                                          radioButtons("C_data","Data:",
                                                       choices = c("Cancel"=F,"Purchase"=T),
                                                       selected = T),
                                          dateRangeInput(
                                            inputId="dates_C",
                                            "Date Range",
                                            start = "2009-12-01",
                                            end = "2011-12-09",
                                            min = "2009-12-01",
                                            max = "2011-12-09"
                                          ),
                                          radioGroupButtons(
                                            "ind_obj",
                                            "Subject",
                                            choices = c("Specific Customer","Top N Customers"),
                                            selected = "Specific Customer",
                                            status = "primary"
                                          )  ,
                                          parameter_tabs,
                                          ),
                                   column(10,
                                          parameter_tabs2,
                                          )
                                   )
                                 ),
                        tabPanel("Products",
                                 fluidPage(
                                   titlePanel("Top 8 Sales/Cancelation"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       helpText(),
                                       dateRangeInput(
                                         inputId="dates",
                                         "Date Range",
                                         start = "2009-12-01",
                                         end = "2011-12-09",
                                         min = "2009-12-01",
                                         max = "2011-12-09"
                                       ),
                                       radioButtons(
                                         inputId = "radio_cancel",
                                         "Cancelled Products?",
                                         choices = list("TRUE", "FALSE"),
                                         selected = "FALSE"
                                       ),
                                       radioButtons(
                                         inputId = "radio_y",
                                         "What are you interested in?",
                                         choices = list("amount", "count"),
                                         selected = "count"
                                       ),
                                       uiOutput("products")
                                     ),
                                     mainPanel(
                                       tabsetPanel(tabPanel("Top 8",
                                         plotlyOutput(
                                         "plotChart",
                                         height = "600px"
                                       )),
                                       tabPanel("Single Product Sale",
                                                plotlyOutput(
                                                  "plotTS",
                                                  height = "600px"
                                                ))
                                     )
                                 )))),
                        tabPanel("Country",
                                 fluidPage(
                                   
                                   # Sidebar 
                                   sidebarPanel(
                                     width=3,
                                     helpText("Summary by Country"),
                                     
                                     radioGroupButtons(
                                       inputId = "data",
                                       label = "Data:", 
                                       choices = c("Buy", "Cancel"),
                                       selected = 'Buy',
                                       status = "primary"
                                     ),
                                     radioGroupButtons(
                                       inputId = "sort",
                                       label = "Sort: ", 
                                       choices = c('Price','Quantity','Transaction'),
                                       selected = 'Price',
                                       status = "primary"
                                     ),
                                     sliderTextInput(
                                       inputId = "rank",
                                       label = "Rank:", 
                                       choices = seq(1, 41),
                                       selected = c(1,41),
                                       grid = TRUE
                                     )
                                   ),
                                   mainPanel(
                                     fluidPage(
                                       fluidRow(
                                         column(12, DT::dataTableOutput("countryDataSet"))
                                       ),
                                       fluidRow(
                                         column(4, plotOutput("countryDonut")),
                                         column(8, plotOutput("countryMap", width = "700px", height = "400px"))
                                       )
                                     )
                                   )
                                 ))),
             navbarMenu("TimeLine",
                        tabPanel("General",
                                 fluidRow(
                                   column(2,
                                          selectInput("ts_g_cancel",
                                                      "Topic",
                                                      c("Purchase"=T,
                                                        "Cancelation"=F)),
                                          radioButtons("ts_g_obj",
                                                       "Objective",
                                                       choices = list("Purchase",
                                                                      "Quantity"))
                                          ),
                                   column(10,
                                          plotlyOutput(
                                            "generalts",
                                            height = "600px"
                                          ))
                                   )
                                 ),
                        tabPanel("Top Countries",
                                 fluidRow(
                                   column(2,
                                          selectInput("ts_i_cancel",
                                             "Topic",
                                             c("Purchase"=T,
                                               "Cancelation"=F)),
                                          radioButtons("ts_i_obj",
                                              "Objective",
                                              choices = list("Purchase",
                                                             "Quantity")),
                                          numericInput("ts_i_rank",
                                              "Top N",
                                               3,
                                              min = 3,
                                              max = 12)),
                                  column(10,
                                         plotlyOutput(
                                         "rankts",
                                         height = "600px")))
                                 )
                        ),
             tabPanel("Credit",
                      fluidRow(
                        column(6,
                               includeMarkdown("www/Credit.md")),
                        column(3,
                               img(src="Credit.jpg"))
                      ))
  )
