server <- function(input, output) {
  values <- reactiveValues(tbl=NULL,
                           data_a = NULL,
                           prod_name = NULL,
                           tb12  =NULL,
                           t=NULL,
                           tbl3=NULL,
                           tempCID = "18102"
                           )
  observeEvent(input$radio_cancel,{
    if(input$radio_cancel){
    values$data_a <- df_cancel
    }
    else{
      values$data_a <- df_net
      }
    })
  
  output$plotChart <- renderPlotly({
       top8_bar(values$data_a, input$dates[1], input$dates[2], input$radio_cancel, input$radio_y)$result
  })
  
  output$products <- renderUI({
    values$prod_name <- top8_bar(values$data_a, input$dates[1], input$dates[2], input$radio_cancel, input$radio_y)[["table"]]["Description"]
    selectInput(inputId = "products", label = "Products interested", 
                choices = values$prod_name, 
                multiple = FALSE)
  })
  
  output$plotTS <- renderPlotly({
    week_ts(values$data_a, input$products)
  })
  
#------  
  output$generalts <- renderPlotly({
    if(input$ts_g_cancel){
      plot_TS(df_net,input$ts_g_obj,focus="Country")
    }
    else{
      plot_TS(df_cancel,input$ts_g_obj,focus="Country")
    }
  })
  output$rankts <- renderPlotly({
    if(input$ts_i_cancel){
      plot_TS_Ranking(df_net,input$ts_i_obj,N=input$ts_i_rank,
                      focus="Country")
    }
    else{
      plot_TS_Ranking(df_cancel,input$ts_i_obj,N=input$ts_i_rank,
              focus="Country")
    }
  })
  
  
#--------  
  observeEvent(c(input$data, input$sort, input$rank), {
    if (input$data == 'Buy') {
      values$tbl.country <- df_net
    }
    else {
      values$tbl.country = df_cancel
    }
    
    values$tbl.country <- .fn.country.summary(values$tbl.country,
                                              sort=input$sort, rank= input$rank)
    
    output$countryDonut <- renderPlot({
      .fn.country.draw.piedonut(values$tbl.country)
    })
    
    output$countryDataSet <- DT::renderDataTable({
      tryCatch({
        values$tbl.country %>% select(Country, Price, Quantity, Transaction, Rank, `Ratio(%)`)
      },
      error = function(e) {
        stop(safeError(e))
      })
    },
    extensions = c('Scroller', 'FixedColumns'), options = list(
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 200,
      scroller = TRUE,
      dom = 'Bfrtip',
      fixedColumns = TRUE
    )) 
    
  })
  
  output$countryMap <- renderPlot({
    .fn.country.draw.ggplot2(values$tbl.country)
  })
  
#--------
  observeEvent(input$C_data, {
    if(input$C_data){
      values$data_a <- df_net
      values$t <- "Total Purchase: "
    }
    else{
      values$data_a <- df_cancel
      values$t <- "Total Cancelation: "
    }
  }) 
  observeEvent(input$ind_obj, {
    updateTabsetPanel(inputId = "params", selected = input$ind_obj)
  }) 
  observeEvent(values$data_a,{
    values$tbl2 <- cusspend(values$data_a,input$dates_C[1],
                            input$dates_C[2],values$tempCID,10)
  })
  observeEvent(input$CID, {
    values$tempCID <- input$CID
    values$tbl2 <- cusspend(values$data_a,input$dates_C[1],
                            input$dates_C[2],values$tempCID,10)
    
  })
  observeEvent(values$tbl2,{
    output$totalP <- renderText({
      paste0(values$t," ",values$tbl2$Totalspend_each[1],"£")
    })
    output$CustomerT <- renderDataTable({
      values$tbl2[,c(3:4)]
      },
      extensions = c('Scroller', 'FixedColumns'), options = list(
        deferRender = TRUE,
        scrollX = TRUE,
        scrollY = 200,
        scroller = TRUE,
        dom = 'Bfrtip',
        fixedColumns = TRUE
      )
    )
  })
  
  
  observeEvent(input$TopC, {
    output$ind_det <- renderUI({
      if(input$det){
        numericInput("SR","Specify Rank",value=1,min = 1,max=input$TopC)
      }
      else{
        NULL
      }
    })
    
  })
  
  observeEvent(input$ind_obj, {
    updateTabsetPanel(inputId = "outR", selected = input$ind_obj)
  }) 
  

  output$TopCP <- renderPlot({
    cusrank(values$data_a,input$dates_C[1],
            input$dates_C[2],input$TopC)$P
  })
  
  observeEvent(input$SR,{
    values$tbl3 <- cusrank(values$data_a,input$dates_C[1],
                    input$dates_C[2],input$TopC)$T %>%
      filter(rank==input$SR)
  })
  
  observeEvent(values$tbl3,{
    output$SRC <- renderDataTable({
      cusspend(values$data_a,input$dates_C[1],
               input$dates_C[2],values$tbl3$Customer.ID[1],10)[,c(3:4)]
    },
    extensions = c('Scroller', 'FixedColumns'), options = list(
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 200,
      scroller = TRUE,
      dom = 'Bfrtip',
      fixedColumns = TRUE
    )
    )
  })
  
  
  
  
  
  
}

