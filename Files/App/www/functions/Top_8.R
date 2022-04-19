#Function for top 8 report
top8_bar <- function(df, start, end, cancel, value){
  
  top <- df
  if (value == "count") {
    
    top <- top %>% 
      filter(top$InvoiceDate >= start & top$InvoiceDate <= end) %>%
      select(Description, Quantity) %>%
      dplyr::group_by(Description) %>%
      dplyr::summarise(n = sum(Quantity)) 
    
    top <- head(top[order(top$n, decreasing=T),], 8)
    if (cancel) {
      t = "Top 8 Cancellation"
    }
    else{
      t ="Top 8 Sale"
    }
  }
  else if (value == "amount"){
    top <- top %>% 
      filter(top$InvoiceDate >= start & top$InvoiceDate <= end) %>%
      mutate(Amount = Quantity*Price) %>%
      select(Description, Amount) %>%
      dplyr::group_by(Description) %>%
      dplyr::summarise(n = sum(Amount))
    
    top <- head(top[order(top$n, decreasing=T),], 8)
    if (cancel) {
      t = "Top 8 Deduction"
    }
    else{
      t ="Top 8 Revenue"
    }
  }
  result <- plot_ly(
    data = top, 
    x = ~reorder(Description, -n), y = ~n, color = ~Description,
    type = 'bar') %>%
    layout(title = t)
  
  
  result <- result %>% layout(showlegend = FALSE, 
                              xaxis = list(title = "Description"))
  
  
  return(list("result" = result, "table" = top))
  
}


week_ts <- function(df, prod_name) {
  df_prod <- subset(df, Description == prod_name)
  
  df_prod_ts <- df_prod %>% 
    group_by_at(colnames(df_prod)[5]) %>% 
    summarize(QuantityS = sum(Quantity))
  
  fig <- plot_ly()%>%
    add_trace(data=df_prod_ts, type = 'scatter', mode = 'lines', fill = 'tozeroy', 
              x = as.formula(paste0("~",colnames(df_prod_ts)[1])), 
              y = ~QuantityS, name = prod_name)%>%
    layout(showlegend = F, 
           yaxis = list(zerolinecolor = '#ffff',
                        zerolinewidth = 2,
                        gridcolor = 'ffff'),
           xaxis = list(zerolinecolor = '#ffff',
                        zerolinewidth = 2,
                        gridcolor = 'ffff'),
           plot_bgcolor='#e5ecf6')
  
  options(warn = -1)
  
  fig <- fig %>%
    layout(
      xaxis = list(zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      yaxis = list(zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      plot_bgcolor='#e5ecf6', width = 900) %>%
    layout(title=prod_name)
  
  return(fig)
}
