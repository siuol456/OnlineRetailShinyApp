#Function for top 8 report
top8_df <- function(df, cancel, value){
  
  top <- df_net 
  nam <- colnames(top)
  if (value == "count") {
    
    top <- top %>% 
      select(Description, Quantity,ends_with('Date')) %>%
      dplyr::group_by_at(nam[c(3,5)]) %>%
      dplyr::summarise(n = sum(Quantity)) 
    
    if (cancel) {
      t = "Top 8 Cancellation"
    }
    else{
      t ="Top 8 Sale"
    }
  }
  else if (value == "amount"){
    top <- top %>% 
      mutate(Amount = Quantity*Price) %>%
      select(Description, Amount,ends_with('Date')) %>%
      dplyr::group_by_at(nam[c(3,5)]) %>%
      dplyr::summarise(n = sum(Amount))
    
    
    if (cancel) {
      t = "Top 8 Deduction"
    }
    else{
      t ="Top 8 Revenue"
    }
  }
  return(list("data" = top, "plot_N" = t))
} 
  
top8 <- function(df,start, end){
    top <-df %>% 
      filter_at(colnames(df)[2],any_vars((.<end) &(.>start))) %>%
      dplyr::group_by(Description) %>%
      dplyr::summarise(n_ = sum(n))
    top <- head(top[order(top$n_, decreasing=T),], 8)
    return(top)
}

top8_plot <- function(df,N){
  result <- plot_ly(
    data = df, 
    x = ~reorder(Description, -n_), y = ~n_, color = ~Description,
    type = 'bar') %>%
    layout(title = N)
  result <- result %>% layout(showlegend = FALSE, 
                              xaxis = list(title = "Description"))
  return(result)
}

week_ts <- function(df, prod_name) {
  df_prod <- subset(df, Description == prod_name)
  
  
  fig <- plot_ly()%>%
    add_trace(data=df_prod, type = 'scatter', mode = 'lines', fill = 'tozeroy', 
              x = as.formula(paste0("~",colnames(df_prod)[2])), 
              y = ~n, name = prod_name)%>%
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
