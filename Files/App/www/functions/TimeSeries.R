# Time Series
ts_process <- function(df,focus){
  if(focus=="Country"){
    df_pre <- df[,c(5,8,4,6)]
  }
  else if(focus=="Product"){
    df_pre <- df[,c(5,9,4,6)]
  }
  else{
    df_pre <- df[,c(5,7,4,6)]
  }
  df_pre$Sales  <- df_pre$Quantity * df_pre$Price
  p1 <- df_pre[,c(1,3,5)]%>%
    group_by_at(colnames(df_pre)[1]) %>%
    summarise_at(colnames(df_pre)[c(3,5)],sum)
  p2 <- df_pre[,c(1,2,3,5)]%>%
    group_by_at(colnames(df_pre)[1:2]) %>%
    summarise_at(colnames(df_pre)[c(3,5)],sum)
  return(list("r1"=p1,"r2" = p2))
}

plot_TS <- function(df,obj="Profit",...){
  df_ts <- ts_process(df,...)$r1
  cn <- 3
  if(obj=="Quantity"){
    cn <- 2
  }
  fig <- plot_ly(df_ts, type = 'scatter', mode = 'lines')%>%
    add_trace(x = as.formula(paste0("~",colnames(df_ts)[1])),
              y = as.formula(paste0("~",colnames(df_ts)[cn])))%>%
    layout(showlegend = F)
  options(warn = -1)
  fig <- fig %>%
    layout(
      xaxis = list(zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      yaxis = list(zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      plot_bgcolor='#e5ecf6', width = 900)
  return(fig)
}

plot_TS_Ranking <- function(df,obj="Profit",N,... ){
  df_ts <- ts_process(df,...)$r2
  cn <- 4
  if(obj=="Quantity"){
    cn <- 3
  }
  top_N <- df_ts %>%
    group_by_at(colnames(df_ts)[2]) %>%
    summarise_at(colnames(df_ts)[cn],sum)%>%
    arrange_at(colnames(df_ts)[cn],funs(desc))
  top_N <- top_N$Country[1:10]
  single <- function(df,country){
    fig1 <- plot_ly(df[df_ts$Country==country,], type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
      add_trace(x = as.formula(paste0("~",colnames(df)[1])),
                y = as.formula(paste0("~",colnames(df)[3])), name = country)
    return(fig1)
  }
  
  result = list()
  for (i in c(1:N)) {
    result[[i]] <- plotly_build(single(df_ts %>% 
                                         arrange(by=colnames(df_ts)[cn],desc=T),
                                       top_N[i]))
  }
  
  
  fig <- subplot(result,
                 nrows = as.integer(N/2)) %>% layout(
                   xaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   yaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   plot_bgcolor='#e5ecf6') %>%
    layout(legend=list(title=rev(top_N))) 
  
  fig$x$layout$legend$traceorder = "normal"
  return(fig)
  
}

