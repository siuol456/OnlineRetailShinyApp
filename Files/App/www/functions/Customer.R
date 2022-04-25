cusrank <- function(df,start,end) {
  func_test <- df %>% 
    filter(!is.na(Customer.ID)) %>%
    filter_at(colnames(df)[5],any_vars((.<end) &(.>start))) %>%
    group_by(Customer.ID) %>%
    summarize(Totalspend = sum(Quantity*Price)) %>%
    mutate(rank = rank(-Totalspend))
  
  allsum <- df %>% 
    filter(!is.na(Customer.ID)) %>%
    filter_at(colnames(df)[5],any_vars((.<end) &(.>start))) %>%
    summarize(Totalspend = sum(Quantity*Price)) 
  
  countcus <- df %>%
    filter(!is.na(Customer.ID)) %>%
    filter_at(colnames(df)[5],any_vars((.<end) &(.>start))) %>%
    summarize(c = length(unique(`Customer.ID`)))
  
  func_test$allmean <- allsum/countcus
  func_test$allmean <- unlist(func_test$allmean)
  func_test$Customer.ID <- factor(func_test$Customer.ID)
  return(func_test)
}

cusrank_plot <- function(df,n,start,end){
  func_test <- df %>%
    filter(rank<n)
  fig <- func_test %>%
    mutate(`Customer.ID` = fct_reorder(`Customer.ID`, desc(Totalspend))) %>%
    ggplot(aes(x = `Customer.ID`, y = Totalspend,fill = `Customer.ID`)) +
    geom_bar(stat="identity")  + 
    labs(title=paste("Top",as.character(n), "Customer Spend from",start,"to",end),y="Total Spend(£)", subtitle =paste("mean:",round(func_test$allmean,2),"£"), color=NULL)+
    theme(
      # Top-right position
      legend.pos = c(0.575, 0.975),
      # Elements within a guide are placed one next to the other in the same row
      legend.direction = "horizontal",
      axis.title.x = element_blank(),
      # Light background color
      plot.background = element_rect(fill = "#F5F4EF", color = NA),
      plot.title = element_text(
        margin = margin(-1, -1, 15, 0), 
        size = 13, 
        family = "", 
        face = "bold", 
        vjust = 0, 
        color = "grey25"
      ),
      # Specify color for the tick labels along both axes 
      axis.text = element_text(color = "grey40"),
      # Specify face and color for the text on top of each panel/facet
      strip.text = element_text(face = "bold", color = "grey20"),
      plot.caption = element_text(size = 11),
      axis.text.x = element_text(size=10, angle=30)
    )
  return(fig)
}




cusspend <- function(df,start,end,ID,R){
  each <- df %>%
    filter_at(colnames(df)[5],any_vars((.<end) &(.>start))) %>%
    filter(Customer.ID == ID)%>%
    group_by(Customer.ID,Description) %>%
    summarize(Totalspend_each = sum(Quantity*Price)) %>%
    mutate(rank = dense_rank(-Totalspend_each)) %>%
    filter(rank <= R) %>%
    arrange(by=rank,desc=T)
  return(each)
}


     