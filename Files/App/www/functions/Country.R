# create summary dataframe group by 'Country'
.fn.country.summary = function(df, sort="Price", rank= c(1, 10)) {
  numOfCountry = length(unique(df$Country))
  
  min = rank[1]
  max = rank[2]
  
  if (max > numOfCountry) {
    max = numOfCountry
  }
  
  df %>% 
    group_by(Country) %>%
    summarise(
      Transaction=n(), 
      Quantity=sum(Quantity), 
      Price=round(sum(Price))
    ) %>%
    mutate(sort_column = .[[sort]]) %>%
    arrange(desc(sort_column)) %>%
    mutate(Rank=rank(desc(sort_column))) %>%
    head(max) %>%
    tail(max - min + 1) %>%
    mutate(
      `Ratio(%)` = round(sort_column/sum(sort_column)*100, 2)
    )
}

# Function #2: Recode to match the country name in ggplot2's 'mapdata'
.fn.country.recode = function(col) {
  recode(col,
         'United States' = 'USA',
         'United Kingdom' = 'UK',
         'West Indies' = 'Virgin Islands',
         'Korea' = 'South Korea',
         'Hong Kong' = 'China',
         'EIRE' = 'Ireland',     # Republic of Ireland
         'RSA' = 'South Africa', #  Republic of South Africa (RSA)
         #         'Channel Islands' = 'UK',
         #'European Community' = '',
         #'Unspecified' = ''
  )
}

#  Draw a world map using ggplot2.
.fn.country.draw.ggplot2 = function(df) {
  df$Country <- .fn.country.recode(df$Country)
  
  map <- left_join(map_data("world"), df, by = c('region' = 'Country'))
  
  map$Rank <- as.character(map$Rank)
  
  ggplot(map) +
    geom_polygon(
      aes(
        x = long, y = lat, group = group,
        fill = (Rank)
      ), 
      color='#666666') +
    scale_fill_viridis_d() +
    theme_void() +
    theme(
      legend.position = "none"
    )
}


# Function #4: Draw a world map using ggplot2.
.fn.country.draw.ggplotly = function(df) {
  ggplotly(.fn.country.draw.ggplot2(df)) 
}


# Function #4: Draw a world map using ggplot2.
.fn.country.draw.piedonut = function(df) {
  PieDonut(df,
           aes(Country, count = Price),
           showPieName = F,
           explodePie = T,
           r0 = 0.3,
           start = 3 * pi / 2,
           showRatioThreshold = 0.05,
           labelpositionThreshold = 0.05) +
    theme(
      panel.border = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
    )
}

# Function for Customer

Customerplot <- function(df,y,m,n) {
  func_test <- df %>%
    filter(!is.na(Customer)) %>%
    group_by(year = lubridate::floor_date(InvoiceDate, "year"),month = lubridate::floor_date(InvoiceDate, "month"),Customer) %>%
    summarize(Totalspend = sum(Quantity*Price)) %>%
    mutate(rank = rank(-Totalspend)) %>%
    filter(rank <= n) %>%
    filter(year(ymd(year)) == y) %>%
    filter(month(ymd(month)) == m)
  func_test$mean <- mean(func_test$Totalspend)
  func_test$Customer <- factor(func_test$Customer)
  func_test %>%
    mutate(Customer = fct_reorder(Customer, desc(Totalspend))) %>%
    ggplot(aes(x = Customer, y = Totalspend,fill = Customer)) +
    geom_bar(stat="identity")  + geom_hline(yintercept = func_test$mean,color = "red")+
    theme(axis.text.x = element_text(size=10, angle=30))
}
