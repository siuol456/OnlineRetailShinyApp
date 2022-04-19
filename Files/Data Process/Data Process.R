library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)
#data source: https://archive-beta.ics.uci.edu/ml/datasets/online+retail+ii

# As the data is in excel format with 2 seperate sheet, 
# we need to import twice and merge them.
df1 <- read_excel('online_retail_II.xlsx', sheet = 1)
df2 <- read_excel('online_retail_II.xlsx', sheet = 2)
df <- rbind(df1,df2)
#Keep an back up for error and faster process
df_back <- df
# In case anything went wrong
# df <- df_back
#--------------

#Create a new column for product category that will be se in model.
df$cat <- sapply(df$StockCode, substring,1,3)


# Drop the time details na djust keep the date
df$InvoiceDate <- as.character(df$InvoiceDate)
df$InvoiceDate <- sapply(str_split(df$InvoiceDate, " "),"[[",1)

# Unify the product code and drop admin testing entries
df$ProductCode <- as.integer(str_match(df$StockCode, "[0-9]{5}"))
df <- drop_na(df, "ProductCode")

# Drop admin testing entries
df <- df %>% filter(Quantity != 0) %>% filter(!is.na(`Customer ID`))

# Separate the data into cancel and buy
df_cancel <- df %>% filter(Quantity < 0)
df_buy <- df %>% filter(Quantity > 0)

# Optimize cancel data
df_cancel$Quantity <- -df_cancel$Quantity
names(df_cancel)[names(df_cancel) == "Invoice"] <- "CancelInvoice"
names(df_cancel)[names(df_cancel) == "InvoiceDate"] <- "CancelInvoiceDate"

# Find the matched canceled sales
df_cancel <- df_cancel %>%
  mutate(id = row_number()) %>% # using row number to prevent duplicated rows
  left_join(df_buy, by = c(
    "StockCode", "Description", "Quantity", "Price",
    "Customer ID", "Country","cat","ProductCode"
  )) %>%
  distinct(id, .keep_all = T)  %>% # remove duplicated rows
  select(-id)

# Drop the canceled entries and create the net sale
df_buy_net <- anti_join(df_buy, df_cancel, by = c(
  "Invoice", "StockCode", "Description", "Quantity", "InvoiceDate", 
  "Price", "Customer ID", "Country", "ProductCode","cat"))


# Save the data
write_csv(df_buy_net,'Netpurchase.csv')
write_csv(df_cancel,'Cancelation.csv')

