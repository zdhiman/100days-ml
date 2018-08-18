# Modelling churn
## https://towardsdatascience.com/modelling-customer-churn-when-churns-are-not-explicitly-observed-with-r-a768a1c919d5

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gdata)

theme_set(theme_minimal())


raw.data <- read_delim("data/OnlineRetail.csv", delim = ';', locale = )
data <- raw.data

data$InvoiceDate <- as.POSIXct(data$InvoiceDate, format="%d.%m.%y %H:%M",tz=Sys.timezone())

data$UnitPrice <-  gsub(",", ".", data$UnitPrice) %>%
  as.numeric()

data$Total <- data$Quantity * data$UnitPrice

txns <- data %>% 
  mutate(CustomerID = as.factor(CustomerID),
         InvoiceDate = InvoiceDate) %>%
  group_by(CustomerID, InvoiceNo, InvoiceDate) %>% 
  summarise(Spend = sum(Total)) %>%
  ungroup() %>% 
  filter(Spend>0)

time_between <- txns %>% 
  arrange(CustomerID, InvoiceDate) %>% 
  group_by(CustomerID) %>% 
  mutate(dt = as.numeric(InvoiceDate - lag(InvoiceDate), unit=  'days')) %>% 
  ungroup() %>% 
  na.omit()

Ntrans = txns %>% 
  group_by(CustomerID) %>% 
  summarise(N = n()) %>%
  filter(N>20)

sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  grps = tbl %>% groups %>% lapply(as.character) %>% unlist
  keep = tbl %>% summarise() %>% ungroup() %>% sample_n(size, replace, weight)
  tbl %>% right_join(keep, by=grps) %>% group_by_(.dots = grps)
}

ecdf_df <- time_between %>% group_by(CustomerID) %>% arrange(dt) %>% mutate(e_cdf = 1:length(dt)/length(dt))
sample_users <- ecdf_df %>% inner_join(Ntrans) %>% sample_n_groups(20)

ggplot(data = time_between %>% inner_join(Ntrans) %>% filter(CustomerID %in% sample_users$CustomerID), aes(dt)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), bins = 15) + 
  facet_wrap(~CustomerID) +
  labs(x = 'Time Since Last Purchase (Days)',y = 'Frequency')

ggplot(data = ecdf_df %>% inner_join(Ntrans) %>% filter(CustomerID %in% sample_users$CustomerID), aes(dt,e_cdf) ) + 
  geom_point(size =0.5) +
  geom_line() + 
  geom_hline(yintercept = 0.9, color = 'red') + 
  facet_wrap(~CustomerID) +
  labs(x = 'Time Since Last Purchase (Days)')

getq <- function(x,a = 0.9){
  if(a>1|a<0){
    print('Check your quantile')
  }
  X <- sort(x)
  e_cdf <- 1:length(X) / length(X)
  aprx = approx(e_cdf, X, xout = c(0.9))
  return(aprx$y)
}
percentiles = time_between %>% 
  inner_join(Ntrans) %>% 
  filter(N>5) %>% 
  group_by(CustomerID) %>% 
  summarise(percentile.90= getq(dt)) %>% 
  arrange(percentile.90)

percentiles[ which(percentiles$CustomerID==12748), ]

txns[ which(txns$CustomerID==12748), ]

percentiles[ which(percentiles$CustomerID==13102), ]
txns[ which(txns$CustomerID==13102), ]

