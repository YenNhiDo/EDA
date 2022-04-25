#------------------------------ Online Retail ---------------------------------#
# Link: https://archive.ics.uci.edu/ml/datasets/Online+Retail
# Read Data
library('readxl')
library('dplyr')
library('ggplot2')
library('lubridate')
data <- read_xlsx('Online Retail.xlsx')

# EDA
str(data)
summary(data)
glimpse(data)

# 1. Cancelled Orders:
cancelled_orders <- data %>% filter(substr(InvoiceNo, 1, 1) == "C")
summary(cancelled_orders)

# a. Per Cancelled Order Summary:
per_cancelled_order_summary <- cancelled_orders %>% group_by(InvoiceNo) %>% 
  mutate(revenue_lost = (-Quantity) * UnitPrice) %>% 
  summarize(Total_Items = length(unique(StockCode)), Total_Units = -sum(Quantity),
            Avg_Unit_Price = mean(UnitPrice), Total_Revenue_Lost = sum(revenue_lost),
            .groups = 'drop') %>%  arrange(desc(Total_Revenue_Lost))

cancelled_orders$InvoiceDate <- as_date(cancelled_orders$InvoiceDate)
cancelled_orders$InvoiceDate <- strftime(cancelled_orders$InvoiceDate, format="%Y-%m")

# b. Total Revenue Lost Due To Cancellation Per Invoice Month:
revenue_lost_per_month <- cancelled_orders %>% select(InvoiceDate, Quantity, UnitPrice) %>% 
  group_by(InvoiceDate) %>% summarize(Total_Revenue_Lost = sum((-Quantity) * UnitPrice), 
                                      .groups = 'drop') 

ggplot(revenue_lost_per_month, aes(x = InvoiceDate, y = Total_Revenue_Lost, group = 1 )) + 
  geom_line(color = 'steelblue', size = 1.5) +
  ggtitle('Total Revenue Lost Due To Cancellation Per Invoice Month') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(label = revenue_lost_per_month$Total_Revenue_Lost,
            nudge_x = 0.1, nudge_y = 0.1, check_overlap=T)

# c. Total Cancelled Orders Per Invoice Month & Total Units Returned Per Invoice Month:
total_cancellation_orders_and_items_per_month <- cancelled_orders %>% 
  group_by(InvoiceDate) %>% summarize(Total_Orders = length(unique(InvoiceNo)), 
                                      Total_Units = -sum(Quantity), 
                                      .groups = 'drop')

ggplot(total_cancellation_orders_and_items_per_month, aes(x = InvoiceDate, y = Total_Orders, group = 1)) + 
  geom_line(color = '#52854C', linetype = "3313") + geom_point(color = '#52854C') +
  geom_text(label = total_cancellation_orders_and_items_per_month$Total_Orders,
    nudge_x = 0.36, nudge_y = 0.1, check_overlap=T) +
  ggtitle('Total Cancelled Orders Per Invoice Month') + theme(plot.title = element_text(hjust = 0.5))

ggplot(total_cancellation_orders_and_items_per_month, aes(x = InvoiceDate, y = Total_Units, group = 1)) +
  geom_line(color = 'darkred', linetype = 3, size = 0.75) + geom_point(color = 'darkred') +
  geom_text(label = total_cancellation_orders_and_items_per_month$Total_Units,
            nudge_x = 0.5, nudge_y = 0.5, check_overlap=T) +
  ggtitle('Total Units Returned Per Invoice Month') + theme(plot.title = element_text(hjust = 0.5))

# d. Number of Cancelled Orders In Top 5 Countries:
cancelled_orders_per_country <- cancelled_orders %>% group_by(Country) %>%  
  summarize(Number_of_Cancelled_Orders = length(unique(InvoiceNo)), .groups = 'drop') %>% 
  arrange(desc(Number_of_Cancelled_Orders)) %>% head(10)
  
ggplot(cancelled_orders_per_country , aes(x = reorder(Country, -Number_of_Cancelled_Orders), y = Number_of_Cancelled_Orders)) +
  geom_bar(stat='identity', fill = '#D16103') +
  ggtitle('Number of Cancelled Orders In Top 5 Countries') +
  theme(axis.text.x = element_text(angle=45, hjust=0.9)) + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab('Country') + 
  ylab('Number of Cancelled Orders')

# 2. Orders:
orders <- data %>% filter(substr(InvoiceNo, 1, 1) != "C")
summary(orders)
orders1 <- orders %>% filter(!UnitPrice <= 0)

orders1$InvoiceDate <- as_date(orders1$InvoiceDate)
orders1$InvoiceDate <- strftime(orders1$InvoiceDate, format="%Y-%m")

# a. Per Order Summary:
per_order_summary <- orders1 %>% group_by(InvoiceNo) %>% 
  summarize(Total_Items = length(unique(StockCode)), Total_Units = sum(Quantity),
            Avg_Unit_Price = mean(UnitPrice), Total_Revenue = sum(Quantity * UnitPrice),
            .groups = 'drop') %>% arrange(desc(Total_Revenue))

# b. Total Revenue Per Invoice Month:
revenue_per_month <- orders1 %>% select(InvoiceDate, Quantity, UnitPrice) %>% 
  group_by(InvoiceDate) %>% summarize(Total_Revenue = sum((Quantity) * UnitPrice), 
                                      .groups = 'drop')

ggplot(revenue_per_month, aes(x = InvoiceDate, y = Total_Revenue, group = 1 )) + 
  ggtitle('Total Revenue Per Invoice Month') +
  theme(plot.title = element_text(hjust = 0.5)) + geom_area(fill = '#E7B800', color = 'yellow') +
  geom_text(label = revenue_per_month$Total_Revenue,
            nudge_x = 0, nudge_y = 0.5, check_overlap=T)

# c. Orders Made In The Last 0 Years 3 Months 120 Days:
orders2 <- orders %>% filter(!UnitPrice <= 0)
orders2$InvoiceDate <- as_date(orders2$InvoiceDate)
orders2$InvoiceDate <- strftime(orders2$InvoiceDate, format="%Y-%m-%d")

last_3m120d <- ymd(max(orders2$InvoiceDate)) - years(0) - months(3) - days(120) 

orders_last_3m120d <- orders2 %>% filter(InvoiceDate > last_3m120d) %>% 
  group_by(InvoiceNo) %>% summarize(Total_Items = length(unique(StockCode)), 
                                    Total_Units = sum(Quantity),
                                    Avg_Unit_Price = mean(UnitPrice), 
                                    Total_Revenue = sum(Quantity * UnitPrice),
                                    .groups = 'drop') %>% arrange(desc(Total_Revenue), 
                                                                  desc(Total_Units))
