### MARKETING ANALYTICS ###



#installing required packages
install.packages("plotly")

library(ggplot2)
library(lmtest)
library(tseries)
library(tidyverse)
library(plotly)


current_path = rstudioapi::getActiveDocumentContext()$path
current_dir = dirname(current_path)


#importing data
data <- read.csv(paste(current_dir,"DATA//Marketing Insight.csv", sep = "/"))

#filtering for MAnufacturing Sector
data = data[data$sector == 1,]




### Descriptive Stats

#mkt size 

#PROBLEM!!!: not all firms each year

sales_by_year = summarise_at(group_by(data, year), vars(sales), funs(sum))
colnames(sales_by_year) <- c("year", "tot_sales")

fig1 <-  plot_ly(x = sales_by_year$year, y = sales_by_year$sales, type = 'bar') %>%
  layout(title = 'Market Size by Year',
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'), 
         yaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'))

fig1


# Market Competition

comps_by_year = summarise_at(group_by(data, year), vars(id),funs(length))


fig2 <-  plot_ly(x = comps_by_year$year, y = comps_by_year$id, type = 'bar') %>%
  layout(title = 'Number of Competitors by Year',
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'), 
         yaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'))
fig2

#todo: HHI

#market shares:
#for each firm, sales for the year/totsales for that year

# total 4 each year --> sales_by_year$sales

# assign to each firm in [data] value 

data = list(data, sales_by_year)
data = data %>% reduce(full_join, by = data$year)

data$share = (data$sales/data$tot_sales)*100

shares = data[c("year", "share")]
shares$share_sq = data$share^2
hhi_year = summarise_at(group_by(shares, year),vars(share_sq), funs(sum))

colnames(hhi_year) <- c("year", "hhi")

fig3 <-  plot_ly(x = hhi_year$year, y = hhi_year$hhi, type = 'bar') %>%
  layout(title = 'HH Index by Year',
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'), 
         yaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'))
fig3


# TODO: rd vs advertising STACKED, diff colours

rd_ad_year = summarise_at(group_by(data, year), vars(ad,rd), funs(sum))




























