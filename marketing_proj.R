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

# checking magnitude of panel imbalance:
obs_count = summarise_at(group_by(data, id), vars(year), funs("length"))
colnames(obs_count) = c("id", "n_obs")

obs_list = list()
range = 1:27
for (i in range) {
  n_rows = nrow(obs_count[obs_count$n_obs >= i,])/299
  obs_list <- append(obs_list, n_rows)
}

vline <- function(x = 0, color = "red") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}

fig0 = plot_ly(x = range, y = obs_list, type = "bar") %>%
  layout(title = "% observations at different thresholds", shapes = vline(2.5)) %>%
  add_text(showlegend = FALSE, x = 3, y = -0.1, text = "3", color = 'red')
fig0


# market size

sales_by_year = summarise_at(group_by(data, year), vars(sales), funs(sum))
colnames(sales_by_year) <- c("year", "tot_sales")

fig1 <-  plot_ly(x = sales_by_year$year, y = sales_by_year$tot_sales, type = 'bar') %>%
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



data = list(data, sales_by_year)
data = data %>% reduce(full_join, by = data$year)

data$share = (data$sales/data$tot_sales)*100 #multiplied to compute hhi easily

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



#R&D vs Ads Spending

rd_ad_year = summarise_at(group_by(data, year), vars(ad, rd), funs(sum))
rd_ad_year$rd_ratio = rd_ad_year$rd/sales_by_year$tot_sales

# absolute
fig4 <- plot_ly(x = rd_ad_year$year, y = rd_ad_year$rd, type = "bar", name = "R&D Spending")
fig4 <- fig4 %>% add_trace(y = rd_ad_year$ad, name = "Advertising Spending") 
fig4 <- fig4 %>% layout(yaxis = list(title = "Total"), barmode = "stack")
fig4

attach(rd_ad_year)
rd_ad_year$tot_spending = ad + rd

#relative
fig5 <- plot_ly(x = year, y = rd/tot_spending, type = "bar", name = "%R&D")
fig5 <- fig5 %>% add_trace(y = ad/tot_spending, name = "%Ad")
fig5 <- fig5 %>% layout(yaxis = list(title = "% Spending"), barmode = "group")
detach(rd_ad_year)
fig5

# TODO
  #check correlations
  #check values (eg mkt>0)
  #hausman test

























