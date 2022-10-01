#### MARKETING ANALYTICS ####

#installing required packages
# install.packages("Rtools")
# install.packages("plotly")
# install.packages("plm")

library(plm)
library(lmtest)
library(tseries)
library(tidyverse)
library(plotly)

#env cleanup
rm(list = ls())

#context
current_path = rstudioapi::getActiveDocumentContext()$path
current_dir = dirname(current_path)

#importing data
data <- read.csv(paste(current_dir,"DATA//Marketing Insight.csv", sep = "/"))

#filtering for Manufacturing Sector
data = data[data$sector == 1,]
#dropping null
data <- drop_na(data)
data <- data[order(data$id, data$year),]

#### Descriptive Stats ####

# checking magnitude of panel imbalance:
obs_count = summarise_at(group_by(data, id), vars(year), funs("length"))
colnames(obs_count) = c("id", "n_obs")

obs_list = list()
range = 1:27
for (i in range) {
  n_rows = nrow(obs_count[obs_count$n_obs >= i,])/299
  obs_list <- append(obs_list, n_rows)
}
############# IGNORE 
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
################################################# END

#### OBSERVATION THRESHOLDS ####
fig0 = plot_ly(x = range, y = obs_list, type = "bar") %>%
  layout(title = "% observations at different thresholds", shapes = vline(2.5)) %>%
  add_text(showlegend = FALSE, x = 3, y = -0.1, text = "3", color = 'red')
fig0

#removing too_small firm series
under_4 <- subset(obs_count,n_obs<=4)$id
data <- subset(data, !(id %in% under_4))

# market size
sales_by_year = summarise_at(group_by(data, year), vars(sales), funs(sum))
colnames(sales_by_year) <- c("year", "tot_sales")

#### MARKET SIZE GRAPH ####
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

#### MARKET COMPETITION GRAPH ####
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

#### HHI GRAPH ####
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

#### RD VS ADS ABSOLUTE
fig4 <- plot_ly(x = rd_ad_year$year, y = rd_ad_year$rd, type = "bar", name = "R&D Spending")
fig4 <- fig4 %>% add_trace(y = rd_ad_year$ad, name = "Advertising Spending") 
fig4 <- fig4 %>% layout(yaxis = list(title = "Total"), barmode = "stack")
fig4

attach(rd_ad_year)
rd_ad_year$tot_spending = ad + rd

#### RD VS ADS RELATIVE
fig5 <- plot_ly(x = year, y = rd/tot_spending, type = "bar", name = "%R&D")
fig5 <- fig5 %>% add_trace(y = ad/tot_spending, name = "%Ad")
fig5 <- fig5 %>% layout(yaxis = list(title = "% Spending"), barmode = "group")
detach(rd_ad_year)
fig5



#### CREATING RISK MEASURE ####
data$rom <- rep(0,nrow(data))



#computing returns on market
add_rom <- function() {
  rom <- data$rom
  for (firm in unique(data$id)) {
    
    years = unique(data[data$id == firm,]$year)
    l = length(years)
     
    firm_data = data[data$id ==  firm,]
    # print(firm_data)
    for (y in 2:l) { #sanity check 4 consecutive years?
      t = years[y]
      t_1 = years[y-1]
      
      replacement = firm_data[firm_data$year == t,]$mv - firm_data[firm_data$year == t_1,]$mv
      # print("TO REPLACE:")
      # print(data[data$id == firm & data$year == t,]$rom)
      # print("REPLACEMENT:")
      # print(replacement)
      
      #might be wrong oopsie (replace on data index, not on rom index)
      rom <- replace(rom, data$id == firm & data$year == t, replacement )
    }
  }
  return(rom)
}


data$rom <- add_rom()

#computing standard deviation on 3years


sd3years_rom <- function() {
  sd_3 <- rep(0,nrow(data))
  for (firm in unique(data$id)) {
    years = unique(data[data$id == firm,]$year)
    l = length(years)
    
    firm_data = data[data$id ==  firm,]
    # print(firm_data)
    for (y in 3:l) {
      t = years[y]
      t_3 = years[y-2]
      focus = subset(firm_data, (year <= t & year >= t_3))$rom
        
      replacement = sd(focus)
      sd_3 <- replace(sd_3, data$id == firm & data$year == t, replacement)
    }
  }
  return(sd_3)
}

sd3years_mv <- function() {
  sd_3 <- rep(0,nrow(data))
  for (firm in unique(data$id)) {
    years = unique(data[data$id == firm,]$year)
    l = length(years)
    
    firm_data = data[data$id ==  firm,]
    # print(firm_data)
    for (y in 3:l) {
      t = years[y]
      t_3 = years[y-2]
      focus = subset(firm_data, (year <= t & year >= t_3))$mv
      
      replacement = sd(focus)
      sd_3 <- replace(sd_3, data$id == firm & data$year == t, replacement)
    }
  }
  return(sd_3)
}

data$sdev_rom <- sd3years_rom()
data$sdev_mv <- sd3years_mv()

#removing insufficient datapoints
data <- subset(data, sdev_rom != 0)

#extra control variables:

data$roa = data$earnings/data$assets



#export
write_csv(data, "C:\\Users\\carlo\\Desktop\\MARKETING\\SCRIPTS\\data_clean.csv")


# TODO:
  #check correlations
  #check values (eg mkt>0)
  #hausman test






















