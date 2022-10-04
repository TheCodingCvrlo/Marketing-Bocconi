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
library(corrplot)
library(stringr)

#env cleanup
rm(list = ls())

#context
current_path = rstudioapi::getActiveDocumentContext()$path
current_dir = dirname(current_path)

#importing data
df <- read.csv(paste(current_dir,"DATA//Marketing Insight.csv", sep = "/"))

#filtering for Manufacturing Sector
df = df[df$sector == 1,]

#dropping null
df <- drop_na(df)
df <- df[order(df$id, df$year),]

# checking magnitude of panel imbalance:
obs_count = summarise_at(group_by(df, id), vars(year), funs("length"))
colnames(obs_count) = c("id", "n_obs")

obs_list = list()
range = 1:27
for (i in range) {
  n_rows = nrow(obs_count[obs_count$n_obs >= i,])/299
  obs_list <- append(obs_list, n_rows)
}

# removing too_small firm series 
under_4 <- subset(obs_count,n_obs<=3)$id
df <- subset(df, !(id %in% under_4))

###################### Additional Variables ###############################

# aggregated sales
sales_by_year = summarise_at(group_by(df, year), vars(sales), funs(sum))
colnames(sales_by_year) <- c("year", "tot_sales")

# adding to main df
df = list(df, sales_by_year)
df = df %>% reduce(full_join, by = df$year)

# shares; shares^2, hhi
df$share = (df$sales/df$tot_sales)*100 #multiplied to compute hhi easily
df$share_sq = df$share^2
hhi_year = summarise_at(group_by(df, year),vars(share_sq), funs(sum))
colnames(hhi_year) <- c("year", "hhi")

# adding hhi to main df
df = list(df, hhi_year)
df = df %>% reduce(full_join, by = df$year)

# aggregated ad/rd spending
rd_ad_year = summarise_at(group_by(df, year), vars(ad, rd, mkt), funs(sum))
rd_ad_year$tot_spending = rd_ad_year$ad + rd_ad_year$rd
rd_ad_year$rd_ratio = rd_ad_year$rd/sales_by_year$tot_sales
rd_ad_year$ad_ratio = rd_ad_year$ad/sales_by_year$tot_sales
rd_ad_year$mkt_ratio = rd_ad_year$mkt/sales_by_year$tot_sales
#roa 
df$roa = df$earnings/df$assets

#rd/sales
df$rd_ratio = df$rd/df$sales
df$ad_ratio = df$ad/df$sales

#logassets
df$log_assets = log(df$assets)

df2 = df

#################### Discontinuities ##############################

find_error <- function() {
  broken_ids = list()
  for (firm in unique(df$id)) {
    years = unique(df[df$id == firm,]$year)
    l = length(years)
    for (y in 2:l) { #sanity check 4 consecutive years?
      t = years[y]
      t_1 = years[y-1]
      if (t - t_1 > 1) {
        print(str_glue("MISSING OBSERVATION AT id = {firm}, year = {t}"))
        broken_ids = append(broken_ids, firm)
      }
    }
  }
  return(broken_ids)
}

broken_ids <- find_error()

df_balanced <- subset(df, !(id %in% broken_ids))

######################## CREATING RISK MEASURE ###############################

df$rom <- rep(0,nrow(df))

# computing rom
add_rom <- function() {
  rom <- df$rom
  broken_ids = list()
  for (firm in unique(df$id)) {
    years = unique(df[df$id == firm,]$year)
    l = length(years)
    firm_data = df[df$id ==  firm,]
    # print(firm_data)
    for (y in 2:l) { #sanity check 4 consecutive years?
      t = years[y]
      t_1 = years[y-1]
      if (t - t_1 > 1) {
        print(str_glue("MISSING OBSERVATION AT id = {firm}, year = {t}"))
        append(broken_ids, firm)
      }
      replacement = firm_data[firm_data$year == t,]$mv - firm_data[firm_data$year == t_1,]$mv
      rom <- replace(rom, df$id == firm & df$year == t, replacement )
    }
  }
  return(rom)
}

df$rom <- add_rom()



#computing standard deviation on 3 years

sd3years_rom <- function() {
  sd_3 <- rep(0,nrow(df))
  for (firm in unique(df$id)) {
    years = unique(df[df$id == firm,]$year)
    l = length(years)
    
    firm_data = df[df$id ==  firm,]
    # print(firm_data)
    for (y in 3:l) {
      t = years[y]
      t_3 = years[y-2]
      focus = subset(firm_data, (year <= t & year >= t_3))$rom
        
      replacement = sd(focus)
      sd_3 <- replace(sd_3, df$id == firm & df$year == t, replacement)
    }
  }
  return(sd_3)
}

sd3years_mv <- function() {
  sd_3 <- rep(0,nrow(df))
  for (firm in unique(df$id)) {
    years = unique(df[df$id == firm,]$year)
    l = length(years)
    
    firm_data = df[df$id ==  firm,]
    # print(firm_data)
    for (y in 3:l) {
      t = years[y]
      t_3 = years[y-2]
      focus = subset(firm_data, (year <= t & year >= t_3))$mv
      
      replacement = sd(focus)
      sd_3 <- replace(sd_3, df$id == firm & df$year == t, replacement)
    }
  }
  return(sd_3)
}

df$sdev_rom <- sd3years_rom()
df$sdev_mv <- sd3years_mv()

#removing insufficient datapoints
df <- subset(df, sdev_rom != 0)

############################### Export ###################################

write_csv(df, "C:\\Users\\carlo\\Desktop\\MARKETING\\SCRIPTS\\data_clean.csv")

############################## REGRESSION ################################à

form <- sales ~ rd + ad + rd*ad + hhi + log(assets)

wi <- plm(form, df, model = "within")
re <- plm(form, df, model = "random")

#hausman test
phtest(wi, re)
summary(re)




######################## Lagged Terms ##############################

df2$lagged_rd = rep(0,nrow(df2))
df2$lagged_rd[2:nrow(df2)] = df2$rd[1:(nrow(df2)-1)]

df2$lagged_ad = rep(0,nrow(df2))
df2$lagged_ad[2:nrow(df2)] = df2$ad[1:(nrow(df2)-1)]

df2 = subset(df2, lagged_rd != 0)

###################### Correlation MAtrix ########################

regvars = c("sales","hhi", "roa", "lagged_rd", "lagged_ad", "threat", "log_assets")
regdata = subset(df2, select = regvars)
corrs = cor(regdata)[,c(7:1)]

corrs[upper.tri(corrs)] = NA

fig_m = plot_ly(
  x = regvars,
  y = regvars[7:1],
  z = corrs,
  type = "heatmap",
  colors = "Reds",
  colorbar = list(
    len = 1
  )
) %>%
  layout(
    yaxis = list(
      showgrid = FALSE
    ),
    xaxis = list(
      showgrid = FALSE
    )
  )
fig_m

################### Dummies ####################


################### Visuals #######################

vline <- function(x = 0, color = "red") {
  list(
    type = "line",
    y0 = -0.1,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}

t <- list(
  family = "sans serif",
  size = 14,
  color = 'blue')

areacolor = 'rgba(153, 0, 0, 0.5)'
areacolor2 = "rgba(255,102,102, 0.5)"
barcolor = "rgb(153,0,0)"
barcolor2 = "rgb(255,102,102)"
areacolor3 = "rgba(255,204,153,0.5)"
barcolor3 = "rgb(255,204,153)"
####################### Observation loss ####################################

fig0 = plot_ly(
  x = range, 
  y = obs_list, 
  type = "bar",
  marker = list(color = barcolor)
  ) %>%
  layout(
    title = "", 
    shapes = vline(2.5, color = "green"))
fig0

######################### Market Size #################################

fig1 <-  plot_ly(
  x = sales_by_year$year,
  y = sales_by_year$tot_sales,
  type = 'bar',
  marker = list(color = barcolor)) %>%
  layout(title = '',
         plot_bgcolor='white', 
         xaxis = list( 
           zerolinecolor = 'white', 
           zerolinewidth = 1
           ), 
         yaxis = list( 
           zerolinecolor = 'white', 
           zerolinewidth = 2, 
           gridcolor = "rgb(192,192,192)")
  )
fig1

########################## Market Competition ############################

fig3 <-  plot_ly(
  x = hhi_year$year,
  y = hhi_year$hhi,
  type = 'bar',
  marker = list(color = barcolor)) %>%
  layout(title = '',
         plot_bgcolor='white', 
         xaxis = list( 
           title = "Year",
           showgrid = FALSE,
           zerolinecolor = "white"),
         yaxis = list( 
           title = "HH Index",
           gridcolor = 'rgb(192,192,192)',
           zerolinecolor = "white"))
fig3

##################### R&D vs Ads Spending ###############################

fig5 <- plot_ly(
  x = rd_ad_year$year,
  y = rd_ad_year$rd/rd_ad_year$tot_spending, 
  type = "bar",
  name = "%R&D",
  marker = list(
    color = barcolor
  ))
fig5 <- fig5 %>% 
  add_trace(
    y = rd_ad_year$ad/rd_ad_year$tot_spending, 
    name = "%Ad", 
    marker = list(
      color = barcolor2
    ))
fig5 <- fig5 %>% 
  layout(
    yaxis = list(
      title = "% Spending"),
    barmode = "group",
    xaxis = list(
      title = "Year"
    ))

fig5


#ad ratio (lagged),
#rd ratio (lagged),
#hhi,
#roa

########################## Extra ############################

fig6 <- plot_ly(
  x = rd_ad_year$year,
  y = rd_ad_year$rd_ratio*100,
  type = "scatter",
  mode = "lines",
  fill = "tozeroy",
  fillcolor = areacolor,
  line = list(
    width = 1,
    color = barcolor          
    )
) %>%
  layout(title = '',
         plot_bgcolor='white', 
         xaxis = list(
           showgrid = FALSE,
           zerolinecolor = 'white', 
           zerolinewidth = 1
         ), 
         yaxis = list( 
           zerolinecolor = 'white', 
           zerolinewidth = 2, 
           gridcolor = "rgb(192,192,192)")
  )

fig6

fig7 <- plot_ly(
  x = rd_ad_year$year,
  y = rd_ad_year$ad_ratio*100,
  type = "scatter",
  mode = "lines",
  fill = "tozeroy",
  fillcolor = areacolor,
  line = list(
    width = 1.5,
    color = barcolor)
) %>%
  layout(title = '',
         plot_bgcolor='white', 
         xaxis = list(
           showgrid = FALSE,
           zerolinecolor = 'white', 
           zerolinewidth = 1
         ), 
         yaxis = list( 
           zerolinecolor = 'white', 
           zerolinewidth = 2, 
           gridcolor = "rgb(192,192,192)")
  )

fig7 



#lineplot with:
  # rd spending
  # ad spending
  # other spending
  # total


fig8 <- plot_ly(
  x = rd_ad_year$year,
  y = rd_ad_year$ad_ratio*100,
  name = "Ads",
  type = "scatter",
  mode = "lines",
  fill = "tonexty",
  fillcolor = areacolor2,
  line = list(
    width = 1.5,
    color = barcolor2)
) %>%
  layout(title = '',
         plot_bgcolor='white', 
         xaxis = list(
           showgrid = FALSE,
           zerolinecolor = 'white', 
           zerolinewidth = 1,
           dtick = 5,
           title = "Year"
         ), 
         yaxis = list( 
           zerolinecolor = 'white', 
           zerolinewidth = 2, 
           gridcolor = "rgb(192,192,192)",
           ticksuffix = "%")
  )

fig8 <- fig8 %>%
  add_trace(
    x = rd_ad_year$year,
    y = rd_ad_year$rd_ratio*100,
    name = "R&D",
    type = "scatter",
    mode = "lines",
    fill = "tozeroy",
    fillcolor = areacolor,
    line = list(
      width = 1,
      color = barcolor     
    )
  ) %>%
  add_trace(
    x = rd_ad_year$year,
    y = rd_ad_year$mkt_ratio*100,
    name = "Other",
    type = "scatter",
    mode = "lines",
    fill = "tozeroy",
    fillcolor = areacolor3,
    line = list(
      width = 1,
      color = barcolor3
  ))

fig8

