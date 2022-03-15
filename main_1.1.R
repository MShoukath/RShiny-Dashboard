library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(highcharter) 


# setwd("/srv/shiny-server/nnatool")
# install.packages("dplyr", type = "binary")
# install.packages("tidyr", type = "binary")
# install.packages("stringr", type = "binary")
# install.packages("lubridate", type = "binary")
# install.packages("shiny", type = "binary")
# install.packages("shinythemes", type = "binary")
# install.packages("tippy", type = "binary")


df <- read.csv("test_1.1.csv", stringsAsFactors = FALSE)

#for start and end dates 
start <- min(as.Date(df$event_date, format = "%d-%m-%Y"))
end <- max(as.Date(df$event_date, format = "%d-%m-%Y"))



###########################################################
# Set highcharter options
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

df1 <- data.frame(
  x = c(0, 1, 2, 3, 4),
  y = c(93.06, 44.04, 30.34, 62.81, 30.91),
  name = as.factor(c("RP", "QP", "SS", "SC", "Activations"))
) %>%
  arrange(-y)
df1

hc <- df1 %>%
  hchart(
    "funnel", hcaes(x = name, y = y, colorize(x, "#f02d3a")),
    name = "Funnel Chart"
  )
hc

###########################################################

#Uncomment ****** lines for data with all values ****** 
filter_data <- function(df, start_date, end_date, country, pagenames, device_type, cookies, funnel_type){
  
  #filter data for different values
  df$event_date <- as.Date(df$event_date, format = "%d-%m-%Y")
  # print(class(df$event_date))
  df <- df %>% filter(event_date >= start_date, event_date <= end_date)
  df <-  df %>% filter(country_name == country)
  df <-  df %>% filter(grouped_page_name == pagenames)
  df <-  df %>% filter(Device == device_type)
  df <-  df %>% filter(cookies == cookies)
  
  if(funnel_type=='overall'){
    filtered_df <- df %>% filter(!grepl('Consumer|Merchant', KPI))
  } else if(funnel_type=='merchant'){
    filtered_df <- df %>% filter(grepl('Merchant|Visitors|Raw Prospects|QLFD Prospects|Impressions', KPI))
  } else if(funnel_type =='consumer'){
    filtered_df <- df %>% filter(grepl('Consumer|Visitors|Raw Prospects|QLFD Prospects|Clicks', KPI))
  }
  return (filtered_df)
}

# filter <- filter_data(df, start, end, 'FR', '#', 'smartphone', 1, 'overall')

pivot_data_overall <- function(filtered_df){
  
  pivoted_df <- data.frame(t(filtered_df %>%
                               group_by(KPI) %>%
                               summarize(Value_sum = sum(values))), stringsAsFactors = FALSE)
  rownames(pivoted_df) <- NULL
  colnames(pivoted_df) <- NULL
  
  names(pivoted_df) <- as.matrix(pivoted_df[1,])
  pivoted_df <- pivoted_df[-1, ]
  pivoted_df[] <- lapply(pivoted_df, as.integer)
  
  #for data without Imps & Clicks
  pivoted_df <- pivoted_df[c('# Visitors', '# Raw Prospects', '# QLFD Prospects', '# Signup Start', '# Signup Complete', '# Activations')]
  colnames(pivoted_df) <- c('Visitors', 'RP', 'QP', 'SS', 'SC', 'Activations')
  
  # ************for data with Imps & Clicks*********************
  # pivoted_df <- pivoted_df[c('# Impressions', '# Clicks', '# Visitors', '# Raw Prospects', '# QLFD Prospects', '# Signup Start', '# Signup Complete', '# Activations')]
  # colnames(pivoted_df) <- c('Impressions', 'Clicks', 'Visitors', 'RP', 'QP', 'SS', 'SC', 'Activations')
  
  return (pivoted_df)
}

# pvt<-pivot_data_overall(filter)

pivot_data_merchant <- function(filtered_df){
  
  pivoted_df <- data.frame(t(filtered_df %>%
                               group_by(KPI) %>%
                               summarize(Value_sum = sum(values))), stringsAsFactors = FALSE)
  rownames(pivoted_df) <- NULL
  colnames(pivoted_df) <- NULL
  
  names(pivoted_df) <- as.matrix(pivoted_df[1,])
  pivoted_df <- pivoted_df[-1, ]
  pivoted_df[] <- lapply(pivoted_df, as.integer)
  
  pivoted_df <- pivoted_df[c('# Visitors', '# Raw Prospects', '# QLFD Prospects', '# Merchant Signup Start', '# Merchant Signup Complete', '# Merchant Activations')]
  colnames(pivoted_df) <- c('Visitors', 'RP', 'QP', 'SS', 'SC', 'Activations')
  
  # ************for data with Imps & Clicks & Signup Starts *********************
  # pivoted_df <- pivoted_df[c('# Impressions', '# Clicks', '# Visitors', '# Raw Prospects', '# QLFD Prospects', '# Merchant Signup Start', '# Merchant Signup Complete', '# Merchant Activations')]
  # colnames(pivoted_df) <- c('Impressions', 'Clicks', 'Visitors', 'RP', 'QP', 'SS', 'SC', 'Activations')
  
  return (pivoted_df)
}

# pvt<-pivot_data_merchant(filter)

pivot_data_consumer <- function(filtered_df){
  pivoted_df <- data.frame(t(filtered_df %>%
                               group_by(KPI) %>%
                               summarize(Value_sum = sum(values))), stringsAsFactors = FALSE)
  rownames(pivoted_df) <- NULL
  colnames(pivoted_df) <- NULL
  
  names(pivoted_df) <- as.matrix(pivoted_df[1,])
  pivoted_df <- pivoted_df[-1, ]
  pivoted_df[] <- lapply(pivoted_df, as.integer)
  
  pivoted_df <- pivoted_df[c('# Visitors', '# Raw Prospects', '# QLFD Prospects', '# Consumer Signup Start', '# Consumer Signup Complete', '# Consumer Activations')]
  colnames(pivoted_df) <- c('Visitors', 'RP', 'QP', 'SS', 'SC', 'Activations')
  
  # ************for data with Imps & Clicks & Signup Starts *********************
  # pivoted_df <- pivoted_df[c('# Impressions', '# Clicks', '# Visitors', '# Raw Prospects', '# QLFD Prospects', '# Consumer Signup Start', '# Consumer Signup Complete', '# Consumer Activations')]
  # colnames(pivoted_df) <- c('Impressions', 'Clicks', 'Visitors', 'RP', 'QP', 'SS', 'SC', 'Activations')
  
  return (pivoted_df)
}

# pvt<-pivot_data_consumer(filter)

conversions_data <- function(pivoted_df){
  c <- pivoted_df
  
  #******
  # c$CTR <- round(c$Clicks*100/c$Impressions, 2)
  
  c$RawPros <- round(c$RP*100/c$Visitors, 2)
  c$QP <- round(c$QP*100/c$RP, 2)
  c$SS <-round(c$SS*100/c$RP, 2)
  c$SC <- round(c$SC*100/c$RP, 2)
  c$Activations <- round(c$Activations*100/c$RP, 2)
  
  #delete columns
  
  #*****
  # c$Impressions<-NULL
  # c$Clicks<-NULL
  c$RP <- NULL
  c$Visitors<-NULL
  
  #order cols
  c <- c %>% select(RawPros, everything())
  #****
  # c<-c%>% select(CTR, everything())
  
  conversion_df <- c
  
  return (conversion_df)
}

# conv <- conversions_data(pvt)

funnel_data <- function(pivoted_df){
  
  f <- pivoted_df
  #*****
  # f$Impressions<-NULL
  # f$Clicks<-NULL
  
  a <- f$RP
  b <- f$QP
  c <- f$SS
  d <- f$SC
  e <- f$Activations
  
  f$RP<-round(f$RP*100/f$Visitors, 2)
  f$QP<-round(f$QP*100/a, 2)
  f$SS<-round(f$SS*100/b, 2)
  f$SC<-round(f$SC*100/c, 2)
  f$Activations<-round(f$Activations*100/d, 2)
  
  funnel_df <- f
  return(funnel_df)
}

# funnel <- funnel_data(pvt)

sample_size <- function(alpha, beta, mde, baseline_val, relative=TRUE){
  # https://stats.stackexchange.com/questions/392979/ab-test-sample-size-calculation-by-hand
  
  conf <- alpha/2 # for two-tailed test, we need to split alpha by 2, for either side
  
  z_a <- qnorm(conf) # z-score of alpha/2
  z_b <- qnorm(beta) # z-score of beta
  p1 <- baseline_val
  if (relative==TRUE){
    p2 = p1 + p1 * mde
  }else{
    p2 = p1 + mde
  }
  num <- z_a * sqrt(2*p1*(1-p1)) + z_b * sqrt(p1*(1-p1) + p2*(1-p2))
  num <- num ^ 2
  den <- (p2-p1) ^ 2
  
  return (as.integer(num/den))
}

test_duration <- function(sample_size, df, col, no_of_days){
  
  total = col
  avg = total/no_of_days
  
  return (ceiling(as.integer(sample_size*2/avg)/70))
}

sizing_data<-function(mde, conversion_df, pivoted_df){
  
  pivot_df <- pivoted_df 
  
  baseline_val <- conversion_df$Activations 
  
  a<-conversion_df$SS 
  b<-conversion_df$SC 
  c<-round(pivoted_df$Activations * 100/pivoted_df$QP ,2)
  d<-round(pivoted_df$SC * 100/pivoted_df$QP,2)
  
  #SS/Rps <- conversion_df$SS /100
  
  if(baseline_val >= 1)
    baseline_val = 0.94
  
  # baseline_val <- 0.03
  
  ss1= sample_size(alpha=0.05, beta=0.2, mde=mde, baseline_val=(baseline_val/100))
  duration1 = test_duration(ss1, pivot_df, pivot_df$Activations, 14)
  
  ss2= sample_size(alpha=0.05, beta=0.2, mde=mde, baseline_val=(a/100))
  duration2 = test_duration(ss2, pivot_df, pivot_df$SS, 14)
  # print(as.integer(duration2))
  
  ss3= sample_size(alpha=0.05, beta=0.2, mde=mde, baseline_val=(b/100))
  duration3 = test_duration(ss3, pivot_df, pivot_df$SC, 14)
  
  ss4= sample_size(alpha=0.05, beta=0.2, mde=mde, baseline_val=(c/100))
  duration4 = test_duration(ss4, pivot_df, pivot_df$Activations, 14)
  
  ss5= sample_size(alpha=0.05, beta=0.2, mde=mde, baseline_val=(d/100))
  duration5 = test_duration(ss5, pivot_df, pivot_df$SC, 14)
  
  
  conversion_values<-matrix(c(conversion_df$Activations, a, b, c, d), ncol=1)
  sizing_df <- data.frame(conversion_values)
  sizing_df$MSS<-c(ss1,ss2,ss3,ss4,ss5)
  sizing_df$Number_days<-c(duration1, duration1, duration3, duration4, duration5)
  sizing_df <- round(sizing_df, 2)
  sizing_df$conversion_matrices<-c("7d/RP","SS/RP","SC/RP","7d/QP","SC/QP")
  sizing_df<- sizing_df %>% select(conversion_matrices, everything())
  
  #sizing_df <- data.frame(ss2, as.integer(duration2))
  colnames(sizing_df) <- c('Metrics', 'Conversions (in %)','Sample Size','Days Required')
  return(sizing_df)
}

# sizing_data(0.3, conv, pvt)

get_nna <- function(end_date, ramp_date, conversion_df, lift,pivoted){
  df <- conversion_df
  num_days <- 14   #Biweekly basis
  
  ramp_date<-dmy(ramp_date)
  
  ramp_year <- as.POSIXct(ramp_date, format = "%Y-%m-%d")
  a <- format(ramp_date, format="%Y")
  
  eoy1 <- paste(a,"-12-31")
  eoy <- str_replace_all(eoy1," ","")
  eoy <- as.Date(eoy)
  
  (ramped_days<-as.numeric(eoy-ramp_date))
  print(df)
  daily_rp <- (df$RawPros/num_days)
  
  # Taking 7d activation as conversion criteria Dividing by 100 cause values are in %
  base1 <- (df$Activations)/100
  
  #Checking for daily activations by taking daily RP numbers
  daily_activations1 <- ceiling(daily_rp * lift * base1)
  
  #Checking activation for whole year by multiplying daily activation with 365
  yearly_activations1 <- ceiling(daily_activations1 * 365)
  
  #Checking activation for remaining year after ramp date
  ye_activations1 <- ceiling(yearly_activations1 * ramped_days /365) 
  
  
  base2<-(df$SS)/100
  daily_activations2 <- ceiling(daily_rp * lift * base2)
  yearly_activations2 <- ceiling(daily_activations2 * 365)
  ye_activations2 <- ceiling(yearly_activations2 * ramped_days /365) 
  
  base3<-(df$SC)/100
  daily_activations3 <- ceiling(daily_rp * lift * base3)
  yearly_activations3 <- ceiling(daily_activations3 * 365)
  ye_activations3 <- ceiling(yearly_activations3 * ramped_days /365) 
  
  #c<-round(pivoted_df$Activations * 100/pivoted_df$QP ,2)
  #d<-round(pivoted_df$SC * 100/pivoted_df$QP,2)
  
  base4<-pivoted$Activations/pivoted$QP
  daily_activations4 <- ceiling(daily_rp * lift * base4)
  yearly_activations4 <- ceiling(daily_activations4 * 365)
  ye_activations4 <- ceiling(yearly_activations4 * ramped_days /365)
  
  base5<-pivoted$SC/pivoted$QP
  daily_activations5 <- ceiling(daily_rp * lift * base5)
  yearly_activations5 <- ceiling(daily_activations5 * 365)
  ye_activations5 <- ceiling(yearly_activations5 * ramped_days /365)
  
  #daily_activations, yearly_activations, ye_activations
  m <- matrix(c(daily_activations1, daily_activations2,daily_activations3,daily_activations4,daily_activations5) ,ncol = 1)
  activations_df <- as.data.frame(m)
  colnames(activations_df) <- ('daily_act')
  activations_df$year <- c(yearly_activations1,yearly_activations2,yearly_activations3,yearly_activations4,yearly_activations5)
  activations_df$year_end <- c(ye_activations1,ye_activations2,ye_activations3,ye_activations4,ye_activations5)
  activations_df$metrics<-c("7d/RP","SS/RP","SC/RP","7d/QP","SC/QP")
  activations_df<- activations_df %>% select(metrics, everything())
  colnames(activations_df) <- c('Metrics', 'Activations(Daily)', 'Activations(Yearly)', 'Activations(YearEnd)')
  
  return (activations_df)
}


# get_nna(end, "28/06/2021", conv, 1,pvt)

get_overall_funnel <- function(df, start_date, end_date, mde, lift, ramp_date, country, pagenames, device_type, cookies){
  print(start_date)
  print(end_date)
  filter <- filter_data(df, start_date, end_date, country, pagenames, device_type, cookies, 'overall')
  pivoted <- pivot_data_overall(filter)
  
  conversions <- (conversions_data(pivoted))
  funnel <- funnel_data(pivoted)
  sizing <- sizing_data(mde, conversions, pivoted)
  nna <- get_nna(end_date, ramp_date, conversions, lift,pivoted)
  
  # colnames(conversions) <- c("CTR (in %)", "Raw Prospects (in %)","Qualified Prospects (in %)", "Signup Starts (in %)", "Signup Completes (in %)", "Activations (in %)")
  colnames(conversions) <- c("Raw Prospects (in %)","Qualified Prospects (in %)", "Signup Starts (in %)", "Signup Completes (in %)", "Activations (in %)")
  conversions <- round(conversions, 2)
  colnames(funnel) <- c("Total Visitors", "Raw Prospects (in %)","Qualified Prospects (in %)", "Signup Starts (in %)", "Signup Completes (in %)", "Activations (in %)")
  funnel <- round(funnel, 2)
  
  l <- list(conversions, funnel, sizing, nna)
  return (l)
  
}

# get_overall_funnel(df, start, end, 0.03, 1, "15/02/2021", 'us', '#', 'smartphone', 1)

get_merchant_funnel <- function(df, start_date, end_date, mde, lift, ramp_date, country, pagenames, device_type, cookies){
  
  filter <- filter_data(df, start_date, end_date, country, pagenames, device_type, cookies, 'merchant')
  pivoted <- pivot_data_merchant(filter)
  
  conversions <- (conversions_data(pivoted))
  funnel <- funnel_data(pivoted)
  sizing <- sizing_data(mde, conversions, pivoted)
  
  nna <- get_nna(end_date, ramp_date, conversions, lift,pivoted)  
  # colnames(conversions) <- c("CTR (in %)", "Raw Prospects (in %)","Qualified Prospects (in %)", "Signup Starts (in %)", "Signup Completes (in %)", "Activations (in %)")
  colnames(conversions) <- c("Raw Prospects (in %)","Qualified Prospects (in %)", "Signup Starts (in %)", "Signup Completes (in %)", "Activations (in %)")
  conversions <- round(conversions, 2)
  colnames(funnel) <- c("Total Visitors", "Raw Prospects (in %)","Qualified Prospects (in %)", "Signup Starts (in %)", "Signup Completes (in %)", "Activations (in %)")
  funnel <- round(funnel, 2)
  
  l <- list(conversions, funnel, sizing, nna)
  return (l)
  
}

get_consumer_funnel <- function(df, start_date, end_date, mde, lift, ramp_date, country, pagenames, device_type, cookies){
  
  filter <- filter_data(df, start_date, end_date, country, pagenames, device_type, cookies, 'consumer')
  pivoted <- pivot_data_consumer(filter)
  
  conversions <- (conversions_data(pivoted))
  funnel <- funnel_data(pivoted)
  sizing <- sizing_data(mde, conversions, pivoted)
  nna <- get_nna(end_date, ramp_date, conversions, lift,pivoted)  
  # colnames(conversions) <- c("CTR (in %)", "Raw Prospects (in %)","Qualified Prospects (in %)", "Signup Starts (in %)", "Signup Completes (in %)", "Activations (in %)")
  colnames(conversions) <- c("Raw Prospects (in %)","Qualified Prospects (in %)", "Signup Starts (in %)", "Signup Completes (in %)", "Activations (in %)")
  conversions <- round(conversions, 2)
  colnames(funnel) <- c("Total Visitors", "Raw Prospects (in %)","Qualified Prospects (in %)", "Signup Starts (in %)", "Signup Completes (in %)", "Activations (in %)")
  funnel <- round(funnel, 2)
  
  l <- list(conversions, funnel, sizing, nna)
  return (l)
  
}














