#install.packages("devtools")
library(devtools)

#install.packages('RCurl')
library(RCurl)

#install.packages('RGA')
library(RGA)

library(lattice)

# Open a authorize screen on a new window
authorize()

# Help you find which view to choose for config.R file
#ga_profiles <- list_profiles()

# Load config file
source('config.R')

filtering_device <- function(device='') {
  if (device != '') { 
    paste('ga:deviceCategory==',device, sep='') 
  }
}

first_step <- function(device='') {
  device <- filtering_device(device)
  return (
    get_ga(
      id, 
      start.date = start_date, 
      end.date = end_date,
      metrics = "ga:sessions, ga:bounceRate",
      filters = device
    )
  )
}

# get sessions that went to first checkout step based on URL.
start_CO <- function(device='') {
  device <- filtering_device(device)
  
  return (
    get_ga(id,
    start.date = start_date,
    end.date = end_date,
    metric="ga:sessions",
    sort="-ga:sessions",
    segment= paste("sessions::condition::ga:pagePath=@",first_step_CO, sep=""),
    filters = device
    )
  )
}

# get transactions
transac <- function(device='') {
  device <- filtering_device(device)
  
  return (
    get_ga(id,
      start.date = start_date,
      end.date = end_date,
      metric="ga:transactions",
      filters= device
    )
  )
}

dataset_global <- function(device=''){
  first_step <- first_step(device)
  start_CO <- start_CO(device)
  transac <- transac(device)
  
  total_sessions <- first_step$sessions
  BR <- round(first_step$bounceRate, 2)
  before_shopping_percent <- round(100 - first_step$bounceRate, 2)
  before_shopping <- total_sessions * (first_step$bounceRate/100)
  entry_CO <- start_CO$sessions
  entry_CO_percent <- round(entry_CO / total_sessions *100, 2)
  transactions <- transac$transactions
  transactions_percent <- round(transactions / total_sessions * 100, 2)
  
  s <- c("total", "Shopping", "Checkout", "Conversions")
  stays <- data.frame(status="stays", percent=c(100, before_shopping_percent, entry_CO_percent, transactions_percent))
  stays['steps'] <-  s

  lost <- data.frame(status="lost",percent=round(c(100,BR, 100-entry_CO / before_shopping *100, 100 - transactions / entry_CO *100), 2))
  lost['steps'] <- s
  
  df <- data.frame(status=stays, lost=lost)
  df <- rbind(lost, stays)
  df['device'] <- device
  df
}

dfg <- dataset_global()
df_desktop <- dataset_global('desktop')
df_mobile <- dataset_global('mobile')
df_tablet <- dataset_global('tablet')

ddd <- rbind(dfg, df_desktop, df_mobile, df_tablet)

barchart(percent ~ steps | device, groups=status, ddd, auto.key = list(columns = 2))