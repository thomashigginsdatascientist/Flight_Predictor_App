



library(anyflights)
library(censusapi)
library(tidyverse)


airlines <- as.data.frame(anyflights::get_airlines())
airports <- as.data.frame(anyflights::get_airports())




options(timeout = 1000*1000*1000*1000)

getOption('timeout')

pit <- get_flights(station = "PIT", year = 2021, month = 1:10, dir = NULL)

saveRDS(roc, "ROC 2021 thruOct Flights.RDS")

buff <- get_flights(station = "BUF", year = 2021, month = 1:10, dir = NULL)

cleve <- get_flights(station = "CLE", year = 2021, month = 1:10, dir = NULL)

roc <- get_flights(station = "ROC", year = 2021, month = 1:10, dir = NULL)



get_apt_info(.icao)
