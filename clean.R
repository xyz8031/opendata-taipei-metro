library(dplyr)
library(data.table)
library(fasttime)
setwd('Documents/Github/taipei-metro/')

files = list.files('data/raw/', full.names = T)
data = rbindlist(lapply(files,fread, skip = 2, header = F, sep = ' ', fill = T)) 

colnames(data) = c('date','hour','from','to','number')


data$time = paste(data$date, ' ',data$hour, ':00', sep = '')

# data$time = fastPOSIXct(data$time) - 60 * 60 * 8
# data$date = as.Date(data$date)

data$number = as.numeric(data$number)

data = data %>% dplyr::select(time, date, from, to, number)

# fwrite(data, 'clean_data.csv', row.names = F)

