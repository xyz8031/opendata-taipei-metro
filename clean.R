library(dplyr)
library(data.table)
library(fasttime)
library(lubridate)
setwd('Documents/Github/taipei-metro/')

holiday = read.csv('data/2017_holiday.csv', encoding = 'UTF-8')

files = list.files('data/raw/', full.names = T)
data = rbindlist(lapply(files,fread, skip = 2, header = F, sep = ' ', fill = T)) 

colnames(data) = c('date','hour','from','to','number')


data$time = paste(data$date, ' ',data$hour, ':00', sep = '')

# data$time = fastPOSIXct(data$time) - 60 * 60 * 8
# data$date = as.Date(data$date)

data$number = as.numeric(data$number)

data = data %>% 
  dplyr::mutate(weekday = weekdays(date, abbreviate = T),
                hour = hour(date)) %>% 
  dplyr::select(time, date, hour, from, to, number)

# fwrite(data, 'clean_data.csv', row.names = F)


t = paste0(temp$date[1:100], ' ', temp$hour[1:100], ':00')

result = microbenchmark(
  'as.POSIXct' = as.POSIXct(t),
  'lubridate' = ymd_hm(t),
  'fasttime' = fastPOSIXct(t)
)

autoplot(result)

ggsave('time_object_speed.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)


