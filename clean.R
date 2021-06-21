library(dplyr)
library(data.table)
library(fasttime)
library(lubridate)
setwd('Documents/Github/opendata-taipei-metro/')

holiday = read.csv('data/2017_holiday.csv', encoding = 'UTF-8')
colnames(holiday) = c('date', 'weekday', 'type', 'note')

holiday$date = as.Date(as.character(holiday$date), format = '%Y%m%d')
holiday$type = ifelse(holiday$type == 0, 'On', 'Off')

holiday = holiday %>% dplyr::select(-weekday)

files = list.files('data/raw/', full.names = T)
data = rbindlist(lapply(files,fread, skip = 2, header = F, sep = ' ', fill = T)) 

colnames(data) = c('date','hour','from','to','number')

data$number = as.numeric(data$number)

data$time = paste(data$date, ' ',data$hour, ':00', sep = '')
data$time = fastPOSIXct(data$time) - 60 * 60 * 8

data$date = as.Date(data$date)
data$weekday = weekdays(data$date, abbreviate = T)
data$hour = as.numeric(data$hour)

data = merge(data, holiday, by = 'date')

data$weekday = factor(data$weekday, levels = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun'))
data$type = factor(data$type, levels = c('On','Off'))

data = data %>% 
  dplyr::select(time, date, hour, weekday, type, note, from, to, number)

# saveRDS(data, "clean_data.rds")



t = paste0(temp$date[1:100], ' ', temp$hour[1:100], ':00')

result = microbenchmark(
  'as.POSIXct' = as.POSIXct(t),
  'lubridate' = ymd_hm(t),
  'fasttime' = fastPOSIXct(t)
)

autoplot(result)

ggsave('time_object_speed.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)


