library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(forcats)
library(lubridate)
setwd('~/Documents/Github/opendata-taipei-metro/')

options(scipen = 999)
showtext::showtext_auto()
theme_set(theme_minimal(base_family = 'Raleway', base_size = 10))

data = fread('data/clean_data.csv')

station = read.csv('https://raw.githubusercontent.com/repeat/northern-taiwan-metro-stations/master/northern-taiwan.csv') %>% 
  dplyr::rename(line = line_name,
                station = station_name_tw) %>% 
  dplyr::mutate(district = str_sub(address, 4, 6)) %>% 
  dplyr::select(line, station, district, lat, lon) 

cols <- c("文湖線" = '#c48c31', "淡水信義線" = '#e3002c', "松山新店線" = '#008659',
          "中和新蘆線" = '#f8b61c', "板南線" = '#0070bd', "環狀線" = '#ffdb00',
          "機場線" = '#8246AF')

ggplot(station) + 
  geom_point(aes(x = lon, y = lat, col = line)) + 
  scale_color_manual(values = cols) + 
  theme(legend.position = 'None',
        panel.grid.minor = element_blank())

#
temp = data %>%
  pivot_longer(cols = c('from', 'to'),
               names_to = 'direction',
               values_to = 'station') %>% 
  dplyr::group_by(date, station) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  arrange(desc(number)) %>% 
  dplyr::filter(row_number() <= 30)

ggplot(temp)

# time series by date by station
temp = data %>% 
  pivot_longer(cols = c('from', 'to'),
               names_to = 'direction',
               values_to = 'station') %>% 
  dplyr::group_by(date, station) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  merge(station, by = 'station') %>% 
  dplyr::mutate(date = as.Date(date),
                group = ifelse(line %in% c('機場線','環狀線'), '機場線＆環狀線', line)) %>% 
  dplyr::filter(date < as.Date('2018-01-01'))
  
ggplot(temp, aes(x = date, y = number / 10000, group = station, col = line)) + 
  geom_line(alpha = 0.5) + 
  facet_wrap(~group) + 
  labs(title = '台北捷運各站進出站人次') + 
  scale_x_date(name = '',
               date_labels = "%b",
               date_breaks = '3 month',
               minor_breaks = '1 month') + 
  ylab('人數（萬人）') + 
  scale_color_manual(values = cols) + 
  theme(legend.position = 'None',
        panel.grid.minor.y = element_blank())

# ggsave('num_by_station_and_hour.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

# time series by date by line
temp %>% 
  dplyr::group_by(date, group, line) %>% 
  dplyr::summarise(number = sum(number) / 10000) %>% 
  ggplot() + 
  geom_line(aes(x = date, y = number, col = line)) + 
  scale_x_date(name = '',
               date_labels = "%b",
               date_breaks = '3 month',
               minor_breaks = '1 month') + 
  scale_color_manual(values = cols) + 
  ylab('人數（萬人）') + 
  facet_wrap(~group) + 
  labs(title = '台北捷運各線進出站人次') + 
  theme(legend.position = 'None',
        panel.grid.minor.y = element_blank()) 

# ggsave('num_by_hour_and_line.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

# outbound & inbound
temp = data %>% 
  pivot_longer(cols = c('from', 'to'),
               names_to = 'direction',
               values_to = 'station') %>% 
  dplyr::group_by(date, station, direction) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(date, station),
              names_from = direction,
              values_from = number,
              values_fill = 0) %>% 
  merge(station, by = 'station') %>% 
  dplyr::mutate(group = ifelse(line %in% c('機場線','環狀線'), '機場線＆環狀線', line)) 

ggplot(temp, aes(x = from / 10000, y = to / 10000, group = date)) +
  # geom_point(col = 'lightgrey', alpha = 0.1) +
  stat_smooth(geom = 'line', aes(group = station, col = line), method = 'lm', se = F, fullrange = T, alpha = 0.6) + 
  geom_abline(slope = 1, linetype = 'dashed') + 
  scale_color_manual(values = cols) +
  facet_wrap(~group, scales = 'free') + 
  theme(panel.grid.minor = element_blank(),
        legend.position = 'None') + 
  xlab('單日總進站人數（萬人）') + ylab('單日總出站人數（萬人）')

# ggsave('outbound_vs_inbound.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

# 內科
temp = data %>% 
  dplyr::filter(to %in% c('西湖','港墘')) %>% 
  dplyr::group_by(from) %>% 
  dplyr::summarise(number = sum(number) / 100000) %>% 
  # dplyr::filter(number >= 1) %>% 
  top_n(10)
temp$from = fct_reorder(temp$from, temp$number)

ggplot(temp) + 
  geom_bar(aes(y = from, x = number), stat = 'identity') + 
  scale_x_continuous(name = '十萬人',
                     breaks = seq(1, 6)) + 
  ylab('') + 
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y  = element_blank())

# 信義區


# 東區


# 台北車站




# https://bookdown.org/tpemartin/108-1-ntpu-datavisualization/annotation-and-maps.html
# https://gist.motc.gov.tw/gist_api/swagger/ui/index#!/District/District_03002

