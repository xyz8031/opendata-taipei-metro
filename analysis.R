library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
setwd('Documents/Github/taipei-metro/')

options(scipen = 999)
showtext::showtext_auto()
theme_set(theme_minimal(base_family = 'Raleway', base_size = 10))

data = fread('data/clean_data.csv')

station = read.csv('https://raw.githubusercontent.com/repeat/northern-taiwan-metro-stations/master/northern-taiwan.csv') %>% 
  dplyr::select(line_name, station_name_tw, lat, lon) %>% 
  dplyr::rename(line = line_name,
                station = station_name_tw)

cols <- c("文湖線" = '#c48c31', "淡水信義線" = '#e3002c', "松山新店線" = '#008659',
          "中和新蘆線" = '#f8b61c', "板南線" = '#0070bd', "環狀線" = '#ffdb00',
          "機場線" = '#8246AF')

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
  
ggplot(temp, aes(x = date, y = number, group = station, col = line)) + 
  geom_line(alpha = 0.5) + 
  facet_wrap(~group) + 
  scale_x_date(date_labels = "%m",
               date_breaks = '1 month') + 
  scale_color_manual(values = cols) + 
  theme(legend.position = 'None',
        panel.grid.minor = element_blank())

# ggsave('num_by_station_and_hour.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

# time series by date by line
temp %>% 
  dplyr::group_by(date, group, line) %>% 
  dplyr::summarise(number = sum(number) / 10000) %>% 
  ggplot() + 
  geom_line(aes(x = date, y = number, col = line)) + 
  scale_x_date(date_labels = "%m",
               date_breaks = '1 month') + 
  scale_color_manual(values = cols) + 
  facet_wrap(~group) + 
  labs(title = '台北捷運各線進出站人次',
       subtitle = '單位：萬人') + 
  theme(legend.position = 'None',
        panel.grid.minor = element_blank()) 

# ggsave('num_by_hour_and_line.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

# direction
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

ggplot(temp, aes(x = from, y = to, group = date)) +
  geom_point(col = 'lightgrey', alpha = 0.25) +
  stat_smooth(aes(group = station, col = line), method = 'lm', se = F, fullrange = T, alpha = 0.5) + 
  scale_color_manual(values = cols) +
  facet_wrap(~group, scales = 'free') + 
  theme(panel.grid.minor = element_blank(),
        legend.position = 'None')



data %>% 
  dplyr::group_by(from, to) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  dplyr::filter(to == '港墘' & number >= 5000) %>% 
  dplyr::mutate(from = forcats::fct_reorder(from, desc(number))) %>% 
  ggplot() + 
  geom_bar(aes(y = from, x = number), stat = 'identity') + 
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y  = element_blank())

data %>% dplyr::filter(to == '港墘') %>% 
  dplyr::mutate(weekend = chron::is.weekend(time),
                hour = lubridate::hour(time)) %>% 
  dplyr::group_by(weekend, hour) %>% 
  dplyr::summarise(number = mean(number)) %>% 
  ggplot() + 
  geom_line(aes(x = hour, y = number)) + 
  scale_x_continuous(breaks = seq(0, 23, 3),
                     minor_breaks = NULL) + 
  facet_wrap(~weekend, nrow = 2) +
  scale_color_brewer(palette = 'Set1')

data %>% 
  dplyr::mutate(hour = lubridate::hour(time)) %>% 
  dplyr::group_by(from, hour) %>% 
  dplyr::summarise(number = mean(number)) %>% 
  merge(station, by.x = 'from', by.y = 'station') %>% 
  ggplot(aes(x = hour, y = number, group = from, col = line)) + 
  geom_line(show.legend = F) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  facet_wrap(~line, scales = 'free_y') +
  scale_color_manual(values = cols) +
  theme(panel.grid.minor = element_blank())

data %>% 
  dplyr::mutate(hour = lubridate::hour(time)) %>% 
  dplyr::group_by(to, hour) %>% 
  dplyr::summarise(number = mean(number)) %>% 
  merge(station, by.x = 'to', by.y = 'station') %>% 
  ggplot(aes(x = hour, y = number, group = to, col = line)) + 
  geom_line(show.legend = F) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  facet_wrap(~line, scales = 'free_y') +
  scale_color_manual(values = cols) +
  theme(panel.grid.minor = element_blank())

# https://bookdown.org/tpemartin/108-1-ntpu-datavisualization/annotation-and-maps.html
# https://gist.motc.gov.tw/gist_api/swagger/ui/index#!/District/District_03002

