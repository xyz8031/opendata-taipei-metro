library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(forcats)
library(lubridate)
library(treemap)
library(d3treeR)
setwd('~/Documents/Github/opendata-taipei-metro/')

options(scipen = 999)
showtext::showtext_auto()
theme_set(theme_minimal(base_family = 'Raleway', base_size = 10))

data = readRDS('data/clean_data.rds')

station = read.csv('https://raw.githubusercontent.com/repeat/northern-taiwan-metro-stations/master/northern-taiwan.csv') %>% 
  dplyr::rename(line = line_name,
                station = station_name_tw) %>% 
  dplyr::mutate(district = str_sub(address, 4, 6)) %>% 
  dplyr::select(line, station, district) %>% 
  dplyr::filter(line != '環狀線' & line != '機場線') %>% 
  distinct()

transfer = station %>% 
  dplyr::group_by(station) %>% 
  count() %>% 
  dplyr::filter(n > 1) 

station$line = ifelse(station$station %in% transfer$station, '轉乘站', station$line)

cols <- c("文湖線" = '#c48c31', "淡水信義線" = '#e3002c', "松山新店線" = '#008659',
          "中和新蘆線" = '#f8b61c', "板南線" = '#0070bd', '轉乘站' = 'black')

# ggplot(station) + 
#   geom_point(aes(x = lon, y = lat, col = line)) + 
#   scale_color_manual(values = cols) + 
#   theme(legend.position = 'None',
#         panel.grid.minor = element_blank())

# time series by date by station
temp = data %>% 
  pivot_longer(cols = c('from', 'to'),
               names_to = 'direction',
               values_to = 'station') %>% 
  dplyr::group_by(date, station) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  merge(station, by = 'station') %>% 
  dplyr::mutate(date = as.Date(date)) %>% 
  dplyr::filter(date < as.Date('2018-01-01'))

highlight = temp %>% 
  dplyr::group_by(line, station) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  dplyr::group_by(line) %>% 
  dplyr::arrange(desc(number)) %>% 
  dplyr::filter(row_number() == max(row_number()) | row_number() == min(row_number()))
  
ggplot() + 
  geom_line(data = temp %>% dplyr::filter(station %in% highlight$station), aes(x = date, y = number / 10000, group = station, col = line), lwd = 0.5) + 
  geom_line(data = temp %>% dplyr::filter(!(station %in% highlight$station)), aes(x = date, y = number / 10000, group = station), col = 'lightgrey', alpha = 0.25, lwd = 0.5) + 
  facet_wrap(~line, scales = 'free_y') + 
  labs(title = '台北捷運各站進出站人次') + 
  scale_x_date(name = '',
               date_labels = "%b",
               date_breaks = '2 month') + 
  ylab('人數（萬人）') + 
  scale_color_manual(values = cols) + 
  theme(legend.position = 'None',
        panel.grid.minor = element_blank())
# ggsave('num_by_station_and_hour.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

# 總運量排名
temp = data %>% 
  pivot_longer(cols = c('from', 'to'),
               names_to = 'direction',
               values_to = 'station') %>% 
  dplyr::group_by(station, direction) %>% 
  dplyr::summarise(number = sum(number) / 10000) %>% 
  merge(station %>% dplyr::select(-line) %>% distinct(), by = 'station')

temp$station = fct_reorder(temp$station, abs(temp$number))

ggplot() + 
  geom_point(data = temp %>% dplyr::filter(direction == 'from'), aes(x = number, y = station, col = direction), stat = 'identity', position = 'stack') + 
  geom_point(data = temp %>% dplyr::filter(direction == 'to'), aes(x = number, y = station, col = direction), stat = 'identity', position = 'stack') + 
  # geom_pointrange(aes(y = station, xmin = , xmax = )) + 
  scale_color_brewer(palette = 'Set1') + 
  facet_wrap(~district, scales = 'free') +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())



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
  dplyr::group_by(from, type) %>% 
  dplyr::summarise(number = sum(number) / 10000) %>% 
  # dplyr::filter(number >= 1) %>% 
  dplyr::arrange(desc(number)) %>% 
  dplyr::group_by(type) %>% 
  dplyr::filter(row_number() <= 10)

ggplot(temp, aes(y = reorder_within(from, number, type), x = number, fill = type)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.9) +
  facet_wrap(~type, scales = 'free_y') + 
  scale_x_continuous(name = '萬人',
                     breaks = c(seq(0, 10, 5), seq(20, 60, 10))) + 
  scale_y_reordered() + 
  scale_fill_brewer(palette = 'Set1') + 
  ylab('') + 
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'None')

ggsave('neihu_top_origin.png', width = 16, height = 9, dpi = 500, scale = 0.6)


temp = data %>% 
  dplyr::filter(to %in% c('西湖','港墘')) %>% 
  dplyr::group_by(date, hour, type) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  dplyr::group_by(hour, type) %>% 
  dplyr::summarise(avg = mean(number),
                   sd = sd(number)) %>% 
  dplyr::filter(hour >= 5)

ggplot(temp) +
  geom_line(aes(x = hour, y = avg, col = type), lwd = 1.1) +
  geom_ribbon(aes(x = hour, ymin = avg - 1.5 * sd, ymax = avg + 1.5 * sd, group = type, fill = type), alpha = 0.25) + 
  scale_color_brewer(palette = 'Set1') + 
  scale_x_continuous(breaks = c(6, 9, 12, 15, 18, 21, 23)) + 
  theme(panel.grid.minor.y = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal') 

ggsave('neihu_hourly_passenger.png', width = 16, height = 9, dpi = 500, scale = 0.6)


temp = data %>% 
  dplyr::filter(to %in% c('西湖','港墘')) %>% 
  dplyr::group_by(date, hour, weekday) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  dplyr::group_by(hour, weekday) %>% 
  dplyr::summarise(avg = mean(number),
                   sd = sd(number)) %>% 
  dplyr::filter(hour >= 5)

ggplot(temp) +
  geom_line(aes(x = hour, y = avg, col = weekday), lwd = 1.1) +
  # geom_ribbon(aes(x = hour, ymin = avg - 1.5 * sd, ymax = avg + 1.5 * sd, group = weekday, fill = weekday), alpha = 0.25) + 
  scale_color_brewer(palette = 'Set1') + 
  scale_x_continuous(breaks = c(6, 9, 12, 15, 18, 21, 23)) + 
  theme(panel.grid.minor.y = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal') 

# ggsave('neihu_hourly_passenger.png', width = 16, height = 9, dpi = 500, scale = 0.6)

# 信義區


# 東區


# 台北車站




# https://bookdown.org/tpemartin/108-1-ntpu-datavisualization/annotation-and-maps.html
# https://gist.motc.gov.tw/gist_api/swagger/ui/index#!/District/District_03002

