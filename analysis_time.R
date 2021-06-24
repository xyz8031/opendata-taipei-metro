
# 轉乘站 平日/假日
temp = data %>%
  pivot_longer(cols = c('from', 'to'),
               names_to = 'direction',
               values_to = 'station') %>% 
  dplyr::group_by(date, station, direction, type) %>% 
  dplyr::summarise(number = sum(number)) 

temp$station = fct_reorder(temp$station, temp$number, .fun = max)

temp %>% 
  dplyr::filter(station %in% transfer$station) %>% 
  ggplot() +
  geom_boxplot(aes(x = station, y = number, color = type), position = 'dodge') + 
  facet_wrap(~direction, ncol = 1) + 
  theme(panel.grid.minor = element_blank())