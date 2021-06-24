
# treemap
temp = data %>%
  dplyr::group_by(from, to) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  pivot_longer(cols = c('from', 'to'),
               names_to = 'direction',
               values_to = 'station') %>% 
  dplyr::group_by(station) %>% 
  dplyr::summarise(number = sum(number)) 

temp = merge(temp, station, by = 'station')

p <- treemap(temp,
             index=c("district","station"),
             vSize="number",
             type="index",
             palette = "Set2",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             )  
)            

inter <- d3tree2( p ,  rootname = "General" )