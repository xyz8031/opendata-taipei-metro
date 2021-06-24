# https://www.data-to-viz.com/graph/arc.html
# https://www.r-bloggers.com/2013/02/arc-diagrams-in-r-les-miserables/

temp = station %>% 
  dplyr::group_by(station) %>% 
  dplyr::summarise(n = n()) %>% 
  merge(station, by = 'station') %>% 
  dplyr::select(district, station, n) %>% 
  dplyr::rename(grp = district,
                name = station) %>% 
  dplyr::distinct() %>% 
  dplyr::select(name, n, grp) 

temp = temp %>% 
  arrange(grp, desc(n)) %>% 
  dplyr::mutate(name = factor(name))

connect = data %>%
  dplyr::filter(type == 'Off') %>% 
  dplyr::group_by(from, to) %>% 
  dplyr::summarise(value = mean(number)) 

connect$from = ifelse(connect$from == '大橋頭站', '大橋頭', connect$from)
connect$to = ifelse(connect$to == '大橋頭站', '大橋頭', connect$to)

mygraph <- graph_from_data_frame( connect, vertices = temp, directed = T)


# Make the graph
ggraph(mygraph, layout="linear") + 
  geom_edge_arc(edge_colour="black", edge_alpha=0.2, edge_width=0.3, fold=TRUE) +
  geom_node_point(aes(size=n, color=as.factor(grp), fill=grp), alpha=0.5) +
  scale_size_continuous(range=c(1,5)) +
  # scale_color_manual(values=mycolor) +
  geom_node_text(aes(label=name), angle=65, hjust=1, nudge_y = -1.1, size=2.3) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0.4,0), "null"),
    panel.spacing=unit(c(0,0,3.4,0), "null")
  ) +
  expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2)) 

