# https://www.r-graph-gallery.com/309-intro-to-hierarchical-edge-bundling.html
# https://www.r-graph-gallery.com/310-custom-hierarchical-edge-bundling.html


d1 <- data.frame(from="origin", to = unique(station$district))
d2 <- station %>% 
  dplyr::select(district, station) %>% 
  distinct() %>% 
  dplyr::rename(from = district,
                to = station)

hierarchy <- rbind(d1, d2)

vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) ) 


mygraph <- graph_from_data_frame( hierarchy, vertices=vertices )

# With igraph: 
plot(mygraph, vertex.label="", edge.arrow.size=0, vertex.size=2)

# With ggraph:
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_link() +
  theme_void()

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() +
  theme_void()


connect = data %>% 
  dplyr::filter(type == 'On' & hour == 6) %>% 
  dplyr::group_by(from, to) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  dplyr::filter(number > 5000) %>% 
  dplyr::select(-number)
  
connect$from = ifelse(connect$from == '大橋頭站', '大橋頭', connect$from)
connect$to = ifelse(connect$to == '大橋頭站', '大橋頭', connect$to)

from <- match( connect$from, vertices$name)
to <- match( connect$to, vertices$name)


# plot
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", tension = 0.9) +
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()

  
