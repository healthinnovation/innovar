
#data: datos (en sf con crs: EPSG: 4326)
# lon y lat: coordenadas para los buffers (en crs: EPSG: 4326)

mmap2.ggmap <-function(data,lon,lat) {
  
  
  # basemap
  
  mapa<-get_map(location = st_bbox(data) %>% as.numeric(),
                
                source = "osm",
                
                maptype = "watercolor",zoom = 12)
  
  
  # Defincion de puntos para el buffer
  df2<-data.frame(lat,lon)
  df2 <- st_as_sf(df2, coords = c("lon","lat"), crs = "EPSG: 4326")
  label<-c("C")
  
  buffer1<- st_buffer(df2, 0.03)
  buffer2<- st_buffer(df2, 0.1)
  buffer3<- st_buffer(df2, 0.2)
  
  
  # Mapa con ggmap 
  
  ggmap(mapa) +
    
    
    # Buffers (centro: gamitanacocha)  
    geom_sf(data = buffer3, color = "slateblue4", linetype = "dashed", size = 1,  alpha = 0, inherit.aes = F) + 
    
    
    geom_sf(data = buffer2, color = "blue4", size = 1, linetype = "dashed",  alpha = 0 , inherit.aes = F) + 
    
    geom_sf(data = buffer1, color = "red4", size = 1, linetype = "dashed",  alpha = 0, inherit.aes = F) + 
    
    
    # Puntos de gps
    geom_sf(data = data, aes(fill=behav), pch=21, alpha=0.7,size=2,inherit.aes = F) +
    
    # Marca de incio por id
    geom_sf(data = data %>%
              group_by(id) %>%
              slice(which(row_number() == 1)) %>%
              ungroup(), color = "green", pch = 21, size = 3, stroke = 1.25, inherit.aes = F) +
    
    # Marca de fin por id
    geom_sf(data = data %>%
              group_by(id) %>%
              slice(which(row_number() == n())) %>%
              ungroup(), color = "red", pch = 24, size = 3, stroke = 1.25,  inherit.aes = F) +
    
    
    # Etiquetas del centro del buffer
    geom_text(data = df2, aes(x = lon, y = lat , label = label, fontface = "bold"), colour = "Black") +
    
    
    
    #scale_fill_manual(values = c("#DCE319FF","#404788FF"), na.value = "#3CBB75FF", label = c("Community", "Displacement", "Unclassified")) +
    labs(x = "", y = "", fill = "") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 16),
      axis.text.x  = element_text(size = 7),
      axis.text.y  = element_text(size = 7),
      strip.text = element_text(size = 14, face = "bold"),
      panel.grid = element_blank()) +
    guides(fill = guide_legend(label.theme = element_text(size = 10),
                               title.theme = element_text(size = 14))) 
  
}


  








  
