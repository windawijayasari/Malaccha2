plot_reclassified_table = function(reclassified_table){
  #Plot map after reclassification
  plot_reclassified = ggplot(reclassified_table%>% filter(Year%in% c(2001, 2020) ), aes(X, Y, fill = LC_class)) +
    geom_raster() +
    scale_fill_manual(values = c("#009e73", "#f0e442", "#d55e00")) +
    geom_polygon(data = bounding_shapefile, aes(x = long, y = lat, group = group), 
                 col = "black", fill = NA, size = 1) +
    facet_wrap(~Year) +
    coord_fixed() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          
          legend.position = "bottom")
  
  plot(plot_reclassified)
  
  png(file="../Results_example/Plot Modis 2001 and 2020.png")
  plot(plot_reclassified)
  dev.off()
  
  pdf(file="../Results_example/Plot Modis 2001 and 2020.pdf")
  plot(plot_reclassified)
  dev.off()
  
  plot_reclassified
}

