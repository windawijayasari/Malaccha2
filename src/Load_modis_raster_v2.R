Load_jcr_raster = function(jcr_database_path,
                           bounding_shapefile_path){
  
  # Load shapefile and its CRS (coordinate system)
  bounding_shapefile = vect(bounding_shapefile_path)
  plot(bounding_shapefile)
  bounding_shapefile_crs = crs(bounding_shapefile)
  
  # List jcr tif files
  jcr_files = list.files(jcr_database_path, pattern = "tif$", full.names = TRUE)
  plot(rast(jcr_files[1]))
  
  #Reproject the raster
  jcr_cropped_rasters = lapply(jcr_files, function(jcr_file){
    a = rast(jcr_file) 
    jcr_repojected = 
      terra::project(a, bounding_shapefile_crs, method = "bilinear")
    jcr_cropped = crop(jcr_repojected, bounding_shapefile, snap = "OUT")
    jcr_masked = mask(jcr_cropped, bounding_shapefile)
  }) %>%
    `names<-`(jcr_files)
  
  
  
  plot(jcr_cropped_rasters[[15]])
  plot(bounding_shapefile, add = TRUE)
  
  
  jcr_cropped_rasters
}
