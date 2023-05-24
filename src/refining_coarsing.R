# using standard aggregation/disaggregation
modis_cropped_1000 = lapply(modis_cropped_rasters, function(rast){ terra::aggregate(rast, fact = 2, fun = "modal",  na.rm=TRUE) })
modis_cropped_2000 = lapply(modis_cropped_rasters, function(rast){ terra::aggregate(rast, fact = 4, fun = "modal") })
modis_cropped_5000 = lapply(modis_cropped_rasters, function(rast){ terra::aggregate(rast, fact = 10, fun = "modal") })

modis_cropped_250 = lapply(modis_cropped_rasters, function(rast){ terra::disagg(rast, fact = 2, method = "near") })
modis_cropped_125 = lapply(modis_cropped_rasters, function(rast){ terra::disagg(rast, fact = 4, method = "near") })


# checks
modis_cropped_1000 |> sapply(function(x){ (x |> as.data.frame() |> dim())[[1]] })
modis_cropped_2000 |> sapply(function(x){ (x |> as.data.frame() |> dim())[[1]] })
modis_cropped_5000 |> sapply(function(x){ (x |> as.data.frame() |> dim())[[1]] })
modis_cropped_250 |> sapply(function(x){ (x |> as.data.frame() |> dim())[[1]] })
modis_cropped_125 |> sapply(function(x){ (x |> as.data.frame() |> dim())[[1]] })


#using resample
r_1000 = terra::rast(extent = terra::ext(modis_cropped_rasters[[1]]), resolution = 1000)
r_2000 = terra::rast(extent = terra::ext(modis_cropped_rasters[[1]]), resolution = 2000)
r_5000 = terra::rast(extent = terra::ext(modis_cropped_rasters[[1]]), resolution = 5000)

r_500 = terra::rast(extent = terra::ext(modis_cropped_rasters[[1]]), resolution = 500)
r_250 = terra::rast(extent = terra::ext(modis_cropped_rasters[[1]]), resolution = 250)
r_125 = terra::rast(extent = terra::ext(modis_cropped_rasters[[1]]), resolution = 125)


modis_cropped_500 = lapply(modis_cropped_rasters, function(rast){ terra::resample(rast, r_500, method = "near")})

modis_cropped_1000 = lapply(modis_cropped_500, function(rast){ terra::resample(rast, r_1000, method = "mode")})
modis_cropped_2000 = lapply(modis_cropped_500, function(rast){ terra::resample(rast, r_2000, method = "mode")})
modis_cropped_5000 = lapply(modis_cropped_500, function(rast){ terra::resample(rast, r_5000, method = "mode")})
modis_cropped_250 = lapply(modis_cropped_500, function(rast){ terra::resample(rast, r_250, method = "near")})
modis_cropped_125 = lapply(modis_cropped_500, function(rast){ terra::resample(rast, r_125, method = "near")})

# checks
modis_cropped_1000 |> sapply(function(x){ (x |> as.data.frame() |> dim())[[1]] })
modis_cropped_2000 |> sapply(function(x){ (x |> as.data.frame() |> dim())[[1]] })
modis_cropped_5000 |> sapply(function(x){ (x |> as.data.frame() |> dim())[[1]] })
modis_cropped_250 |> sapply(function(x){ (x |> as.data.frame() |> dim())[[1]] })
modis_cropped_125 |> sapply(function(x){ (x |> as.data.frame() |> dim())[[1]] })
