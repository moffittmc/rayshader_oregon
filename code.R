library(devtools)
library(leaflet)
library(rgdal)
library(tidyverse)
options(rgl.useNULL=TRUE)
library(rgl)
library(rayshader)
library(magick)

session_info()

#install older version of RcppArmadillo which has the correct compiler for rayshader
RcppArmadillo::armadillo_version(single = F)
detach(package:RcppArmadillo)
packageurl <- "https://cran.r-project.org/src/contrib/Archive/RcppArmadillo/RcppArmadillo_0.9.900.3.0.tar.gz"
install.packages(packageurl, repos=NULL, type="source")


#Selecting a Region
bbox <- list(
  p1 = list(long = -121.316, lat = 45.675),
  p2 = list(long = -121.291, lat = 45.695)
)

leaflet() %>%
  addTiles() %>%
  addRectangles(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
    fillColor = "transparent"
  ) %>%
  fitBounds(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat
  )

#Setting Image Size
#Outsourced function below:
define_image_size <- function(bbox, major_dim = 400) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}

image_size <- define_image_size(bbox, major_dim = 600)

#Downloading Elevation Data
#Outsourced function below:
get_usgs_elevation_data <- function(bbox, size = "400,400", file = NULL, 
                                    sr_bbox = 4326, sr_image = 4326) {
  require(httr)
  
  # TODO - validate inputs
  
  url <- parse_url("https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage")
  res <- GET(
    url, 
    query = list(
      bbox = paste(bbox$p1$long, bbox$p1$lat, bbox$p2$long, bbox$p2$lat,
                   sep = ","),
      bboxSR = sr_bbox,
      imageSR = sr_image,
      size = size,
      format = "tiff",
      pixelType = "F32",
      noDataInterpretation = "esriNoDataMatchAny",
      interpolation = "+RSP_BilinearInterpolation",
      f = "json"
    )
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    # TODO - check that bbox values are correct
    # message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    
    img_res <- GET(body$href)
    img_bin <- content(img_res, "raw")
    if (is.null(file)) 
      file <- tempfile("elev_matrix", fileext = ".tif")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    warning(res)
  }
  invisible(file)
}

#Specify path
elev_file <- file.path("data", "or-elevation.tif")

#Get elevation data
get_usgs_elevation_data(bbox, size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)


#Plotting data
read_elevation_file <- function(file) {
  elev_img <- raster::raster(file)
  elev_matrix <- matrix(
    raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
    nrow = ncol(elev_img), ncol = nrow(elev_img)
  )
  elev_matrix
}

#Create matrix
elev_matrix <- read_elevation_file(elev_file)

#Calculate layers
ambmat <- ambient_shade(elev_matrix, zscale = 30)
raymat <- ray_shade(elev_matrix, zscale = 30, lambert = TRUE)
watermap <- detect_water(elev_matrix)

#Plot 2D
elev_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()


#Get more detail
#Outsourced function below:
get_arcgis_map_image <- function(bbox, map_type = "World_Street_Map", file = NULL, 
                                 width = 400, height = 400, sr_bbox = 4326) {
  require(httr)
  require(glue) 
  require(jsonlite)
  
  url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")
  
  # define JSON query parameter
  web_map_param <- list(
    baseMap = list(
      baseMapLayers = list(
        list(url = jsonlite::unbox(glue("https://services.arcgisonline.com/ArcGIS/rest/services/{map_type}/MapServer",
                                        map_type = map_type)))
      )
    ),
    exportOptions = list(
      outputSize = c(width, height)
    ),
    mapOptions = list(
      extent = list(
        spatialReference = list(wkid = jsonlite::unbox(sr_bbox)),
        xmax = jsonlite::unbox(max(bbox$p1$long, bbox$p2$long)),
        xmin = jsonlite::unbox(min(bbox$p1$long, bbox$p2$long)),
        ymax = jsonlite::unbox(max(bbox$p1$lat, bbox$p2$lat)),
        ymin = jsonlite::unbox(min(bbox$p1$lat, bbox$p2$lat))
      )
    )
  )
  
  res <- GET(
    url, 
    query = list(
      f = "json",
      Format = "PNG32",
      Layout_Template = "MAP_ONLY",
      Web_Map_as_JSON = jsonlite::toJSON(web_map_param))
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    if (is.null(file)) 
      file <- tempfile("overlay_img", fileext = ".png")
    
    img_res <- GET(body$results[[1]]$value$url)
    img_bin <- content(img_res, "raw")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    message(res)
  }
  invisible(file)
}


#Retrieve map details (3 types)
#World_Topo_Map
#World_Street_Map
#World_Imagery
overlay_file <- "images/or-map.png"
overlay_file2 <- "images/or-map2.png"
overlay_file3 <- "images/or-map3.png"
get_arcgis_map_image(bbox, map_type = "World_Topo_Map", file = overlay_file,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)
get_arcgis_map_image(bbox, map_type = "World_Street_Map", file = overlay_file2,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)
get_arcgis_map_image(bbox, map_type = "World_Imagery", file = overlay_file3,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)
overlay_img <- png::readPNG(overlay_file)
overlay_img2 <- png::readPNG(overlay_file2)
overlay_img3 <- png::readPNG(overlay_file3)


# 2D plot with added map overlays
elev_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.4) %>%
  add_overlay(overlay_img3, alphalayer = 0.8) %>%
  plot_map()

# 3D Render
zscale <- 10
rgl::clear3d()
options(rgl.printRglwidget = TRUE)

elev_matrix %>% 
  sphere_shade(texture = "imhof4") %>% 
  add_water(watermap, color = "imhof4") %>%
  add_overlay(overlay_img, alphalayer = 0.8) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = TRUE, soliddepth = -max(elev_matrix)/zscale, wateralpha = 0,
          theta = 25, phi = 30, zoom = 0.65, fov = 60)
render_snapshot()
rglwidget()

