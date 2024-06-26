# Mapeo de cambios en el parque natural de monte alto---------------------------

#------------------------------------------------------------------------------#
# Proyecto: CAPACITACIÓN

# Fecha de creación:          25 de junio de 2024

# Última actualización:       25 de junio de 2024
#------------------------------------------------------------------------------#

# Temas: trabajar con Google Earth Engine en R y otras herramientas de análisis geoespacial

# Cargar librerías y preparación inicial----------------------------------------

# ---- Limpiar espacio de trabajo 

rm(list = ls())

# ---- Directorio de trabajo

setwd("/Users/nicolasgalindosalazar/Library/CloudStorage/Dropbox/Learning R/Google Earth Engine Proyecto VDB")

# ---- Cargar librerías 

library(rgee)      
library(sf)        
library(tidyverse) 
library(cptcity)   
library(tidyterra)
library(leaflet)
library(leafsync)

# Inicialización del Engine-----------------------------------------------------
gee_user <- Sys.getenv("GEE_USER")
ee_Initialize(user = gee_user, drive = TRUE)

# Área de estudio---------------------------------------------------------------
study.area <- st_read('vdb_area_interes.kml') %>%
  st_zm(drop = TRUE)

# Usar GEE----------------------------------------------------------------------
study.area_ee <- study.area %>% sf_as_ee()

box_ee <- study.area_ee$geometry()$bounds()

# Cargar y procesar imágenes satelitales de GEE---------------------------------

landstat_montealto <- ee$ImageCollection('LANDSAT/LC08/C02/T1_TOA')$
  filterDate('2013-03-18', '2024-06-24')$
  filterBounds(box_ee)$
  filterMetadata('CLOUD_COVER', 'less_than', 10)

img <- ee_get_date_ic(landstat_montealto)

# ---- NDVI 2016-05-19 17:05:46

landsat_image1 <- ee$Image('LANDSAT/LC08/C02/T1_TOA/LC08_027047_20160519')$ 
  clip(box_ee) %>%
  ee$Image$select(c('B6', 'B5', 'B4')) 
ndvi_image1 <- landsat_image1$normalizedDifference(c('B5', 'B4'))

# ---- NDVI 2024-05-25 17:05:14

landsat_image2 <- ee$Image('LANDSAT/LC08/C02/T1_TOA/LC08_027047_20240525')$ 
  clip(box_ee) %>%
  ee$Image$select(c('B6', 'B5', 'B4')) 
ndvi_image2 <- landsat_image2$normalizedDifference(c('B5', 'B4'))

# ---- NDVI Diferencia

diff_palette <- cpt("ncl_BlWhRe", n = 10)

diff_visParams <- list(
  min = -1,
  max = 1,
  palette = cpt("ncl_BlWhRe", n = 10)
)

ndvi_difference <- ndvi_image1$subtract(ndvi_image2)
ndvi_diff_mapid <- ee$Image$getMapId(ndvi_difference, diff_visParams)

# Definir bbox y paleta de NDVI-------------------------------------------------

bbox <- st_bbox(study.area) %>%
  as.vector()

ndvi_palette <- cpt("grass_ndvi", n = 10)

visParams <- list(
  min = -1,
  max = 1,
  palette = ndvi_palette
)

ndvi_mapid1 <- ee$Image$getMapId(ndvi_image1, visParams)
ndvi_mapid2 <- ee$Image$getMapId(ndvi_image2, visParams)

# Generar mapas interactivos de leaflet-----------------------------------------

# ---- Mapa base

mapa_base <- leaflet(options = leafletOptions(scrollWheelZoom = FALSE)) %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery)

# ---- Mapa 2016

map1 <- mapa_base %>%
  addTiles(
    urlTemplate = ndvi_mapid1$tile_fetcher$url_format,
    options = tileOptions(opacity = 1),
    group = "2016-05-19"
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric(ndvi_palette, c(-1, 1)),
    values = c(-1, 1),
    title = "NDVI",
    opacity = .7
  ) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
  addMiniMap(position = "topright")

# ---- Mapa 2024

map2 <- mapa_base %>%
  addTiles(
    urlTemplate = ndvi_mapid2$tile_fetcher$url_format,
    options = tileOptions(opacity = 1),
    group = "2024-05-25"
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric(ndvi_palette, c(-1, 1)),
    values = c(-1, 1),
    title = "NDVI",
    opacity = .7
  ) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

# ---- Mapa Diferencia

map_diff <- mapa_base %>%
  addTiles(
    urlTemplate = ndvi_diff_mapid$tile_fetcher$url_format,
    options = tileOptions(opacity = 1),
    group = "Diferencia NDVI"
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric(diff_palette, c(-1, 1)),
    values = c(-1, 1),
    title = "Diferencia",
    opacity = .7
  ) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

# ---- Display mapas

sync(map1, map2, map_diff, ncol = 1)







