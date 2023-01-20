###################################################
## data retrieval for exploring census data ##
###################################################

rm(list=ls())

## import libraries
library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(sf)
library(tmap)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/yazoo-ms-delta')

## define equal area projection for area estimates
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

## define census year, geography, & variables of interest
DAT <- 'acs5'
YR <- 2020
ST <- c('MS')
CNTY <- c('Bolivar', 'Carroll', 'Coahoma', 'Desoto', 'Grenada', 'Holmes', 'Humphreys', 'Issaquena', 'Leflore', 'Panola', 
          'Quitman', 'Sharkey', 'Sunflower', 'Tallahatchie', 'Tate', 'Tunica', 'Warren', 'Washington', 'Yazoo')
VAR = c(white = "B03002_003E", black = "B03002_004E",
        native_american = "B03002_005E", asian = "B03002_006E",
        hawaiian = "B03002_007E", other = "B03002_008E",
        multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
        medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E")

# dat <- load_variables(YR, DAT)

## get county level data
cnty<- get_acs(geography = "block group",
               variables = VAR,
               state = ST,
               county = CNTY,
               year = YR,
               output = 'wide',
               geometry = TRUE,
               keep_geo_vars = TRUE)

## get block group data 
bg <- get_acs(geography = "block group",
                 variables = 	VAR,
                 state = ST,
                 county = CNTY,
                 year = YR,
                 output = 'wide',
                 geometry = TRUE,
                 keep_geo_vars = TRUE)

## tidy BG census data
bg2 <- bg %>%
  st_sf() %>%
  st_transform(alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu,
         propPOC = 1 - (white/total)) %>%
  dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, mnhhinc) 


st <- get_acs(geography = 'county',
              state = c('MS'),
              variables = VAR,
              year = 2020,
              geometry = TRUE,
              output = 'wide') %>%
  mutate(mnhhinc = agghhinc/hu,
         propPOC = 1 - (white/total),
         medhhinc = medhhinc/1000,
         sqkm_cnty = as.numeric(st_area(geometry)) / 1e6) %>%
  mutate(popd = total/sqkm_cnty,
         total = total/1000)


##import area of interest data
AOI <- st_read(file.path(datadir, '/MAP_generalized_regions/MAP_generalized_regions.shp')) %>%
  st_transform(alb) %>%
  rowid_to_column() %>%
  mutate(sqkm_aoi = as.numeric(st_area(geometry) / 1e6)) %>%
  filter(region == 'Delta')

## interactive map of census data
tmap_mode(mode = c('plot'))

map <- tm_shape(st) + 
  tm_polygons() + 
  tm_shape(AOI) +
  tm_borders(col = 'red', lwd = 2) + 
  tm_compass(position = c(0.11,0.12),
             size = 1.1) +
  tm_scale_bar(position = 'left',
               breaks = c(0,50,100),
               text.size = 0.5) + 
  tm_layout(inner.margins = c(0.1,0.05,0.05,0.05))
map

# jpeg(paste0(datadir, "/yazoo-map.jpeg"), 
#      units = 'in', 
#      res = 300, 
#      height = 3, 
#      width = 2)
# map
# dev.off()

## population map
pop <- tm_shape(st) + 
  tm_polygons(col = 'popd', 
              palette = 'Greys',
              title = 'Population Density (per KM^2)',
              # style = 'cont',
              legend.is.portrait = FALSE) + 
  tm_shape(AOI) +
  tm_borders(col = 'red', lwd = 2) + 
  # tm_compass(position = c(0.11,0.12),
  #            size = 1.1) +
  # tm_scale_bar(position = c('left', 'bottom'),
  #              breaks = c(0,100),
  #              text.size = 0.5) +
  tm_layout(inner.margins = c(0.05,0.05,0.05,0.05),
            # legend.position = c('left', 'BOTTOM'),
            legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE
  )
pop

## proportion POC map
race <- tm_shape(st) + 
  tm_polygons(col = 'propPOC', palette = '-viridis',
              title = 'Proportion People of Color',
              # style = 'cont',
              legend.is.portrait = FALSE) + 
  tm_shape(AOI) +
  tm_borders(col = 'red', lwd = 2) + 
  # tm_compass(size = 1.1,
  #            position = c('right', 'bottom')) +
  # tm_scale_bar(position = 'left',
  #              breaks = c(0,50,100),
  #              text.size = 0.5) + 
  tm_layout(inner.margins = c(0.1,0.05,0.05,0.05),
            # legend.position = c('left', 'BOTTOM'),
              legend.outside.position = "bottom",
              legend.outside.size = 0.35,
              legend.outside = TRUE
            )
race

## median hh income map
inc <- tm_shape(st) + 
  tm_polygons(col = 'medhhinc', palette = 'YlOrBr', 
              title = 'Median Income (x $1000)',
              # style = 'cont',
              legend.is.portrait = FALSE) + 
  tm_shape(AOI) +
  tm_borders(col = 'red', lwd = 2) + 
  tm_compass(position = c('left', 'bottom'),
             size = 1.1) +
  tm_scale_bar(position = c('left', 'bottom'),
               breaks = c(0,100),
               text.size = 0.5) +
  tm_layout(inner.margins = c(0.05,0.05,0.05,0.05),
            # legend.position = c('left', 'BOTTOM'),
            legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE
  )
inc

## create panel figure
jpeg(paste0(datadir, "/yazoo-panel.jpeg"),
     units = 'in',
     res = 300,
     height = 3.5,
     width = 6.5)
tmap_arrange(pop, race, inc, ncol = 3, nrow = 1)
dev.off()

tmap_mode('view')

## proportion POC map
race <- tm_shape(st) + 
  tm_polygons(col = 'propPOC', palette = '-viridis',
              title = 'Proportion People of Color',
              # style = 'cont',
              legend.is.portrait = FALSE) + 
  tm_shape(AOI) +
  tm_borders(col = 'red', lwd = 2) + 
  # tm_compass(size = 1.1,
  #            position = c('right', 'bottom')) +
  # tm_scale_bar(position = 'left',
  #              breaks = c(0,50,100),
  #              text.size = 0.5) + 
  tm_layout(inner.margins = c(0.1,0.05,0.05,0.05),
            # legend.position = c('left', 'BOTTOM'),
            legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE
  )
race
