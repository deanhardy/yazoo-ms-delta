#===============================================================
# data retrieval for exploring census data from 2000 to present
# within a pre-defined area of interest (AOI)
#===============================================================
rm(list=ls())

# import libraries
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
  
# define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/yazoo-ms-delta')

#====================================================================
# define AOI, census year, geography, & explore variables of interest
#====================================================================
YR <- seq(2013, 2024, 1) 
ST <- c('MS')
CNTY <- c('Bolivar', 'Carroll', 'Coahoma', 'Desoto', 'Grenada', 'Holmes', 'Humphreys', 'Issaquena', 'Leflore', 'Panola', 
          'Quitman', 'Sharkey', 'Sunflower', 'Tallahatchie', 'Tate', 'Tunica', 'Warren', 'Washington', 'Yazoo')

# v00 <- load_variables(2000, 'sf1')
# v10 <- load_variables(2010, 'sf1')
# v20 <- load_variables(2020, 'dhc')
# v24 <- load_variables(2024, 'acs5')

# 2000-2010 decennial variables
SF1 = c(total = "P003001", white = "P003002", black = "P003003",
        native_american = "P003004", asian = "P003005",
        hawaiian = "P003006", other = "P003007",
        multiracial = "P003008", latinx = "P005010", 
        hu = "H001001")

# 2020 decennial variables
DHC20 = c(total = "P8_001N", white = "P8_003N", black = "P8_004N",
           native_american = "P8_005N", asian = "P8_006N",
           hawaiian = "P8_007N", other = "P8_008N",
           multiracial = "P8_009N", latinx = "P8_002N", 
          hu = 'H1_001N')

# acs 5-yr variables
ACS5 = c(total = "B03002_001", white = "B03002_003", black = "B03002_004",
         native_american = "B03002_005", asian = "B03002_006",
         hawaiian = "B03002_007", other = "B03002_008",
         multiracial = "B03002_009", latinx = "B03002_012", 
         hu = "B25001_001", medhhinc = "B19049_001", agghhinc = "B19025_001")


#====================================================================
# import AOI boundary & ancillary data
#====================================================================
AOI <- st_read(file.path(datadir, 'spatial/lwr_yazoo_basin')) %>%
  mutate(sqkm_site = as.numeric(st_area(geometry) / 1e6)) %>%
  st_transform(4326)

rd <- primary_roads(year = 2024) %>%
  filter(RTTYP == 'I')
urban <-urban_areas(year = '2020')
cnty <- counties(state = 'MS')
cnty_list <- list_counties('MS')

#====================================================================
# for loop to assess block group demographic changes of site 
# over assigned time period 
#====================================================================
df <- NULL

for (i in 1:length(YR)) {
  OUT <- get_acs(geography = 'block group',
                 variables = ACS5,
                 state = ST,
                 year = YR[[i]],
                 # year = 2024,
                 county = CNTY,
                 output = 'wide',
                 geometry = TRUE,
                 keep_geo_vars = TRUE,
                 cache_table = TRUE) %>%
    st_transform(4326) %>%
    mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhincE = agghhincE/huE,
           propPOCE = 1 - (whiteE/totalE),
           year = YR[[i]]
           ) %>%
    dplyr::select(year, GEOID, ALAND, AWATER, sqkm_bg, totalE, whiteE, blackE, native_americanE, asianE, hawaiianE,
                  otherE, multiracialE, latinxE, propPOCE, huE, medhhincE, agghhincE, mnhhincE)
  
  df <- rbind(df, OUT)
}


#====================================================================
# plot map of data
#====================================================================
ggplot() + 
  geom_sf(data = filter(df, year == 2024), aes(fill = 100*propPOCE)) + 
  geom_sf(data = cnty, color = 'black', linewidth = 0.5) + 
  geom_sf(data = AOI, color = 'orange', linewidth = 1) + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1) + 
  labs(title = "People of Color by Block Group, 2024",
       caption = "Data source: 2024 5-year ACS, US Census Bureau",
       fill = "Percentage") + 
  theme_void()
  

#====================================================================
# OLD
#====================================================================
cnty_dec <- 
  get_decennial(
    geography = "county",
    variables = DHC20,
    state = ST,
    county = CNTY,
    year = YR,
    sumfile = 'dhc',
    output = 'wide',
    geometry = TRUE,
    keep_geo_vars = TRUE
    )

## get block group data 
bg_acs <- get_acs(geography = "block group",
                 variables = VAR,
                 state = ST,
                 county = CNTY,
                 year = YR,
                 output = 'wide',
                 geometry = TRUE,
                 keep_geo_vars = TRUE)

## get place data for rolling fork
pl20 <- get_decennial(geography = "place",
                    variables = PL20,
                    state = ST,
                    year = 2020,
                    output = 'wide',
                    geometry = TRUE,
                    keep_geo_vars = TRUE)


cnty_acs2 <- cnty_acs %>%
  st_sf() %>%
  st_transform(alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu,
         propPOC = 1 - (white/total)) %>%
  dplyr::select(NAME.x, GEOID, ALAND, AWATER, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, mnhhinc) 

## tidy BG census data
bg2 <- bg_acs %>%
  st_sf() %>%
  st_transform(alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu,
         propPOC = 1 - (white/total)) %>%
  dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, mnhhinc) 


st <- get_acs(geography = 'county',
              state = c('MS'),
              variables = VAR,
              year = 2024,
              geometry = TRUE,
              output = 'wide') %>%
  mutate(mnhhinc = agghhinc/hu,
         propPOC = 100*(1 - (white/total)),
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
  tm_borders(col = 'green', lwd = 2) + 
  tm_compass(position = c(0.11,0.12),
             size = 1.1) +
  # tm_scale_bar(position = 'left',
  #              breaks = c(0,50,100),
  #              text.size = 0.5) + 
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
  tm_polygons(fill = 'popd',
              fill.scale = tm_scale_continuous(values = "-hcl.blues3"),
              # fill.scale = tm_scale_intervals(breaks = seq(0, 150, by = 30)),
              fill.legend = tm_legend(
                title = 'Population Density (per KM\u00B2)', 
                orientation = "landscape",
                position = tm_pos_out("center", "bottom"), 
                frame = FALSE)
              ) +
  tm_shape(AOI) +
    tm_borders(col = 'red', lwd = 3) + 
  tm_layout(inner.margins = c(0.05,0.05,0.05,0.05),
            # legend.position = c('left', 'BOTTOM'),
            legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE,
            frame = FALSE
  )
pop

## proportion POC map
race <- tm_shape(st) + 
  tm_polygons(fill = 'propPOC',
              fill.scale = tm_scale_continuous(values = "-pu_gn"),
              fill.legend = tm_legend(
                title = 'People of Color (%)', 
                orientation = "landscape",
                position = tm_pos_out("center", "bottom"), 
                frame = FALSE)
  ) +
  tm_shape(AOI) +
  tm_borders(col = 'red', lwd = 3) + 
  tm_layout(inner.margins = c(0.1,0.05,0.05,0.05),
            # legend.position = c('left', 'BOTTOM'),
              legend.outside.position = "bottom",
              legend.outside.size = 0.35,
              legend.outside = TRUE,
            frame = FALSE
            )
race

## median hh income map
inc <- tm_shape(st) + 
  tm_polygons(fill = 'medhhinc',
              fill.scale = tm_scale_continuous(values = "-viridis"),
              fill.legend = tm_legend(
                title = 'Median Household Income (x $1000)', 
                orientation = "landscape",
                position = tm_pos_out("center", "bottom"), 
                frame = FALSE,
                size = 1.5)
  ) +
  tm_shape(AOI) +
  tm_borders(col = 'red', lwd = 3) + 
  tm_compass(
    position = c(0.17, 0.2),
             size = 1.5) +
  tm_scalebar(
    # position = c(0, 0.13),
    position = c('bottom', 'left'),
               breaks = c(0,100),
               text.size = 1.5) +
  tm_layout(inner.margins = c(0.05,0.05,0.05,0.05),
            # legend.position = c('left', 'BOTTOM'),
            legend.outside.position = "bottom",
            legend.outside.size = 1.35,
            legend.outside = TRUE,
            frame = FALSE
  )
inc

## create panel figure
jpeg(paste0(datadir, "/yazoo-panel.jpeg"),
     units = 'in',
     res = 300,
     height = 7.4,
     width = 15.7)
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
