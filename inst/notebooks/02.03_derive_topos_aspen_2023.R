# Derive topo values for Blonder aspen plots
# Calculates the values of various topographic factors for rectangular 40x40 m neighborhoods around specified coordinates and/or within the extents of aspen plots in the East River watershed.
# Author: Marshall Worsham
# Created: 01-20-23
# Revised: 02-03-23

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load packages and local functions
devtools::load_all()
load.pkgs(config$pkgs)

# Ingest aspen site info
asp <- drive_download(as_id(config$extdata$asp), tempfile())$local_path
asp <- read.csv(asp)
head(asp)

# Ingest cored aspen info
asp.cored <- drive_download(as_id(config$aspcored), tempfile())$local_path
asp.cored <- read.csv(asp.cored)
head(asp.cored)

#### Process and clean data ####

# Merge site info and cored tree info
aspen <- merge(asp, asp.cored, by='Site_Code', all=T)
aspen <- aspen[!is.na(aspen$Date.Completed),]
aspen.coords <- data.frame(Site_Code=aspen$Site_Code,
                           Longitude=aspen$Longitude,
                           Latitude=aspen$Latitude)
aspen.coords <- na.omit(aspen.coords)

# Filter for target sites for sap flux install in 2023
targ <- asp[which(asp$Site_Code %in% c('ACGNT01',
                                   'AGXLZ',
                                   'DBUZY',
                                   'FOASS01',
                                   'GRJXK',
                                   'NGTWF',
                                   'OFCNI',
                                   'POZDQ',
                                   'WCPJH',
                                   'WPRLZ',
                                   'XKPJS',
                                   'ZJQRW')),]

targ.coords <- data.frame(Site_Code=targ$Site_Code,
                          Longitude=targ$Longitude,
                          Latitude=targ$Latitude)

#### Compute topographic metrics ####

# Specify topographic factors of interest
topo.factors <- c('dem_100m',
                  '205f_aspect_100m',
                  'aspect_100m',
                  'curvature_100m',
                  'heatload_100m',
                  'slope_100m',
                  'tpi_1000m',
                  'twi_100m',
                  'twi_1000m')

# List raster files corresponding to factors of interest
topo.rasters <- list.files(config$dat_int,
                           paste0('*', topo.factors, '.tif', collapse='|'),
                           full.names=T)

topo.rasters <- lapply(topo.rasters, rast)

# Compute topo statistics for all cored aspen plots
aspen.zonals <- zonals(aspen.coords, topo.rasters, type='coord', radius=10, shape='circle')

# Write out topo stats to csv
write.csv(aspen.zonals, file.path(config$dat_pro, 'aspen_zonals_2023.csv'))

# Compute topo statistics for targeted sapflux plots
targ.zonals <- zonals(targ.coords, topo.rasters, type='coord', radius=10, shape='circle')

# Write out topo stats to csv
write.csv(targ.zonals, file.path(config$dat_pro, 'aspen_sf_proposed_zonals_2023.csv'))
