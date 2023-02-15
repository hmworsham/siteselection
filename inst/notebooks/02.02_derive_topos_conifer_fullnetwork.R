# Derive topo values for final network of conifer plots
# Calculates the values of various topographic factors for rectangular 40x40 m neighborhoods around specified coordinates and/or within the extents of forest inventory plots in the East River watershed.
# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 02-03-23

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load packages and local functions
devtools::load_all()
load.pkgs(config$pkgs)

# Ingest index with extant conifer plot characteristics
tmpfile <- drive_download(
  as_id(config$extdata$siteindex),
  type='csv',
  tempfile())$local_path
siteinfo.ext <- as.data.frame(read.csv(tmpfile))
coords.ext <- siteinfo.ext[,c('Location_ID', 'Latitude', 'Longitude')]

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

# Ingest rasters as list of SpatRaster objects
topo.rasters <- lapply(topo.rasters, rast)

# Compute topo statistics from input rasters for existing research sites
conifer.zonals <- zonals(coords.ext, topo.rasters, type='coord', radius=20, shape='rectangle')

# Write out topo stats to csv
write.csv(conifer.zonals, file.path(config$dat_pro, 'conifer_zonals_fullnetwork.csv'))
