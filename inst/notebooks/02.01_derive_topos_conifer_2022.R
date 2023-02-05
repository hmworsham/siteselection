# vDerive topo values for 2022 additions to network of conifer plots
# Calculates the values of various topographic factors for rectangular 40x40 m neighborhoods around specified coordinates and/or within the extents of forest inventory plots in the East River watershed.
# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 02-02-23

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load packages and local functions
devtools::load_all()
load.pkgs(config$pkgs)

#### Ingest data ####

# Ingest 2021 Kueppers plot characteristics CSVs
tmpfile <- drive_download(as_id(config$extdata$siteindex), tempfile())$local_path
siteinfo.ext <- as.data.frame(read_excel(tmpfile))

# Ingest 2022 proposed site info
tmpfile <- drive_download(config$extdata$siteinfo22, tempfile())$local_path
siteinfo.22 <- read.csv(tmpfile, header=T)

#### Process and clean data ####

# Prepare coordinates for processing
coords.ext <- siteinfo.ext[,c('Location_ID', 'Latitude', 'Longitude')]
coords.22 <- siteinfo.22[,c('Location_Name', 'Latitude', 'Longitude')]
names(coords.22)[1] <- 'Location_ID'

# Bind existing coordinates df to new coordinates df
allcoords <- rbind(coords.ext, coords.22)

# Create sets of 'good' and 'bad' sites for inclusion/exclusion
# goodsites <- c() # Use if good sites list is short
badsites <- c('Ute Gulch 2',
              'Baldy Mountain east 4',
              'Snodgrass NW slope 2',
              'Emerald 1',
              'Cement Creek 28')
goodsites <- coords.22[!coords.22$Location_ID %in% badsites, 'Location_ID']

# Filter all coordinates to those in goodsites
submitcoords <- allcoords[allcoords$Location_ID %in% goodsites,]

# Rename rows to integer sequence
rownames(submitcoords) <- NULL

# Write coordinates to CSV
write.csv(
  submitcoords,
  file.path(
    config$dat_int,
    'EastRiver_Proposed_Coordinates_2022_10.csv')
  )


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

# Ingest rasters as list of SpatRaster objects
topo.rasters <- lapply(topo.rasters, rast)

# Compute topo statistics from input rasters for existing research sites
conifer.zonals <- zonals(coords.ext, topo.rasters, type='coord', radius=20, shape='rectangle')

# Write out topo stats to csv
write.csv(conifer.zonals, file.path(config$dat_pro, 'conifer_zonals_2022.csv'))


#### Create hypothetical plot polygons ####

# Make polygons from submission coordinates
submitpolys <- makepolys(submitcoords, radius=20, shape='rectangle')
submitpolys.sf <- st_as_sf(submitpolys)
submitpolys.sf$Location_ID <- goodsites

# Write polygons to shp
st_write(
  submitpolys,
  file.path(config$dat_int, 'proposed_sites_2022'),
  driver="ESRI Shapefile", append=F)

# Check that shp is readable and plot
plot(st_read(file.path(config$dat_int, 'proposed_sites_2022')))
