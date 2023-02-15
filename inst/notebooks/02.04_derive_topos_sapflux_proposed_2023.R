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

# Ingest Berkelhammer coordinates
berk <- drive_download(
  config$extdata$sap,
  type='csv',
  tempfile())$local_path
berk <- read.csv(berk)
berk.coords <- berk %>%
  group_by(site..) %>%
  filter(row_number()==1)
berk.coords <- data.frame(Location_ID=berk.coords$site.., Longitude=berk.coords$longitude, Latitude=berk.coords$latitude)

# Potential new sap flux sites
p1 <- c(-106.9744584,38.9271352)
p2 <- c(-106.983025,38.924723)
p3 <- c(-107.0071540,38.9736823)
lons <- c(p1,p2,p3)[c(TRUE, FALSE)]
lats <- c(p1,p2,p3)[c(FALSE,TRUE)]
newsf.coords <- data.frame('ID'=seq(length(lons)),
                           'Longitude'=lons,
                           'Latitude'=lats)

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

# Compute zonals for Berkelhammer sapflux sites
zon.berk <- zonals(berk.coords, rasdir, topo.factors, type='coord', radius=20)

# Write out topo stats to csv
write.csv(zon.berk, '~/Desktop/berkelhammer_zonals.csv')

# Potential new sap flux sites
p1 <- c(-106.9744584,38.9271352)
p2 <- c(-106.983025,38.924723)
p3 <- c(-107.0071540,38.9736823)
lons <- c(p1,p2,p3)[c(TRUE, FALSE)]
lats <- c(p1,p2,p3)[c(FALSE,TRUE)]
newsf.coords <- data.frame('ID'=seq(length(lons)),
                            'Longitude'=lons,
                            'Latitude'=lats)


zon.newsf <- zonals(newsf.coords, rasdir, topo.factors, type='coord', radius=5)
write.csv(zon.newsf, '~/Desktop/newsf_zonals.csv')
