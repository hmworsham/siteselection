# Create rasters
# Description: Creates rasters for a bunch of topographic variables from an input DEM
# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 02-02-23

# Ingest config
config <- config::get(file=file.path('config', 'config.yml'))

# Load packages and local functions
devtools::load_all()
load.pkgs(config$pkgs)

# Ingest DEM from Google Drive storage
dem.tmp <- drive_download(config$dem, tempfile())$local_path
dem <- rast(dem.tmp)
names(dem) <- 'dem_10m'

# Rescale DEMs and write to tif
dem1000 <- change.res(dem, targ.res=1000, method='median')
names(dem1000) <- 'dem_1000m'
dem100 <- change.res(dem, targ.res=100, method='median')
names(dem100) <- 'dem_100m'
dem30 <- change.res(dem, targ.res=30, method='median')
names(dem30) <- 'dem_30m'

writeRaster(dem1000, file.path(out.int, 'usgs_dem_1000m.tif'), overwrite=T)
writeRaster(dem100, file.path(out.int, 'usgs_dem_100m.tif'), overwrite=T)
writeRaster(dem30, file.path(out.int, 'usgs_dem_30m.tif'), overwrite=T)

# slope
slope <- terrain(dem100, v = 'slope', unit = 'degrees')
names(slope) <- 'slope_100m'
writeRaster(slope, file.path(out.int, 'usgs_slope_100m.tif'), overwrite=T)
#slope <- NULL

# aspect
aspect100 <- terrain(dem100, v = 'aspect', unit = 'degrees')
names(aspect100) <- 'aspect_100m'
writeRaster(aspect100, file.path(out.int, 'usgs_aspect_100m.tif'), overwrite=T)
#aspect100 <- NULL

# cos(aspect)
caspect <- cosaspect(aspect100, unit='deg')
names(caspect) <- 'cosaspect_100m'
writeRaster(caspect, file.path(out.int, 'usgs_cosaspect_100m.tif'), overwrite=T)

# sin(aspect)
saspect <- sinaspect(aspect100, unit='deg')
names(saspect) <- 'sinaspect_100m'
writeRaster(saspect, file.path(out.int, 'usgs_sinaspect_100m.tif'), overwrite=T)

# Folded aspect
faspect <- fold.aspect(aspect100, fold=205)
names(faspect) <- 'usgs_205faspect_100m'
writeRaster(faspect, file.path(out.int, 'usgs_205faspect_100m.tif'), overwrite=T)

# Eastness
eastness <- facing(slope, aspect100, focal=90, unit='deg')
names(eastness) <- 'eastness_100m'
writeRaster(eastness, file.path(out.int, 'usgs_eastness_100m.tif'), overwrite=T)

# Adjusted southness
asouthness <- facing(slope, aspect100, focal=205, unit='deg')
names(asouthness) <- '205adjsouthness_100m'
writeRaster(asouthness, file.path(out.int, 'usgs_205adjsouthness_100m.tif'), overwrite=T)

# THL
thl <- thl(38.95, aspect100, slope, unit='deg', fold=205, equation=3)
names(thl) <- 'heatload_100m'
writeRaster(thl, file.path(out.int, 'usgs_heatload_100m.tif'), overwrite=T)

# Curvature
curv10 <- curvature(dem, type = c('bolstad'))
curv100 <- curvature(dem100, type=c('bolstad'))
names(curv10) <- 'curvature_10m'
names(curv100) <- 'curvature_100m'
writeRaster(curv10, file.path(out.int, 'usgs_curvature_10m.tif'))
writeRaster(curv100, file.path(out.int, 'usgs_curvature_100m.tif'))

# TPI
tpi1000 <- tpi(dem100, scale = 9, win = 'rectangle', normalize = T)
names(tpi1000) <- 'tpi_1000m'
#tpi1000<- change.res(tpi_1000, targ.res=100, method='bilinear')
writeRaster(tpi_1000, file.path(out.int, 'usgs_tpi_1000m.tif'), overwrite = T)

tpi2000 <- tpi(dem100, scale = 19, win = 'rectangle', normalize = T)
names(tpi2000) <- 'tpi_2000m'
#tpi2000 <- change.res(tpi2000, targ.res=100, method='mean')
writeRaster(tpi2000, file.path(out.int, 'usgs_tpi_2000m.tif'), overwrite=T)

# TWI
twi100 <- twi(dem100, calc.res=100, out.res=100, fill.sinks=T, deg=0.1)
names(twi100) <- 'twi_100m'
writeRaster(twi100, file.path(out.int, 'usgs_twi_100m.tif'), overwrite = T)

twi1000 <- twi(dem100, calc.res=1000, out.res=100, fill.sinks=T, deg=0.1)
names(twi1000) <- 'twi_1000m'
writeRaster(twi1000, file.path(out.int, 'usgs_twi_1000m.tif'), overwrite = T)

# upslope contributing area
uca100 <- uca(dem100, calc.res=100, out.res=100, fill.sinks=T, deg=0.1)
names(uca100) <- 'uca_100m'
writeRaster(uca100, file.path(out.int, 'usgs_uca_100m.tif'), overwrite=T)

uca1000 <- uca(dem100, calc.res=1000, out.res=100, fill.sinks=T, deg=0.1)
names(uca1000) <- 'uca_1000m'
writeRaster(uca1000, file.path(out.int, 'usgs_uca_1000m.tif'), overwrite=T)

# Clean environment
rm(list=ls())
