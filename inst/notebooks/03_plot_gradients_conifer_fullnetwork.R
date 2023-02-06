# Plot geophysical gradient distribution
# Generates figures to depict the distribution of conifer forest plots along
# defined geophysical gradients

# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 02-05-23

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load packages and local functions
devtools::load_all()
load.pkgs(config$pkgs)

# Name packages to install
pkgs <- c(
  'data.table',
  'dplyr',
  'ggplot2',
  'ggpubr',
  'googledrive',
  'readxl',
  'tidyverse',
  'mrmoose')

# Ingest 2020 Kueppers plot characteristics CSVs
tmpfile <- drive_download(as_id(config$extdata$siteindex, tempfile()))$local_path
siteinfo <- read_excel(tmpfile)

# Select variables of interest from 2021 site info
topos <- siteinfo[c('Location_ID',
                  'Established',
                  'Within_SDP_Boundary',
                  'Elevation_m',
                  'Slope',
                  'Aspect',
                  'Heat_Load',
                  'Folded_Aspect_205',
                  'Southness_205',
                  'TWI_100',
                  'TWI_1000',
                  'TPI_1000',
                  'TPI_2000')]

# Make df of topo variables
topos <- as.data.frame(topos)

# Specify which plots to exclude
# outs <- c()
# topos <- topos[!topos$Site_ID %in% outs, ]
# topos <- topos[topos$Established == 'Established', ]

# Add column for sensor presence
topos$Sensor <- 'Sensors Absent'

# Assign T for sites with sensors installed
topos[topos$Location_ID %in% c(
  'CC-CVS1',
  'ER-APL1',
  'ER-APU1',
  'ER-BME1',
  'ER-GT1',
  'SG-NES1',
  'SG-SWR1',
  'SR-PVG1',
  'XX-CAR3',
  'XX-PLN1'
), 'Sensor'] <- 'Sensors Present'

# Add column for cored sites
topos$Coring <- 'Not Cored'
topos[topos$Location_ID %in% c('CC-CVS1',
                                       'CC-UC1',
                                       'ER-APL1',
                                       'ER-APU1',
                                       'ER-BME1',
                                       'ER-GT1',
                                       'SG-NES1',
                                       'SG-NES2',
                                       'SG-SWR1',
                                       'SR-PVG1',
                                       'XX-CAR1',
                                       'XX-CAR2',
                                       'XX-CAR3',
                                       'XX-PLN1',
                                       'XX-PLN2'), 'Coring'] <- 'Cored'

topos[topos$Location_ID %in% c('CC-CVS1',
                                       'ER-APU1',
                                       'SG-NES1',
                                       'SR-PVG1',
                                       'CC-EMN1'), 'Coring'] <- 'Proposed'

# Subset only established sites
# topos_est <- topos[topos$Established == T]

# Subset only proposed coring sites
# topos_coring <- topos[topos$Coring=='Cored',]

# Subset only plots in aop domain
# topos_aop <- topos[topos$Within_SDP_Boundary=='Yes',]

##############################################
# Batch create plots for individual variables
##############################################

# Print figures to png
outdir = file.path(fidir, 'Production_Images', 'AOP_Sites')
print.figs(topos_aop, outdir)

############################
# Facet grid all variables
############################

# Specify vars to include
topos.no <- c('Aspect',
              'Southness_205',
              'TWI_1000',
              'TPI_2000')
kplots <- topos[,!names(topos) %in% topos.no]

# Gather to long format
kplots_long <- gather(
  kplots,
  variable,
  value, -c(
    Location_ID,
    Established,
    Within_SDP_Boundary,
    Sensor,
    Coring))
kplots_long$Elevation <- kplots_long[kplots_long$variable=='Elevation_m','value']
kplots_long <- kplots_long %>%
  arrange(variable, value) %>%
  mutate(order = row_number())

# Set up facet grid with all variables
varsgrid <- ggplot(kplots_long, aes(x = order, y = value)) +
  geom_point(aes(fill = Elevation, size=1), shape=21) +
  #scale_fill_viridis_c(name='Elevation') +
  #scale_fill_manual(values=unname(icolors('mario'))) +
  scale_alpha_discrete(range = c(1, 0.2)) +
  scale_x_continuous(
    breaks = kplots_long$order,
    labels = kplots_long$Location_ID) +
  geom_smooth(method = "lm", se=FALSE, color='black') +
  stat_poly_eq(
    coef.digits=3,
    use_label(c('eq', 'R2'))) +
  facet_wrap(~variable, scales = 'free') +
  guides(color='none', fill='none', size='none') +
  labs(x='Site', y='Value') +
  theme(
    axis.text.x = element_text(size=9, angle=90, hjust=0.95, vjust=0.2),
    axis.title = element_text())

# Print facet grid
varsgrid
