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

# Ingest 2020 Kueppers plot characteristics CSVs
tmpfile <- drive_download(
  as_id(config$extdata$siteindex),
  type='csv',
  tempfile())$local_path
siteinfo <- read.csv(tmpfile)

# Select variables of interest from 2021 site info
topos <- siteinfo[c(
  'Location_ID',
  'Established',
  'Within_SDP_Boundary',
  'Sensors',
  'Cored',
  'Inventory_Cohort',
  'Elevation_m',
  'Slope',
  'Aspect',
  'Heat_Load',
  'Folded_Aspect_205',
  'Southness_205',
  'TWI_100',
  'TWI_1000',
  'TPI_1000'
)]

# Make df of topo variables
topos <- as.data.frame(topos)
#topos$Inventory_Cohort <- factor(topos$Inventory_Cohort)
# Specify which plots to exclude
# outs <- c()
# topos <- topos[!topos$Site_ID %in% outs, ]

# Subset only established sites
# topos_est <- topos[topos$Established == T]

# Subset only cored sites
# topos_coring <- topos[topos$Coring=='Cored',]

# Subset only plots in aop domain
# topos_aop <- topos[topos$Within_SDP_Boundary=='Yes',]

#### Batch create plots for individual variables ####

# Define colors
colors = c("#3B9AB2",
           "#E1AF00",
           "#F21A00",
           "#78B7C5",
           "#00A08A")

# Print figures to png
out.dir <- file.path('inst', 'ms', 'figs' , 'by_cohort')
print.figs(
  topos,
  factor.var='Inventory_Cohort',
  pal=NULL,
  outdir=out.dir,
  dims=c(2560,1920),
  reso=300)

#### Facet grid all variables ####

# Specify vars to include
topos.no <- c('Aspect',
              'Southness_205',
              'TWI_1000',
              'TPI_2000')
kplots <- topos[,!names(topos) %in% topos.no]

# Gather to long format
kplots_long <- kplots %>%
  pivot_longer(cols = Elevation_m:TPI_1000,
               names_to = 'variable') %>%
  arrange(variable, Location_ID)

kplots_long <- kplots_long %>%
  mutate(Elevation = as.numeric(
    flatten(
      rep(kplots_long[kplots_long$variable == 'Elevation_m', 'value'],
          length(unique(kplots_long$variable))
  )))) %>%
  arrange(variable, value) %>%
  group_by(variable) %>%
  mutate(Order = 1:25,
         LIO = paste(Order, Location_ID, sep='_')) %>%
  ungroup()

locids <- function(x) sub("[^_]*_","",x )

vars.labs <- c('Elevation (m)', 'Folded aspect', 'Heat load',
               'Slope (º)', 'TPI', 'TWI')
names(vars.labs) <- unique(kplots_long$variable)

# Set up facet grid with all variables
varsgrid <- ggplot(kplots_long, aes(x = reorder(LIO, Order), y = value)) +
  geom_point(aes(size=1), shape=21, fill='grey50') +
  # geom_point(aes(fill = Elevation, size = 1), shape = 21) +
  # scale_fill_viridis_c(name = 'Elevation') +
  scale_x_discrete(labels = locids) +
  geom_smooth(
    aes(x = Order, y = value),
    method = 'lm',
    se = FALSE,
    color = 'black'
  ) +
  # stat_poly_eq(
  #   aes(
  #     x = Order,
  #     y = value,
  #     label = paste(after_stat(eq.label), sep = "~~~")
  #   ),
  #   eq.with.lhs = "italic(hat(y))~`=`~",
  #   eq.x.rhs = "~italic(x)",
  #   formula = y ~ x,
  #   parse = TRUE,
  #   size = 3
  # ) +
  facet_wrap( ~ variable, scales = 'free', labeller=labeller(variable=vars.labs)) +
  labs(x = 'Site', y = 'Value') +
  guides(color = 'none', fill = 'none', size = 'none') +
  theme_classic(base_size=12) +
  theme(
    axis.text.x = element_text(
      size = 9,
      angle = 70,
      hjust = 1
    ),
    axis.title = element_text()
  )

# Print facet grid
varsgrid

# Write to png
png(
  file.path(out.dir, 'topo_fg.png'),
  width=2560,
  height=1800,
  res=300)
print(varsgrid)
dev.off()
