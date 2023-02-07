# Plot gradients for aspen sites
# Generates figures to depict the distribution of aspen forest plots along
# defined geophysical gradients

# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 02-06-23

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load packages and local functions
devtools::load_all()
load.pkgs(config$pkgs)

# Ingest 2020 Kueppers plot characteristics CSVs
asp <- file.path(config$dat_pro, 'aspen_zonals_2023.csv')
asp <- read.csv(asp, row.names=1)

# Select variables of interest from 2021 site info
topos <- asp[c(
  'Location_ID',
  'dem_100m',
  'slope_100m',
  'aspect_100m',
  'heatload_100m',
  'usgs_205faspect_100m',
  'twi_100m',
  'twi_1000m',
  'tpi_1000m'
)]

# Make df of topo variables
topos <- as.data.frame(topos)

# Specify which plots to exclude
# outs <- c()
# topos <- topos[!topos$Site_ID %in% outs, ]
# topos <- topos[topos$Established == 'Established', ]

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
           "#00A08A",
           "#F98400",
           "#4DA64D",
           "#DFB3F2",
           "#EBCC2A")

# Print figures to png
out.dir <- file.path('inst', 'reports', 'figs', 'aspen')
print.figs(
  topos,
  col.on='dem_100m',
  pal=colors,
  outdir=out.dir,
  dims=c(2560,1920),
  reso=300)

#### Facet grid all variables ####

# Specify vars to include
topos.no <- c('aspect_100m',
              'twi_1000m',
              'tpi_2000m')
kplots <- topos[,!names(topos) %in% topos.no]

# Gather to long format
kplots_long <- kplots %>%
  pivot_longer(cols = dem_100m:tpi_1000m,
               names_to = 'variable') %>%
  arrange(variable, Location_ID)
kplots_long <- kplots_long %>%
  mutate(Elevation = as.numeric(
    flatten(
      rep(kplots_long[kplots_long$variable == 'dem_100m', 'value'],
          length(unique(kplots_long$variable))
  )))) %>%
  arrange(variable, value) %>%
  group_by(variable) %>%
  mutate(Order = 1:100,
         LIO = paste(Order, Location_ID, sep='_')) %>%
  ungroup()

locids <- function(x) sub("[^_]*_","",x )

# Set up facet grid with all variables
varsgrid <- ggplot(kplots_long, aes(x = reorder(LIO, Order), y = value)) +
  geom_point(aes(fill = Elevation, size = 1), shape = 21) +
  scale_fill_viridis_c(name = 'Elevation') +
  scale_x_discrete(labels = locids) +
  geom_smooth(
    aes(x = Order, y = value),
    method = 'lm',
    se = FALSE,
    color = 'black'
  ) +
  stat_poly_eq(
    aes(
      x = Order,
      y = value,
      label = paste(after_stat(eq.label), sep = "~~~")
    ),
    label.x.npc = "right",
    label.y.npc = 0.15,
    eq.with.lhs = "italic(hat(y))~`=`~",
    eq.x.rhs = "~italic(x)",
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) +
  stat_poly_eq(
    aes(
      x = Order,
      y = value,
      label = paste(after_stat(rr.label), sep = "~~~")
    ),
    label.x.npc = "right",
    label.y.npc = "bottom",
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) +
  facet_wrap( ~ variable, scales = 'free') +
  labs(x = 'Site', y = 'Value') +
  guides(color = 'none', fill = 'none', size = 'none') +
  theme(
    axis.text.x = element_text(
      size = 9,
      angle = 90,
      hjust = 1,
      vjust = 0.5
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
