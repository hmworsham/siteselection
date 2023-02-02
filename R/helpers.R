# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Function to make polygons
makepolys <- function(input, radius, shape='rectangle', target.crs=32613){
  xy = input[,c(2,3)]
  xy = st_as_sf(xy, coords=c('Longitude', 'Latitude'), crs=4326)
  sites = st_transform(xy, target.crs)

  # Create polygons with polys
  ID = input$Location_ID
  sites = st_buffer(sites, dist=radius, endCapStyle = 'SQUARE', joinStyle = 'MITRE')
  site_names = input$Location_ID

  return(sites)
}

# Function to print figures
print.figs <- function(df, outdir){
  #'''
  # Function
  # Input: a dataframe of sites and topographic variables
  # Returns: a set of png files
  #'''

  # Define colors
  colors = c("#3B9AB2",
             "#E1AF00",
             "#F21A00",
             "#78B7C5",
             "#00A08A",
             "#F98400",
             "#4DA64D",
             "#DFB3F2",
             "#EBCC2A",
             "#2A2A2A")

  varnames = c('Elevation (m)',
               'Slope angle (º)',
               'Aspect (º)',
               expression(Heat~Load~(MJ~cm^-2~y^-1)),
               'Aspect Folded on 205º (º)',
               'Southness Adjusted to 205º (º)',
               'TWI [100 m]',
               'TWI [1000 m]',
               'TPI [1000 m]',
               'TPI [2000 m]')

  # Refactor Established
  df$Established <- factor(df$Established, levels = c('Established', 'Approved', 'Proposed'))

  # Loop through variables
  for (t in seq(3,length(df))){
    clr = colors[t-2]
    varname = varnames[t-2]

    #LM coeff
    lmv = df[order(df[, t]), t]
    lmi = round(coefficients(lm(lmv ~ seq(1, nrow(df))))[1], 2)
    lms = round(coefficients(lm(lmv ~ seq(1,nrow(df))))[2], 2)
    lmr = round(summary(lm(lmv~seq(1,nrow(df))))$r.squared, 2)

    # 1:1 line coeff
    i1 = round(min(df[, t])-(max(df[, t])-min(df[, t]))/(nrow(df)-1), 2)
    s1 = round((max(df[, t])-min(df[, t]))/(nrow(df)-1), 2)
    s2 = 2*(max(df[, t])-min(df[, t]))/(nrow(df)-1)

    # Open the png quartz image
    png(file.path(outdir,
                  paste0(names(df[t]),'.png')),
        width = 12, height = 9, units = 'in', res = 180)

    # Print the plot to png
    print(
      ggplot(df, aes(x = reorder(Location_ID,  df[, t]), y = df[, t])) +
        geom_point(aes(color = Established), size = 5) +
        # scale_color_viridis_c(
        #   name='Elevation',
        #   limits=c(
        #     min(df$Elevation_m),
        #     max(df$Elevation_m)),
        #   breaks=c(
        #     round(min(df$Elevation_m),0),
        #     round(max(df$Elevation_m))),0) +
        # scale_color_manual(values = c(clr, 'grey 70', 'grey 30')) +
        scale_color_manual(values = c(clr)) +
        scale_y_continuous(name = varname) +
        labs(x = 'Plot ID', y = names(topos)[t]) +
        theme_light(base_size = 20) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              #legend.position='bottom',
              legend.position='none',
              legend.title = element_blank(),
              legend.key.size = unit(1, 'cm')) +
        guides(scale = 'none',
               fill=guide_legend(title="New Legend Title")) +
        geom_abline(intercept=i1,
                    slope=s1,
                    color='black') +
        geom_abline(intercept=lmi,
                    slope=lms,
                    linetype='dashed',
                    color='black') +
        geom_text(x=nrow(df),
                  y=min(df[, t]),
                  label=as.expression(
                    substitute(
                      italic(r)^2~"="~lmr)),
                  hjust=1) +
        geom_text(x=nrow(df),
                  y=min(df[, t])+s1,
                  label=as.expression(
                    substitute(
                      bold('linreg:')~italic(y)~"="~lmi~"+"~lms*italic(x))),
                  hjust=1) +
        geom_text(x=nrow(df),
                  y=min(df[, t])+s2,
                  label=as.expression(
                    substitute(
                      bold('ideal:')~italic(y)~"="~i1~"+"~s1*italic(x))),
                  hjust=1))


    dev.off()
  }
}
