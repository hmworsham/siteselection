# Site selection helpers

#' Load packages
#' @description Loads new packages, installing if they're not already installed
#' @param pkg Character string. Package name
#' @return NULL. Loads packages in global environment
#' @export load.pkgs
#'
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#' Make polygons
#' @description Creates polygons from input coordinates, radius, and CRS
#' @param input Dataframe. Should specify site ID, latitude, and longitude
#' @param radius Numeric. Desired radius of output polygon
#' @param shape Character string. One of 'rectangle' or 'circle'. Default is 'rectangle'
#' @param target.crs Numeric. Desired output CRS in the form of a numeric EPSG code
#' @return sf object containing polygons
#' @export makepolys

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


#' Print figures
#' @description Generates figures with ggplot and writes to png
#' @param df Dataframe object. Input dataframe for plotting
#' @param outdir Character string. Destination directory for writing png
#' @return NULL. Writes to file

print.figs <- function(df,
                       factor.var=NULL,
                       pal=NULL,
                       outdir,
                       dims=c(1280,900),
                       reso=300){

  # Create outdir if it doesn't exist
  dir.create(file.path('.', outdir), showWarnings = FALSE)

  # # Deal with factor var
  if (!is.null(factor.var)) {
    if (length(unique(df[,factor.var]))==2) {
    df$Factor_Var <- factor(df[,factor.var], levels=c('TRUE', 'FALSE'))
    } else
      df$Factor_Var <- factor(df[,factor.var])
  }

  # Generate plot for each topo variable
  for (t in seq(7, length(df)-1)) {

    # Define plot name for saving
    plotname <- paste0(names(df)[t], '.png') #name file

    # Select colors
    clr <- pal[t-6]

    # Calculate 1:1 line coefficients
    i1 <- round(min(df[ , t])-(max(df[ , t])-min(df[ , t]))/(nrow(df)-1), 2)
    s1 <- round((max(df[ , t])-min(df[ , t]))/(nrow(df)-1), 2)
    s2 <- 2*(max(df[ , t])-min(df[ , t]))/(nrow(df)-1)

    # Generate plot
    ggp <- ggplot(df, aes(x = reorder(Location_ID,  df[, t]), y = df[, t]))

    # Conditional color handling
    if (!is.null(factor.var)) {
      ggp <- ggp + geom_point(aes(fill=Factor_Var), size=5, shape=21)
      print(length(unique(df$Factor_Var)))
      if(length(unique(df$Factor_Var)) > 2) {
        ggp <- ggp + scale_fill_brewer(
          palette='RdYlBu',
          name=factor.var)
      } else if (is.null(pal)) {
        ggp <- ggp + scale_fill_manual(
          values=c('red2', 'grey70'),
          name=factor.var)
      } else {
        ggp <- ggp + scale_fill_manual(
          values=c(clr, 'grey70'),
          name=factor.var)
      }
    } else {
      if(is.null(pal)) {
        brks <- c(min(df$Elevation_m), max(df$Elevation_m))
        ggp <- ggp +
          geom_point(aes(fill=Elevation_m), size=5, shape=21) +
          scale_fill_continuous(type='viridis', breaks=brks, name='Elevation (m a.s.l.)')
      } else {
        ggp <- ggp + geom_point(size=5, shape=21, fill=clr)
      }
    }

    ggp <- ggp +

      # Smoothing, 1:1 line and equations
      geom_smooth(
        aes(
          x=1:nrow(df),
          y=df[order(df[,t]), names(df)[t]]),
        method = 'lm',
        formula= y~x,
        se=FALSE,
        color='black',
        linewidth=0.5) +
      geom_abline(
        intercept=i1,
        slope=s1,
        color='black',
        linetype='dashed') +
      stat_poly_eq(
        aes(
          x=1:nrow(df),
          y=df[order(df[,t]), names(df)[t]],
          label = paste(after_stat(eq.label), sep = "~~~")),
        label.x.npc = "right",
        label.y.npc = 0.15,
        eq.with.lhs = "italic(hat(y))~`=`~",
        eq.x.rhs = "~italic(x)",
        formula = y~x ,
        parse = TRUE,
        size = 5) +
      stat_poly_eq(
        aes(
          x=1:nrow(df),
          y=df[order(df[,t]), names(df)[t]],
          label = paste(after_stat(rr.label), sep = "~~~")),
        label.x.npc = "right",
        label.y.npc = "bottom",
        formula = y~x,
        parse = TRUE,
        size = 5) +

      # Formatting
      scale_y_continuous(name = str_replace_all(names(df)[t], '_', ' ')) +
      labs(x = 'Plot ID', y = str_replace_all(names(df)[t], '_', ' ')) +
      theme_light(base_size = 20) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position='bottom',
            legend.title=element_text(size=18, vjust=1))

    png(
      file.path(outdir, plotname),
      width = dims[1],
      height = dims[2],
      units = 'px',
      res = reso)
    print(ggp)
    dev.off()
  }
}

# Pull legend grob from ggplot
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}
