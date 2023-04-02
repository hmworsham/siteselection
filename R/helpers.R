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
                        factor.var,
                        pal=NULL,
                        col.on=NULL,
                        outdir,
                        dims=c(1280,900),
                        reso=300){

  # Create outdir if not exists
  dir.create(file.path('.', outdir), showWarnings = FALSE)

  # Set topo variables
  df <- data.frame(
    Location_ID=df$Location_ID,
    df %>% select_if(is.numeric))

  if (!missing(factor.var)) {
    df$Factor_Var <- df[,factor.var]
    df$Factor_Var <- factor(df$Factor_Var, levels=c('T', 'F'))
  }

  # Generate plot for each topo variable
  for (t in seq(3, length(df))) {

    # Define plot name for saving
    plotname <- paste0(names(df)[t], '.png') #name file

    # Select colors
    clr <- pal[t-2]

    # Calculate 1:1 line coefficients
    i1 <- round(min(df[ , t])-(max(df[ , t])-min(df[ , t]))/(nrow(df)-1), 2)
    s1 <- round((max(df[ , t])-min(df[ , t]))/(nrow(df)-1), 2)
    s2 <- 2*(max(df[ , t])-min(df[ , t]))/(nrow(df)-1)

    # Generate plot
    ggp <- ggplot(df, aes(x = reorder(Location_ID,  df[, t]), y = df[, t]))

    # Conditional color handling
    if (missing(col.on)) {
      ggp <- ggp + geom_point(aes(fill=Factor_Var), size=5, shape=21)
      if (missing(pal)) {
        ggp <- ggp + scale_fill_manual(
          values=c('red2', 'navyblue'),
          name=factor.var)
      } else {
        ggp <- ggp + scale_fill_manual(
          values=c(clr, 'grey70'),
          name=factor.var)
      }
    } else {
      ggp <- ggp + geom_point(aes(fill=df[ , col.on]), size=5, shape=21)
      ggp <- ggp + scale_fill_viridis(name=col.on)
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
      scale_y_continuous(name = names(df)[t]) +
      labs(x = 'Plot ID', y = names(df)[t]) +
      theme_light(base_size = 20) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position='bottom')

    # Print plot
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
