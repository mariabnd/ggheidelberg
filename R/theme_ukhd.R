#' Add UKHD theme to ggplot chart
#'
#' This function ensures adherence to the UKHD styleguide available from:
#' \href{https://design.ukhd.de}{Markenportal}
#' 
#' @importFrom "ggplot2" "theme"
#' @author mariabnd
#' @param base_size Desired base font size. Other font sizes are defined in
#' relation to it. Defaults to 16
theme_ukhd <- function(base_size = 16){
  #font <- "Helvetica"
  font <- "sans"
  ggplot2::theme(
    # Set elements for title
    plot.title = ggplot2::element_text(family = font,
                                       size = base_size,
                                       face = "bold",
                                       colour = "#535455",
                                       hjust = 0),
    # Set elements for subtitle
    plot.subtitle = ggplot2::element_text(family = font,
                                          size = ceiling(base_size * 0.6),
                                          margin = ggplot2::margin(9, 0, 9, 0)),
    # Set elements for caption
    plot.caption = ggplot2::element_text(family = font,
                                         size = ceiling(base_size * 0.6),
                                         colour = "#535455",
                                         hjust = 0),
    
    # Set eleemnts for legend
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(family = font,
                                         face = "bold",
                                         size = ceiling(base_size * 0.4),
                                         colour = "#535455"),
    #legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font,
                                        size = ceiling(base_size * 0.4),
                                        colour = "#535455"),
    legend.key.size = unit(0.5, "lines"),
    # Set elements for axes
    axis.title = ggplot2::element_text(family = font,
                                       face = "bold",
                                       size = ceiling(base_size * 0.4),
                                       colour = "#535455"),
    axis.text = ggplot2::element_text(family = font,
                                      size = ceiling(base_size * 0.4),
                                      colour = "#535455"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10),
                                        colour = "#535455"),
    #axis.ticks = ggplot2::element_blank(),
    axis.ticks.length = unit(0.1, "cm"),
    axis.line = ggplot2::element_blank(),
    
    # Set elements for grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "#535455", 
                                               linewidth = 0.25,
                                               linetype = 3),
    #panel.grid.major.x = ggplot2::element_line(colour = "#535455",
    #                                           linewidth = 0.25,
    #                                           linetype = 3),
    panel.grid.major.x = ggplot2::element_blank(),
    # Set elements for background=
    panel.background = ggplot2::element_blank(),
    # Set elements for facets
    #strip.background = ggplot2::element_rect(fill = "white"),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = ceiling(base_size * 0.6),
                                       hjust = 0,
                                       colour = "#535455",
                                       face = "bold")
  )
}

#' Colour palettes following the UKHD style guide
#' 
#' This function provides colourschemes from the UKHD styleguide available from:
#' \href{https://design.ukhd.de}{Markenportal}
#'
#' @importFrom "grDevices" "rgb"
#' @author mariabnd
#' @param n Number of colours desired
#' @param name Palette chosen. Options are "primary" (dark blue), "secondary"
#' (cyan), "grey", "orange", "beige", and "green" as well as "blueorange1",
#' "blueorange2", and "greige". Defaults to primary
#' @examples
#' scales::show_col(ukhd_pal())
#' colorblindr::palette_plot(ukhd_pal())
ukhd_pal <- function(n, name = "primary"){
  nm1 <- c("primary", "secondary", "grey", "orange", "beige", "green")
  nm2 <- c("blueorange1", "blueorange2", "greige")
  if(!(name %in% c(nm1, nm2))){
    stop(paste(name, "is not a valid palette name"))
  }
  if(name %in% nm1 & n > 5){
    stop("n too large")
  }
  if(name %in% nm2 & n > 10){
    stop("n too large")
  }
  opts <- list("primary" = c(grDevices::rgb(0, 74, 111, maxColorValue = 255), 
                             grDevices::rgb(0, 101, 139, maxColorValue = 255),
                             grDevices::rgb(72, 134, 166, maxColorValue = 255),
                             grDevices::rgb(134, 171, 195, maxColorValue = 255),
                             grDevices::rgb(194, 212, 225, maxColorValue = 255)),
               "secondary" = c(grDevices::rgb(0, 159, 227, maxColorValue = 255), 
                               grDevices::rgb(0, 173, 255, maxColorValue = 255),
                               grDevices::rgb(94, 197, 237, maxColorValue = 255),
                               grDevices::rgb(162, 218, 244, maxColorValue = 255),
                               grDevices::rgb(213, 237, 250, maxColorValue = 255)),
               "grey" = c(grDevices::rgb(84, 84, 85, maxColorValue = 255), 
                          grDevices::rgb(121, 123, 127, maxColorValue = 255),
                          grDevices::rgb(157, 158, 162, maxColorValue = 255),
                          grDevices::rgb(190, 192, 195, maxColorValue = 255),
                          grDevices::rgb(223, 224, 226, maxColorValue = 255)),
               "orange" = c(grDevices::rgb(230, 121, 0, maxColorValue = 255), 
                            grDevices::rgb(236, 150, 62, maxColorValue = 255),
                            grDevices::rgb(242, 177, 110, maxColorValue = 255),
                            grDevices::rgb(247, 203, 159, maxColorValue = 255),
                            grDevices::rgb(252, 230, 207, maxColorValue = 255)),
               "beige" = c(grDevices::rgb(199, 189, 173, maxColorValue = 255), 
                           grDevices::rgb(210, 201, 188, maxColorValue = 255),
                           grDevices::rgb(221, 214, 204, maxColorValue = 255),
                           grDevices::rgb(233, 228, 221, maxColorValue = 255),
                           grDevices::rgb(244, 241, 237, maxColorValue = 255)),
               "green" = c(grDevices::rgb(0, 162, 149, maxColorValue = 255), 
                           grDevices::rgb(52, 178, 166, maxColorValue = 255),
                           grDevices::rgb(126, 199, 188, maxColorValue = 255),
                           grDevices::rgb(177, 219, 211, maxColorValue = 255),
                           grDevices::rgb(219, 237, 233, maxColorValue = 255)))
  
  opts$blueorange1 <- c(opts$primary, rev(opts$orange))
  opts$blueorange2 <- c(opts$secondary, rev(opts$orange))
  opts$greige <- c(opts$grey, rev(opts$beige))
  
  switch(name,
         primary = switch(n - 2,
                          opts$primary[1 : 3],
                          opts$primary[1 : 4],
                          opts$primary[1 : 5]),
         secondary = switch(n - 2,
                            opts$secondary[1 : 3],
                            opts$secondary[1 : 4],
                            opts$secondary[1 : 5]),
         grey = switch(n - 2,
                       opts$grey[1 : 3],
                       opts$grey[1 : 4],
                       opts$grey[1 : 5]),
         orange = switch(n - 2,
                         opts$orange[1 : 3],
                         opts$orange[1 : 4],
                         opts$orange[1 : 5]),
         beige = switch(n - 2,
                        opts$beige[1 : 3],
                        opts$beige[1 : 4],
                        opts$beige[1 : 5]),
         green = switch(n - 2,
                        opts$green[1 : 3],
                        opts$green[1 : 4],
                        opts$green[1 : 5]),
         blueorange1 = switch(n - 2,
                              opts$blueorange1[1 : 3],
                              opts$blueorange1[1 : 4],
                              opts$blueorange1[1 : 5],
                              opts$blueorange1[1 : 6],
                              opts$blueorange1[1 : 7],
                              opts$blueorange1[1 : 8],
                              opts$blueorange1[1 : 9],
                              opts$blueorange1[1 : 10]),
         blueorange2 = switch(n - 2,
                              opts$blueorange2[1 : 3],
                              opts$blueorange2[1 : 4],
                              opts$blueorange2[1 : 5],
                              opts$blueorange2[1 : 6],
                              opts$blueorange2[1 : 7],
                              opts$blueorange2[1 : 8],
                              opts$blueorange2[1 : 9],
                              opts$blueorange2[1 : 10]),
         greige = switch(n - 2,
                         opts$greige[1 : 3],
                         opts$greige[1 : 4],
                         opts$greige[1 : 5],
                         opts$greige[1 : 6],
                         opts$greige[1 : 7],
                         opts$greige[1 : 8],
                         opts$greige[1 : 9],
                         opts$greige[1 : 10]))
}

# Used by scale_x_ukhd
pal_ukhd <- function(type = "seq", palette = "primary", direction = 1) {
  ukhd <- list(
    # List of the sequential colour options
    seq = c("primary", "secondary", "grey", "orange", "beige", "green"),
    # List of the diverging colour options
    # created by combinations of the sequential colour options
    div = c("blueorange1", "blueorange2", "greige")
  )
  
  pal_name <- function(palette, type) {
    if (is.character(palette)) {
      if (!palette %in% unlist(ukhd)) {
        stop(paste(name, "is not a valid palette name"))
        palette <- "primary"
      }
      return(palette)
    }
    
    type <- match.arg(type, c("seq", "div"))
    ukhd[[type]][palette]
  }
  
  pal <- pal_name(palette, type)
  
  # Returns a function for the specific palette
  function(n) {
    pal <- ukhd_pal(n, pal)
    
    if (direction == - 1) {
      pal <- rev(pal)
    }
    pal
  }
}

#' Sequential and diverging colour scales from the UKHD colour palettes
#'
#' @importFrom "ggplot2" "discrete_scale"
#' @author mariabnd
#' @param type Type of palette. Options are "seq" (sequential) and "div"
#' (diverging)
#' @param palette Palette chosen. Options are "primary" (dark blue), "secondary"
#' (cyan), "grey", "orange", "beige", and "green" as well as "blueorange1",
#' "blueorange2", and "greige". Defaults to primary
#' @param direction Direction of the palette. Defaults to 1
#' 
#' For a continous version use the colours from ukhd_pal in scale_x_gradientn
scale_fill_ukhd <- function(name = waiver(), ..., type = "seq", palette = "primary",
                            direction = 1, na.value = "grey50"){
  ggplot2::discrete_scale(
    aesthetics = "fill", name = name,
    palette = pal_ukhd(type, palette, direction),
    ...
  )
}

#' @rdname scale_fill_ukhd
#' @export
scale_colour_ukhd <- function(name = waiver(), ..., type = "seq", palette = 1,
                              direction = 1, na.value = "grey50"){
  ggplot2::discrete_scale(
    aesthetics = "colour", name = name,
    palette = pal_ukhd(type, palette, direction),
    ...
  )
}
