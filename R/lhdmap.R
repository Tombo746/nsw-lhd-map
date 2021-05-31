plot_lhdmap <- function(include = c(),
                       exclude = c(),
                       #data = lhd_map(),
                       values = "values",
                       theme = theme_map(),
                       labels = FALSE,
                       label_color = "black",
                       ...) {
  
  # check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("`ggplot2` must be installed to use `plot_usmap`.
         Use: install.packages(\"ggplot2\") and try again.")
  }
  
  # parse parameters
  #regions_ <- match.arg(regions)
  geom_args <- list(...)
  
  # set geom_polygon defaults
  if (is.null(geom_args[["colour"]]) & is.null(geom_args[["color"]])) {
    geom_args[["color"]] <- "black"
  }
  
  if (is.null(geom_args[["size"]])) {
    geom_args[["size"]] <- 0.4
  }
  
  if (is.null(geom_args[["fill"]])) {
    geom_args[["fill"]] <- "white"
  }
  
  # create polygon layer

    map_df <- lhd_map(include = include, exclude = exclude)
    geom_args[["mapping"]] <- ggplot2::aes(x = long, y = lat, group = group)
  
  polygon_layer <- do.call(ggplot2::geom_polygon, geom_args)
  
  # construct final plot
  ggplot2::ggplot(data = map_df) + polygon_layer + ggplot2::coord_equal() + theme
}

#' This creates a nice map theme for use in plot_usmap.
#' It is borrowed from the ggthemes package located at this repository:
#'   https://github.com/jrnold/ggthemes
#'
#' This function was manually rewritten here to avoid the need for
#'  another package import.
#'
#' All theme functions (i.e. theme_bw, theme, element_blank, %+replace%)
#'  come from ggplot2.
#'
#' @keywords internal
theme_map <- function(base_size = 9, base_family = "") {
  element_blank = ggplot2::element_blank()
  `%+replace%` <- ggplot2::`%+replace%`
  unit <- ggplot2::unit
  
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(axis.line = element_blank,
                   axis.text = element_blank,
                   axis.ticks = element_blank,
                   axis.title = element_blank,
                   panel.background = element_blank,
                   panel.border = element_blank,
                   panel.grid = element_blank,
                   panel.spacing = unit(0, "lines"),
                   plot.background = element_blank,
                   legend.justification = c(0, 0),
                   legend.position = c(0, 0))
}