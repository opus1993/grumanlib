#' A precise & pristine [ggplot2] theme
#'
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot tilte family, face, size and margin
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justification one of `[blmcrt]`
#' @param axis_text_size font size of axis text
#' @param plot_margin plot margin (specify with [ggplot2::margin])
#' @param grid_col grid color
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis_col axis color
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#' @param plot.title.position "plot" or "panel"
#' @param plot.caption.position "plot" or "panel"
#'
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 scatterplot example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_jim()
#'
#' }
#'
#' @importFrom ggplot2 rel margin theme element_blank element_line element_text
#'
#' @export
theme_jim <- function(base_size = 12,
                      base_family = "Titillium Web",
                      plot_title_family=if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web Bold",
                      plot_title_size = 18,
                      plot_title_face="bold",
                      plot_title_margin = 10,
                      subtitle_family=if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web Light",
                      subtitle_size = 13,
                      subtitle_face = "plain",
                      subtitle_margin = 15,
                      strip_text_family = base_family,
                      strip_text_size = 12,
                      strip_text_face = "plain",
                      caption_family=if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web Light",
                      caption_size = 9,
                      caption_face = "plain",
                      caption_margin = 10,
                      axis_text_size = base_size,
                      axis_title_family = base_family,
                      axis_title_size = rel(0.75),
                      axis_title_face = "plain",
                      axis_title_just = "rt",
                      plot_margin = margin(30, 30, 30, 30),
                      grid_col = "#cccccc", grid = TRUE,
                      axis_col = "#cccccc", axis = FALSE, ticks = FALSE,
                      plot.caption.position = "plot",
                      plot.title.position = "plot"

){


  ret <-
    ggplot2::theme_minimal(base_family = base_family,
                           base_size = base_size)

  ret <- ret + theme(legend.background = element_blank())
  ret <- ret + theme(legend.key = element_blank())

  if (inherits(grid, "character") | grid == TRUE) {
    ret <-
      ret + theme(panel.grid = element_line(color = grid_col,
                                            size = 0.2))
    ret <-
      ret + theme(panel.grid.major = element_line(color = grid_col,
                                                  size = 0.2))
    ret <-
      ret + theme(panel.grid.minor = element_line(color = grid_col, size = 0.15))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0)
        ret <- ret + theme(panel.grid.major.x = element_blank())
      if (regexpr("Y", grid)[1] < 0)
        ret <- ret + theme(panel.grid.major.y = element_blank())
      if (regexpr("x", grid)[1] < 0)
        ret <- ret + theme(panel.grid.minor.x = element_blank())
      if (regexpr("y", grid)[1] < 0)
        ret <- ret + theme(panel.grid.minor.y = element_blank())
    }

  } else {
    ret <- ret + theme(panel.grid = element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <-
      ret + theme(axis.line = element_line(color = axis_col, size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x = element_blank())
      } else {
        ret <-
          ret + theme(axis.line.x = element_line(color = axis_col, size = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y = element_blank())
      } else {
        ret <-
          ret + theme(axis.line.y = element_line(color = axis_col, size = 0.15))
      }
    } else {
      ret <-
        ret + theme(axis.line.x = element_line(color = axis_col, size = 0.15))
      ret <-
        ret + theme(axis.line.y = element_line(color = axis_col, size = 0.15))
    }
  } else {
    ret <- ret + theme(axis.line = element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <-
    switch(
      tolower(substr(axis_title_just, 1, 1)),
      b = 0,
      l = 0,
      m = 0.5,
      c = 0.5,
      r = 1,
      t = 1
    )
  yj <-
    switch(
      tolower(substr(axis_title_just, 2, 2)),
      b = 0,
      l = 0,
      m = 0.5,
      c = 0.5,
      r = 1,
      t = 1
    )

  ret <-
    ret +
    theme(axis.text.x = element_text(size = axis_text_size,
                                     margin = margin(t = 0)))

  ret <-
    ret +
    theme(axis.text.y = element_text(size = axis_text_size,
                                     margin = margin(r = 0)))

  ret <-
    ret +
    theme(axis.title = element_text(size = axis_title_size,
                                    family = axis_title_family))
  ret <-
    ret +
    theme(
      axis.title.x = element_text(
        hjust = xj,
        size = axis_title_size,
        family = axis_title_family,
        face = axis_title_face
      )
    )

  ret <-
    ret + theme(
      axis.title.y = element_text(
        hjust = yj,
        size = axis_title_size,
        family = axis_title_family,
        face = axis_title_face
      )
    )

  ret <-
    ret +
    theme(
      axis.title.y.right = element_text(
        hjust = yj,
        size = axis_title_size,
        angle = 90,
        family = axis_title_family,
        face = axis_title_face
      )
    )

  ret <-
    ret +
    theme(
      strip.text = element_text(
        hjust = 0,
        size = strip_text_size,
        face = strip_text_face,
        family = strip_text_family
      )
    )

  ret <- ret +
    theme(panel.spacing = grid::unit(2, "lines"))

  ret <-
    ret +
    theme(
      plot.title = element_text(
        hjust = 0,
        size = plot_title_size,
        margin = margin(b = plot_title_margin),
        family = plot_title_family,
        face = plot_title_face
      ),
      plot.title.position = plot.title.position
    )

  ret <-
    ret +
    theme(
      plot.subtitle = element_text(
        hjust = 0,
        size = subtitle_size,
        margin = margin(b = subtitle_margin),
        family = subtitle_family,
        face = subtitle_face
      )
    )

  ret <-
    ret + theme(
      plot.caption = element_text(
        hjust = 1,
        size = caption_size,
        margin = margin(t = caption_margin),
        family = caption_family,
        face = caption_face
      ),
      plot.caption.position = plot.caption.position
    )

  ret <- ret + theme(plot.margin = plot_margin)

  ret

}

#-----------------------------------------------------------------------------
#' my_ggplot_option_settings() for loading in .Rprofile
#' @param ... Other arguments passed to \code{theme_minimal}
#' @export
my_ggplot_option_settings <- function(...) {
  ## change global theme settings (for all plots)

  ggplot2::theme_set(theme_jim(...))

  ggplot2::update_geom_defaults("point",
                       list(size = 3, stroke = .6, shape = 21))

  alpha_viridis <- function(...) {
    ggplot2::scale_fill_gradientn(..., colors = viridis::viridis(256, option = 'H'))
  }

  color_index <- c(12,8,4,11,7,3,10,6,2,9,5,1)

  options(
    ggplot2.discrete.fill = viridis::viridis_pal(option = "H")(12)[color_index],
    ggplot2.discrete.colour = viridis::viridis_pal(option = "H")(12)[color_index],
    ggplot2.continuous.fill = alpha_viridis,
    ggplot2.continuous.colour = alpha_viridis
  )

  knitr::opts_chunk$set(
    echo = TRUE,
    message = FALSE,
    warning = FALSE,
    cache = FALSE,
    eval = TRUE,
    cache.lazy = FALSE,
    df_print = "paged",
    dpi = 72,
    tidy = "styler",
    dev = "ragg_png",
    autodep = TRUE,
    out.width = '200%',
    fig.align = 'center',
    fig.width = 9,
    fig.asp = 0.618      #,   1 / phi   use 1 for facet_wrap
    #  class.output = "scroll-100"   # must include the CSS, doesn't style on github pages
  )

  knitr::opts_template$set(
    fig.large = list(fig.asp = 0.8),
    fig.square = list(fig.asp = 1),
    fig.long = list(fig.asp = 1.5)
  )

}


#-----------------------------------------------------------------------------
#' better_ggplot_default()
#' @export
better_color_legend <- ggplot2::guides(color = ggplot2::guide_colorbar(title.position = "top",
                                                    title.hjust = .5,
                                                    barwidth = grid::unit(20, "lines"),
                                                    barheight = grid::unit(.5, "lines")))
#-----------------------------------------------------------------------------
#' better_ggplot_default()
#' @export
better_fill_legend <- ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "top",
                                                  title.hjust = .5,
                                                  barwidth = grid::unit(20, "lines"),
                                                  barheight = grid::unit(.5, "lines")))
