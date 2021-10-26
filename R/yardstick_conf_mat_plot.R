#' Confusion Matrix Heatmap Plot for Categorical Data
#' @description Calculates a cross-tabulation graphic of observed and predicted classes. A [ggplot2::geom_tile()] graphic.
#'
#' @param object The `conf_mat` data frame returned from `conf_mat()`.
#' @param colors a vector of 3 colors for the most correct, mid-point, and most miss-classified frequencies of predictions in the heatmap.
#' @param ggtheme ggplot2 function or theme object. Default value is
#'   `theme_minimal`. Other values include the official ggplot2 themes
#'   theme_gray, theme_bw, theme_minimal, theme_classic, theme_void, .... Theme
#'   objects are also allowed (e.g., `theme_classic()`).
#' @param outline.color the outline color of square or circle. Default value is
#'   "gray20".
#' @importFrom ggplot2 guide_axis after_scale rel
#' @importFrom purrr map_chr
#' @return A ggplot2 plot object
#' @export
ggconfusion_matrix <- function(object,
                               colors = c(
                                 "#00BFC4",
                                 "#7CAE00",
                                 "#F8755D"
                               ),
                       ggtheme = ggplot2::theme_minimal,
                       outline.color = "gray20") {

  if (class(object) != "conf_mat") {
    stop("Need a {yardstick} confusion matrix!")
  }

  `%+%` <- ggplot2::`%+%`

  table <- object$table

  df <- as.data.frame.table(table)

  # Force known column names, assuming that predictions are on the
  # left hand side of the table (#157).
  names(df) <- c("Prediction", "Truth", "Freq")

  # Have prediction levels going from high to low so they plot in an
  # order that matches the LHS of the confusion matrix
  lvls <- levels(df$Prediction)
  df$Prediction <- factor(df$Prediction, levels = rev(lvls))
  axis_labels <- get_axis_labels(object)

  df <- dplyr::mutate(df, fillflag = dplyr::if_else(Prediction == Truth,
                                     0, (Freq + .01)))

  df <- dplyr::mutate(df, fillscale = dplyr::if_else(fillflag == 0,
                                     0, fillflag / max(fillflag)))

  p <- ggplot2::ggplot(df,
      ggplot2::aes(
        x = Truth,
        y = Prediction,
        fill =  fillscale)
      ) %+%
    ggplot2::geom_tile(show.legend = FALSE,
                       color = outline.color) %+%
    ggplot2::scale_x_discrete(guide = guide_axis(n.dodge = 2)) %+%
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      legend.position = "none"
    ) %+%
    ggplot2::geom_text(mapping = ggplot2::aes(label = Freq,
                                              color = after_scale(map_chr(fill, best_contrast))),
                       hjust = "middle",
                       size = rel(5)) %+%
    ggplot2::labs(x = axis_labels$x, y = axis_labels$y)

  # depending on the class of the object, add the specified theme
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme()
  } else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }

  # adding colors
  p <-
    p + ggplot2::scale_fill_gradient2(
      low = colors[1],
      mid = colors[2],
      high = colors[3],
      midpoint = 0.5,
      limit = c(0, 1),
      space = "Lab"
    )

  return(p)

}

best_contrast <- function(x, y = c("#010101","#FFFFFF")){
  contrasts <- prismatic::contrast_ratio(x, y)
  y[max(contrasts) == contrasts][1]
}

# Note: Always assumes predictions are on the LHS of the table
get_axis_labels <- function(x) {
  table <- x$table

  labels <- names(dimnames(table))

  if (is.null(labels)) {
    labels <- c("Prediction", "Truth")
  }

  list(
    y = labels[[1]],
    x = labels[[2]]
  )
}
