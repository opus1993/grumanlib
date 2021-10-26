#' a categorical count helper function for cleaner ggplots
#'
#'
#' @param string Input vector. Either a character vector, or something
#'  coercible to one.
#' @param width character vector wrap for better ggplot axis labeling
#'
#'  @examples
#'
#'  x <- factor(letters[rpois(100, 5)])
#'
#'  withfreq(x) |> table()
#'
#' @export
withfreq <- function(string, width = 20){
  tibble::tibble(string) |>
    dplyr::add_count(string) |>
    dplyr::mutate(string = glue::glue("{ stringr::str_wrap(string, width = width) } ({ n })")) |>
    dplyr::pull(string)
}


