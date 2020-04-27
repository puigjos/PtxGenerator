
#' Title
#'
#' @param ft flextable
#' @param x vector id
#' @param columns vector
#'
#' @return
#' @export
#'
#' @examples
merge_custom <- function(ft, x, columns){
  z <- rle(x)
  rows_at <- cumsum(z$lengths) - z$lengths + 1

  for(i in seq_along(rows_at)){
    for(j in columns)
      ft <- flextable::merge_at(x = ft, i = seq( rows_at[i], rows_at[i] + z$lengths[i] - 1), j = j)
  }

  ft
}
