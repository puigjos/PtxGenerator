

set_widths_flextable <- function(x, widths){

  for (w in seq_along(widths)){
    x = x %>%
      flextable::width(j=w, width = widths[w]*inches)
  }
  return(x)
}
