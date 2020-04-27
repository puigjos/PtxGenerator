

ph_insert_bullets <- function(x, str_list, style, level_list, ph_label){

  for(i in 1:length(str_list)){
    x = x %>% officer::ph_add_par(level=level_list[i], ph_label = ph_label) %>%
      officer::ph_add_text(str = str_list[i], ph_label = ph_label, style = style)
  }
  return(x)

}
