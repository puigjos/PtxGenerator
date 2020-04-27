

gen_flex_table_summary <- function(tabla,
                                   name = 'Iniciativas',
                                   group_var = c('Categoría', 'Subcategoría'),
                                   value_var = 'Prioridad',
                                   fontname = "Ubuntu (Body)",
                                   border_color  ="grey"
                                   ){
  df = tabla %>%
    dplyr::select(group_var, value_var)  %>%
    dplyr::group_by_at(c(group_var, value_var))  %>%
    dplyr::count()
  if(length(group_var) == 2){
    df = df %>%
      dplyr::group_by_at(group_var) %>%
      dplyr::mutate(
        !!rlang::enquo(name) := sum(n)
      )
  }
  v = paste0('Total \n', name)
  df = df %>%
    dplyr::group_by_at(group_var[1]) %>%
    dplyr::mutate(
      !!rlang::enquo(v) := sum(n)
    ) %>%
    tidyr::spread(key = value_var, value = n) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, function(x){
      ifelse(is.na(x), 0, x)
    }) %>%
    dplyr::mutate_if(is.numeric, function(x){
      ifelse(x==0, '-', as.character(x))
    })
  nm = names(df)
  df = df[, c(which(nm != v), which(nm == v)) ]

  flx <- df %>%
    flextable::flextable() %>%
    flextable::merge_v(j = 1) %>% merge_custom(columns = ncol(df), x = df[[group_var[1]]]) %>%
    flextable::add_header_row(values = c(rep('', length(group_var)),
                              ifelse(length(group_var) == 1, NULL, name),
                              rep(value_var, length(unique(tabla[[value_var]]))),
                              v) ) %>%
    flextable::merge_h(part = 'header') %>%
    flextable::merge_v(part = 'header') %>%
    flextable::align(part = 'header', align = 'center') %>%
    flextable::align(j = 1:length(group_var), align = 'left') %>%
    flextable::align(j = (length(group_var)+1):ncol(df), align = 'center') %>%
    flextable::valign(valign = 'center') %>%
    flextable::border_remove() %>%
    flextable::border_inner(border = officer::fp_border(color = border_color)) %>%
    flextable::border_outer(border = officer::fp_border(color = border_color)) %>%
    flextable::bold(part = 'header') %>%
    flextable::font(fontname = fontname)
  flx
}
