

relaciones_gen <- function(tabla, id, rel){

  n <- nrow(tabla)

  gen_t <- lapply(1:n, function(i){

    id_ = tabla[[id]][i]
    rel_ = tabla[[rel]][i]
    rel_ = gsub('\n', '\n', rel_)
    rel_ = gsub('\r', '\n', rel_)
    rel_ = gsub(',', '\n', rel_)
    rel_ = strsplit(rel_, '\n')[[1]]
    rel_ = rel_[rel_ != '']
    rel_ = trimws(rel_)
    df = data.frame(as.character(id_), as.character(rel_))
    names(df) = c(id, rel)
    return(df)
  }) %>%
    data.table::rbindlist() %>%
    dplyr::mutate_if(is.factor, as.character)

  gen_t

}
