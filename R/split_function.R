

split_function <- function(x, split){
  x %>%
    lapply(function(x){
      x = gsub(split, '', x)
      x = gsub('–', '-', x)
      x = strsplit(x, '-')[[1]] %>% as.numeric()
      if(length(x) == 1){
        x = rep(x, 2)
      }
      as.data.frame(t(x))
    }) %>% data.table::rbindlist()
}
