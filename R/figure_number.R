

figure_number <- function(slide, GeneralName, colors, 
                          blank_colors, number){
  sl_sm <- get_slide_summary(slide)
  n = length(colors)
  j = 0
  p_nm = paste0(GeneralName, 1:n)
  
  id_ <- sl_sm$id[sl_sm$ph_label %in% p_nm]
  
  for(i in 1:n){
    
    if(number == 0){
      j = 1
    }
    if(j == 1){
      color_shape(slide = slide, office_id = id_[i], fill = blank_colors)
      next
    }
    color_shape(slide = slide, office_id = id_[i], fill = colors[i])
    if(number == i){
      j = 1
    }
  }
  slide$fortify_id()$save()
  return(slide)
}

