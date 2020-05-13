
remove_slides <- function(x, index){
  n  = length(index)
  for (i in 1:n){
    x = officer::remove_slide(x, index = index[i])
    index = index - 1
  }
  return(x)

}


select_slides <- function(x, index){
  numb_slides = seq_along(x)
  rem_slides <- numb_slides[!numb_slides %in% index]
  if(length(rem_slides) == 0){
  }else{
    remove_slides(x, index = rem_slides)
  }

}
