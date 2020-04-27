

change_size <- function(node, x, y, scale = F){

  current_al = xml2::xml_find_all(node, './/a:xfrm')

  for (i in 1:length(current_al)){
    current_p <- xml2::xml_child(current_al[[i]], "a:ext")
    if(scale){
      X = floor(as.numeric(xml2::xml_attr(current_p, 'cx')) * x)
      xml2::xml_attr(current_p, 'cx') = as.character(X)
      Y = floor(as.numeric(xml2::xml_attr(current_p, 'cy')) * y)
      xml2::xml_attr(current_p, 'cy') = as.character(Y)
    }else{
      xml2::xml_attr(current_p, 'cx') = as.character(floor(as.numeric(x)))
      xml2::xml_attr(current_p, 'cy') = as.character(floor(as.numeric(y)))
    }
  }
  return(node)
}

change_position <- function(node, x, y){

  current_al = xml2::xml_find_all(node, './/a:xfrm')

  for (i in 1:length(current_al)){
    current_p <- xml2::xml_child(current_al[[1]], "a:off")
    xml2::xml_attr(current_p, 'x') = as.character(floor(as.numeric(x)))
    xml2::xml_attr(current_p, 'y') = as.character(floor(as.numeric(y)))
  }
  return(node)
}
