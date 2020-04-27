

insert_shape <- function(slide_to_insert, node){
  rm = xml2::xml_child(slide_to_insert$get(), "p:cSld/p:spTree")
  # xml_add_sibling(current_elt, new_node)
  xml2::xml_add_child(rm,
                node)
  slide_to_insert$fortify_id()$save
  return(slide_to_insert)
}
