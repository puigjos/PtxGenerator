

get_xml_id <- function(x, id){

  stopifnot('slide' %in% class(x))

  new_node = xml2::xml_find_first(x$get(),
                 sprintf("p:cSld/p:spTree/*[p:nvGrpSpPr/p:cNvPr[@id='%s']]",
                         id))

  if(class(new_node) == "xml_missing"){
    new_node = xml2::xml_find_first(x$get(),
                              sprintf("p:cSld/p:spTree/*[p:nvSpPr/p:cNvPr[@id='%s']]",
                                      id))
  }
  if(class(new_node) == 'xml_missing'){
    new_node = xml2::xml_find_first(x$get(),
                              sprintf("p:cSld/p:spTree/*[p:nvCxnSpPr/p:cNvPr[@id='%s']]",
                                      id))
  }

  return(new_node)
}
