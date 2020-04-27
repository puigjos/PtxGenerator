color_shape <- function(slide, office_id, fill = NULL,
                        border = NULL){
  # slide <- x$slide$get_slide(x$cursor)
  # current_elt <- xml_find_first(slide$get(),
  #                               sprintf("p:cSld/p:spTree/*[p:nvSpPr/p:cNvPr[@id='%s']]",
  #                                                    office_id))
  current_elt <- get_xml_id(slide, id = office_id)
  # fill ---------------------------------------------------------------------------
  if(!is.null(fill)){
    # yes fill
    current_p <- xml2::xml_child(current_elt, "p:spPr/a:solidFill/a:srgbClr")
    rm = xml2::xml_child(current_elt, "p:spPr/a:noFill")
    xml2::xml_remove(rm)
    if(class(current_p) == "xml_missing"){
      current_p <- xml2::xml_child(current_elt, "p:spPr/a:solidFill")
      if(class(current_p) == 'xml_missing'){
        current_p <- xml2::xml_child(current_elt, "p:spPr")
        xml2::xml_add_child(current_p, 'a:solidFill', .where = 2)
        current_p <- xml2::xml_child(current_elt, "p:spPr/a:solidFill")
      }
      rm = xml2::xml_child(current_elt, "p:spPr/a:solidFill/a:schemeClr")
      xml2::xml_remove(rm)
      xml2::xml_add_child(current_p, 'a:srgbClr')
      current_p <- xml2::xml_child(current_elt, "p:spPr/a:solidFill/a:srgbClr")
    }
    current_p <- xml2::xml_child(current_elt, "p:spPr/a:solidFill/a:srgbClr")
    xml2::xml_attr(current_p, 'val') = fill
  }else{
    # No fill
    rm = xml2::xml_child(current_elt, "p:spPr/a:solidFill/a:srgbClr")
    xml2::xml_remove(rm)
    current_p <- xml2::xml_child(current_elt, "p:spPr/a:solidFill/a:schemeClr")
    if(class(current_p) == "xml_missing"){
      current_p <- xml2::xml_child(current_elt, "p:spPr/a:solidFill")
      xml2::xml_add_child(current_p, 'a:schemeClr')
      current_p <- xml2::xml_child(current_elt, "p:spPr/a:solidFill/a:schemeClr")
      xml2::xml_attr(current_p, 'val') = 'bg1'
    }
  }
  # border ----------------------------------------------------------------

  if(!is.null(border)){
    current_p <- xml2::xml_child(current_elt, "p:spPr/a:ln/a:solidFill/a:srgbClr")
    if(class(current_p) == "xml_missing"){
      current_p <- xml2::xml_child(current_elt, "p:spPr/a:ln/a:solidFill/a:srgbClr")
      if(class(current_p) == 'xml_missing'){
        current_p <- xml2::xml_child(current_elt, "p:spPr/a:ln/")
        xml2::xml_add_child(current_p, 'a:solidFill', .where = 2)
        current_p <- xml2::xml_child(current_elt, "p:spPr/a:solidFill")
      }
      rm = xml2::xml_child(current_elt, "p:spPr/a:ln/a:solidFill/a:schemeClr")
      xml2::xml_remove(rm)
      xml2::xml_add_child(current_p, 'a:srgbClr')
      current_p <- xml2::xml_child(current_elt, "p:spPr/a:ln/a:solidFill/a:srgbClr")
    }
    current_p <- xml2::xml_child(current_elt, "p:spPr/a:ln/a:solidFill/a:srgbClr")
    xml2::xml_attr(current_p, 'val') = border
  }



  return(slide)
}

