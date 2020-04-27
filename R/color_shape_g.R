color_shape_g <- function(slide, office_id, fill = NULL,
                        border = NULL){
  current_elt <- get_xml_id(slide, id = office_id)
  # fill ---------------------------------------------------------------------------
    # No fill
    # rm = xml_child(current_elt, "p:spPr/a:solidFill/a:srgbClr")
    # xml_remove(rm)
    # current_p <- xml_child(current_elt, "p:spPr/a:solidFill/a:schemeClr")
    # if(class(current_p) == "xml_missing"){
    #   current_p <- xml_child(current_elt, "p:spPr/a:solidFill")
    #   xml_add_child(current_p, 'a:schemeClr')
    #   current_p <- xml_child(current_elt, "p:spPr/a:solidFill/a:schemeClr")
    #   xml_attr(current_p, 'val') = 'bg1'
    # }
    #
  # fill ----------------------------------------------------------------
  current_al = xml2::xml_find_all(current_elt, './/p:spPr')
  if(!is.null(fill)){
    for(jj in 1:length(current_al)){
      current_p <- xml2::xml_child(current_al[[jj]], "a:solidFill/a:srgbClr")
      if(class(current_p) == 'xml_missing'){
        current_p <- xml2::xml_child(current_al[[jj]], "a:solidFill")
        if(class(current_p) == 'xml_missing'){
          xml2::xml_add_child(current_al[[jj]], 'a:solidFill', .where = 2)
          rm = xml2::xml_child(current_al[[jj]], 'a:noFill')
          xml2::xml_remove(rm)
          current_p <- xml2::xml_child(current_al[[jj]], "a:solidFill")
        }
        rm = xml2::xml_child(current_p, "a:schemeClr")
        xml2::xml_remove(rm)
        xml2::xml_add_child(current_p, 'a:srgbClr')
        current_p <- xml2::xml_child(current_al[[jj]], "a:solidFill/a:srgbClr")
      }
      xml2::xml_attr(current_p, 'val') = fill
    }
  }

  # border ----------------------------------------------------------------
  current_al = xml2::xml_find_all(current_elt, './/p:spPr/a:ln/a:solidFill')

  if(!is.null(border)){
    for(jj in 1:length(current_al)){
      current_p <- xml2::xml_child(current_al[[jj]], "a:srgbClr")
      if(class(current_p) == 'xml_missing'){
        rm = xml2::xml_child(current_al[[jj]], "a:schemeClr")
        xml2::xml_remove(rm)
        xml2::xml_add_child(current_al[[jj]], 'a:srgbClr')
        current_p <- xml2::xml_child(current_al[[jj]], "a:srgbClr")
      }
      xml2::xml_attr(current_p, 'val') = border
    }
  }

  return(slide)
}
