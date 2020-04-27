ph_add_fpar_jp <- function (x, value, type = "body", id = 1, id_chr = NULL,
          ph_label = NULL, level = 1, par_default = TRUE, pos = 'after')
{
  slide <- x$slide$get_slide(x$cursor)
  if (!is.null(id_chr)) {
    office_id <- id_chr
  }
  else {
    office_id <- officer:::get_shape_id(x, type = type, id = id, ph_label = ph_label)
  }
  current_elt <- xml2::xml_find_first(slide$get(), sprintf("p:cSld/p:spTree/*[p:nvSpPr/p:cNvPr[@id='%s']]",
                                                     office_id))
  current_p <- xml2::xml_child(current_elt, "/p:txBody")
  newp_str <- format(value, type = "pml")
  newp_str <- gsub("<a:p>", officer:::pml_with_ns("a:p"),
                   newp_str)
  if (pos == "after")
    where_ <- length(xml2::xml_children(current_p))
  else where_ <- length(xml2::xml_children(current_p)) - 1
  node <- xml2::as_xml_document(newp_str)
  if (par_default) {
    ppr <- xml2::xml_child(node, "/a:pPr")
    empty_par <- xml2::as_xml_document(paste0(officer:::pml_with_ns("a:pPr"),
                                        "</a:pPr>"))
    xml2::xml_replace(ppr, empty_par)
  }
  ppr <- xml2::xml_child(node, "/a:pPr")
  if (level > 1) {
    xml2::xml_attr(ppr, "lvl") <- sprintf("%.0f", level -
                                      1)
  }
  if (inherits(current_p, "xml_missing")) {
    simple_shape <- paste0(officer:::pml_with_ns("p:txBody"),
                           "<a:bodyPr/><a:lstStyle/></p:txBody>")
    newnode <- xml2::as_xml_document(simple_shape)
    xml2::xml_add_child(newnode, node)
    xml2::xml_add_child(current_elt, newnode)
  }
  else {
    xml2::xml_add_child(current_p, node, .where = where_)
  }
  x
}
