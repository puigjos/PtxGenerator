

get_slide_summary <- function(slide){
  nodes <- xml2::xml_find_all(slide$get(), officer:::as_xpath_content_sel("p:cSld/p:spTree/"))
  data <- officer:::read_xfrm(nodes, file = "slide", name = "")
  data$text <- sapply(nodes, xml2::xml_text)
  data[["offx"]] <- data[["offx"]]/914400
  data[["offy"]] <- data[["offy"]]/914400
  data[["cx"]] <- data[["cx"]]/914400
  data[["cy"]] <- data[["cy"]]/914400
  data$name <- NULL
  data$file <- NULL
  data$ph <- NULL
  data
}
