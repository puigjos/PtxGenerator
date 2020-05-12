

#' Title
#' Append two slides
#'
#' @param x rpptx object
#' @param slide slide object. Slide to Add
#' @param media_copy T if the slide object has images or svg files (icons,...)
#'
#' @return
#' @export
#'
#' @examples
append_slide <- function(x, slide, media_copy = F){

  new_slidename <- x$slide$get_new_slidename()

  file.rename(from = slide$file_name(),
              to = paste0(dirname(slide$file_name()), '/', new_slidename))

  file.copy(from = paste0(dirname(slide$file_name()), '/', new_slidename),
            to = paste0(x$package_dir, "/ppt/slides/", new_slidename))

  file.rename(from = paste0(dirname(slide$file_name()), '/_rels/',
                            basename(slide$file_name()), '.rels') ,
              to = paste0(dirname(slide$file_name()), '/_rels/', new_slidename, '.rels'))

  file.copy(paste0(dirname(slide$file_name()), '/_rels/', new_slidename, '.rels'),
            paste0(x$package_dir, "/ppt/slides/_rels/", new_slidename, '.rels'))

  ### Gen slide ########
  x$presentation$add_slide(target = file.path("slides",
                                                    new_slidename))
  x$content_type$add_slide(partname = file.path("/ppt/slides",
                                                      new_slidename))
  x$slide$add_slide(paste0(x$package_dir, "/ppt/slides/", new_slidename),
                          x$slideLayouts$get_xfrm_data())
  x$cursor = x$slide$length()
  ######################

  #### Media #####

  if(media_copy){

    media_folder = dirname(dirname(slide$file_name()))
    media_folder_from = paste0(media_folder, '/media/')

    media_folder_to = dirname(paste0(x$package_dir, '/ppt/'))
    media_folder_to = paste0(media_folder_to, '/ppt/media/')
    if(!dir.exists(media_folder_to)){
      dir.create(media_folder_to, recursive = T)
    }
    for(file in list.files(media_folder_from, full.names = T)){
      file.copy(from = file,
                to = paste0(media_folder_to, basename(file)),
                overwrite = T)
    }
  }
  x$slide$save_slides()
  x

}
