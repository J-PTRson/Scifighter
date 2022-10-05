####### end_scene ######



#' end_scene
#'
#' This function generates the post-battle end scene
#'
#' @param entity
#' winner of the match
#'
#' @return
#' Displays a plot
#' @export
#'
#' @examples
#' \dontrun{
#' end_scene("hero")
#'}
#'
end_scene <- function(entity=NULL){

  ### init ###
  base <- system.file(package = "Scifighter")
  placeholder <- png::readPNG(paste0(base,"/extdata/img/placeholder.png"), native=T) #white place holder img
  p <- grid::rasterGrob(placeholder, interpolate=TRUE)

  # match winner
  if(!is.null(entity)){
    entity <- tolower(entity)
    entity_img <- paste0(base,"/extdata/img/",entity,".png")
    img.e <- png::readPNG(entity_img, native=T)
    e <- grid::rasterGrob(img.e, interpolate=TRUE)
    winner_plot <- ggplot2::annotation_custom(e, xmin=3, xmax=7, ymin=2.5, ymax=5.8)
  }else{
    img.e <- placeholder
    e <- p
    winner_plot <- ggplot2::annotation_custom(e, xmin=3, xmax=7, ymin=2.5, ymax=5.8)
  }

  #Win frame
  win.frame_img <- paste0(base,"/extdata/img/win_frame.png")
  img.wf <- png::readPNG(win.frame_img, native=T)
  wf <- grid::rasterGrob(img.wf, interpolate=TRUE)
  winframe_plot <- ggplot2::annotation_custom(wf, xmin=0, xmax=10, ymin=-1, ymax=9)

  #Win text
  win.text_img <- paste0(base,"/extdata/img/win_text.png")
  img.wt <- png::readPNG(win.text_img, native=T)
  wt <- grid::rasterGrob(img.wt, interpolate=TRUE)
  wintext_plot <- ggplot2::annotation_custom(wt, xmin=0, xmax=10, ymin=8.2, ymax=11)


  ## canvas plotter ###
  canvas <- ggplot2::qplot(1:10, 1:10, geom="blank") +
    winframe_plot +
    wintext_plot +
    winner_plot +
    #geom_line(data = as.data.frame(c(1:5)), color="red")+
    ggplot2::theme_void()

  print(canvas)

}
