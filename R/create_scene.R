
########## create_scene #########
#todo add status?,
#' create_scene
#'
#' This function creates the Scifighter battle scene
#'
#' @param foe
#' enemy opponent
#' @param hero
#' player entity
#' @param menu
#' menu type
#' @param f.hp
#' opponent health value (max value 10)
#' @param h.hp
#' player health value (max value 10)
#'
#' @return
#' Generates a plot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_scene(foe="publisher", hero="hero", menu="menu_top_level", f.hp=10, h.hp=10)
#'}
#'

create_scene <- function(foe=NULL,hero=NULL,menu=NULL,f.hp=NULL,h.hp=NULL){

  ### init ###
  base <- system.file(package = "Scifighter")
  placeholder <- png::readPNG(paste0(base,"/extdata/img/placeholder.png"), native=T) #white place holder img
  p <- grid::rasterGrob(placeholder, interpolate=TRUE)

  # opponent corner (top right)
  if(!is.null(foe)){
    foe <- tolower(foe)
    foe_img <- paste0(base,"/extdata/img/",foe,".png")
    img.f <- png::readPNG(foe_img, native=T)
    f <- grid::rasterGrob(img.f, interpolate=TRUE)
    opponent_plot <- ggplot2::annotation_custom(f, xmin=5, xmax=10, ymin=5, ymax=9.5)
  }else{
    img.f <- placeholder
    f <- p
    opponent_plot <- ggplot2::annotation_custom(f, xmin=5, xmax=10, ymin=5, ymax=9.5)
  }

  # opponent health (mid right)
  if(!is.null(f.hp)){
    f.hp <- round(f.hp)
    f.hp_img <- paste0(base, "/extdata/img/hp/heart_", f.hp , ".png")
    img.f.hp <- png::readPNG(f.hp_img, native=T)
    fh <- grid::rasterGrob(img.f.hp, interpolate=TRUE)
    f.hp_plot <- ggplot2::annotation_custom(fh, xmin=5, xmax=10, ymin=9.5, ymax=10)
  }else{
    img.f.hp <- placeholder
    fh <- p
    f.hp_plot <- ggplot2::annotation_custom(fh, xmin=5, xmax=10, ymin=9.5, ymax=10)
  }

  # player corner (bottom left)
  if(!is.null(hero)){
    hero <- tolower(hero)
    hero_img <-paste0(base, "/extdata/img/",hero,".png")
    img.h <- png::readPNG(hero_img, native=T)
    h <- grid::rasterGrob(img.h, interpolate=TRUE)
    player_plot <- ggplot2::annotation_custom(h, xmin=0, xmax=5, ymin=0, ymax=4.5)
  }else{
    img.h <- placeholder
    h <- p
    player_plot <- ggplot2::annotation_custom(h, xmin=0, xmax=5, ymin=0, ymax=4.5)
  }

  # player health (mid left)
  if(!is.null(h.hp)){
    h.hp <- round(h.hp)
    h.hp_img <- paste0(base, "/extdata/img/hp/heart_", h.hp , ".png")
    img.h.hp <- png::readPNG(h.hp_img, native=T)
    hh <- grid::rasterGrob(img.h.hp, interpolate=TRUE)
    h.hp_plot <- ggplot2::annotation_custom(hh, xmin=0, xmax=5, ymin=4.5, ymax=5)
  }else{
    img.h.hp <- placeholder
    hh <- p
    h.hp_plot <- ggplot2::annotation_custom(hh, xmin=0, xmax=5, ymin=4.5, ymax=5)
  }

  # options menu (bottom right)
  if(!is.null(menu)){
    menu <- tolower(menu)
    menu_img <-paste0(base, "/extdata/img/",menu,".png")
    img.m <- png::readPNG(menu_img, native=T)
    m <- grid::rasterGrob(img.m, interpolate=TRUE)
    menu_plot <- ggplot2::annotation_custom(m, xmin=5, xmax=10, ymin=0, ymax=5)
  }else{
    img.m <- placeholder
    m <- p
    menu_plot <- ggplot2::annotation_custom(m, xmin=5, xmax=10, ymin=0, ymax=5)
  }

  ## canvas plotter ###
  canvas <- ggplot2::qplot(1:10, 1:10, geom="blank") +
    opponent_plot +
    f.hp_plot +
    player_plot +
    h.hp_plot +
    menu_plot +
    #geom_line(data = as.data.frame(c(1:5)), color="red")+
    ggplot2::theme_void()

  print(canvas)

}
