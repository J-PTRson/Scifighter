########## get_stats ##############
# get_stats expects a entity name as input and returns all information related to that entity

#' get_stats
#'
#' This function retrieves the character stats corresponding to the entity.
#'
#' @param x
#' entity name
#'
#' @return
#' Returns all stats of the corresponding entity.
#' @export
#'
#' @examples
#' get_stats("phd.candidate")
#'
get_stats <- function(x){

  #import .json
  base <- system.file(package = "Scifighter")
  entity <- jsonlite::fromJSON(paste0(base,"/extdata/table/characters.json"))

  #check existence of key
  if(is.na(match(x,entity$name))){
    stop("Error: entity does not exist!")
  }else{
    index <- match(x,entity$name) #get index of entity
  }

  #create list of info
  stats <- c(entity$name[index], entity$health[index],entity$status[index],entity$vulnerable[index],entity$attack[index])

  return(stats)

}
