########## attack #############
#Todo, could add some spicy dmg modifiers

#' attack
#'
#' This function retrieves the damage values from the character.json file for a corresponding attack.
#'
#' @param entity
#' a character entity
#' @param attack
#' a attack option
#'
#' @return
#' Returns a damage value corresponding to the attack
#' @export
#'
#' @examples
#' attack("hero","punch")
#'
attack <- function(entity,attack){
  attack <- tolower(attack)
  e.stat <- get_stats(entity)
  for (n in 1:length(e.stat[[5]])){
    if(attack == colnames(e.stat[[5]][n])){
      dmg <- as.integer(e.stat[[5]][n])
      break
    }else if(n == length(e.stat[[5]])) {
      print("Illegal attack!")
      dmg <- 0
    }
  }

  return(dmg)

}
