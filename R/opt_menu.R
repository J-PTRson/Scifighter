##########  opt_menu ##############
#c.opt expects a character vector

#' opt_menu
#'
#' This function provides a number of options to the user and allows for menu interactions.
#'
#' @param c.opt
#' options from which the user can choose (in capitals!)
#'
#' @return
#' Returns a selected option
#' @export
#'
#' @examples
#' \dontrun{
#' options <- toupper(c("yes","no"))
#' opt_menu(options)
#' }
#'
opt_menu <- function(c.opt=NULL) {
  if(!is.null(c.opt)){

  }else{
    base <- system.file(package = "Scifighter")
    c.opt <- read.table(paste0(base,"/extdata/table/menu_options.txt"), sep=" ", header = T)
    c.opt <- colnames(c.opt)
  }

  pass <- FALSE

  while (pass != TRUE) {
    c.text <- paste0("What do you do?: (", toString(c.opt), ") ")
    #paste0(colnames(h.stat[[5]]),collapse=" ")

    choice <- toupper(as.character(readline(prompt=c.text)))

    for (n in sequence(length(c.opt))){
      if (choice == c.opt[n]) {
        print(paste0("You've selected ", c.opt[n],"!"))
        pass <- TRUE
        break
      }else if (n == length(c.opt)){
        #list depleted and no match found.
        print("Invalid input, please try again!")
      }
    }
  }

  return(choice)

}
