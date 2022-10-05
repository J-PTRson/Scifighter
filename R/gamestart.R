#' gamestart
#'
#' This function is required to start the game.
#'
#' @return
#' Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' gamestart()
#' }
#'
gamestart <- function() {
  continue <- TRUE

  while(isTRUE(continue)){

    base <- system.file(package = "Scifighter")
    print("Choose your fighter!")
    s.opt <- read.table(paste0(base,"/extdata/table/pkmn.txt"), sep=" ", header = T)
    s.opt <- colnames(s.opt)
    s.choice <- opt_menu(s.opt)
    hero <- tolower(s.choice)
    beepr::beep(6)
    Sys.sleep(1)

    possible_enemies <- c("publisher","funder","rival","reviewer2")
    #Free for all
    #possible_enemies <- c("publisher","funder","rival","reviewer2","pi","master.student","bioinformatician","data.steward","technician","postdoc","phd.candidate")
    select <- sample(1:length(possible_enemies),1) #selects a random opponent

    foe <- possible_enemies[select]

    battle <- encounter(foe,hero)
    f.msg <- "Do you want to continue? YES/NO "
    continue <- toupper(as.character(readline(prompt=f.msg)))
    if (continue != "YES"){
      continue <- FALSE
    }else {
      continue <- TRUE
    }
    print("Thank you for playing!")
  }
}
