############ Encounter #################
#encounter(antagonist,protagonist)
#foe & hero expect the name of the character

#' encounter
#'
#' This functions initiates the encounter
#'
#' @param foe
#' opponent entity
#' @param hero
#' player entity
#'
#' @return
#' This function returns the match results
#' @export
#'
#' @examples
#' \dontrun{
#' encounter(foe="funder", hero="hero")
#'}
#'
encounter <- function(foe,hero) {


  ##### init #####
  base <- system.file(package = "Scifighter")

  foe <- tolower(foe)
  hero <- tolower(hero)

  f.stat <- get_stats(foe)
  h.stat <- get_stats(hero)

  f.hp <- as.integer(get_stats(foe)[[2]])
  h.hp <- as.integer(get_stats(hero)[[2]])

  win <- FALSE

  ##### f.stat ####
  #f.stat[1] : name
  #f.stat[2] : health
  #f.stat[3] : status
  #f.stat[4] : vulnerable
  #f.stat[5] : attack


  #intro opponent
  f.msg <- paste0("A wild ", toupper(f.stat[1]), " appears!")
  print(f.msg)
  #create_scene(foe,f.hp=f.hp)

  #break
  #Sys.sleep(1.5)

  #intro player
  h.msg <- paste0("You choose ", toupper(h.stat[1]),"!")
  print(h.msg)
  #create_scene(foe,hero,f.hp=f.hp,h.hp=h.hp)

  #break
  Sys.sleep(1.5)

  #gameloop
  while(win==F){

    #call to action
    menu <- "menu_top_level"
    create_scene(foe,hero,menu,f.hp,h.hp)
    choice <- opt_menu()

    #action_1
    if(choice == "FIGHT"){
      print("Choose an attack:")
      a.opt <- toupper(colnames(h.stat[[5]]))
      a.choice <- opt_menu(a.opt)
      a.dmg <- attack(hero,a.choice)
      f.hp <- f.hp - a.dmg
      if(f.hp <= 0){
        f.hp <- NULL
        }else if(h.hp <= 0){
          h.hp <- NULL
        }
      a.msg <- paste0("Your attack did a whopping ", a.dmg, " dmg! Your opponnents remaining HP is: ", f.hp)
      print(a.msg)
      beepr::beep(7)
      Sys.sleep(1)
      create_scene(foe,hero,menu,f.hp,h.hp)
      #break
      Sys.sleep(1.5)
      counter <- turnend(foe,f.hp) #initiate opponents turn
    }

    #action_2
    if(choice =="PKMN"){
      print("Choose your character!")
      b.opt <- read.table(paste0(base,"/extdata/table/pkmn.txt"), sep=" ", header = T)
      b.opt <- colnames(b.opt)
      b.choice <- opt_menu(b.opt)
      b.msg <- paste0("Go get em ", b.choice, "!")
      print(b.msg)
      hero <- tolower(b.choice)
      h.stat <- get_stats(hero)
      h.hp <- as.integer(get_stats(hero)[[2]])
      create_scene(foe,hero,menu,f.hp,h.hp)
      beepr::beep(5)
      Sys.sleep(1)
      #break
      Sys.sleep(1.5)
      counter <- turnend(foe,f.hp) #initiate opponents turn
    }

    #action_3
    if(choice =="ITEM"){
      print("Choose your item!")
      c.opt <- read.table(paste0(base,"/extdata/table/item.txt"), sep=" ", header = T)
      c.choice <- opt_menu(colnames(c.opt))
      c.msg <- paste0("You've used ", c.choice, "!")
      print(c.msg)
      if(c.choice=="HP.POTION"){
        h.hp <- h.hp + as.integer(c.opt$HP.POTION)
        if(h.hp > 10){
          h.hp <- 10
        }
      }
      #todo add status clearing
      beepr::beep(2)
      Sys.sleep(1)
      create_scene(foe,hero,menu,f.hp,h.hp)
      #break
      Sys.sleep(1.5)
      counter <- turnend(foe,f.hp) #initiate opponents turn
    }

    #action_4
    if(choice =="RUN"){
      print("Choose to run away!")
      counter <- "run"
    }

    #end game conditions
    if(counter == "Game END"){
      win <- "win"
      w.msg <- paste0("You've succesfully defeated: ", toupper(foe))
      print(w.msg)
      beepr::beep(8)
      Sys.sleep(5)
      #break
      Sys.sleep(1.5)
      end_scene(hero)
    }else if(counter == "run"){
      win <- "lose"
      l.msg <- paste0("You've ran away from: ", toupper(foe))
      print(l.msg)
      #break
      Sys.sleep(1.5)
      beepr::beep(3)
      Sys.sleep(1)
      end_scene(foe)
    }else {
      #AI turn
      f.dmg <- as.integer(counter)
      f.attack <- toupper(colnames(counter))
      h.hp <- h.hp - f.dmg
      if(f.hp <= 0){
        f.hp <- NULL
      }else if(h.hp <= 0){
        h.hp <- NULL
      }
      c.msg <- paste0(toupper(foe), " shrugss and uses ", f.attack ,". The attack did ", f.dmg, " dmg! Your remaining HP is: ", h.hp )
      print(c.msg)
      #break
      Sys.sleep(1.5)
      beepr::beep(9)
      Sys.sleep(1)
      create_scene(foe,hero,menu,f.hp,h.hp)
    }

    #Lose condition
    if(is.null(h.hp)){
      win <- "lose"
      l.msg <- paste0("You've lost the fight from: ", toupper(foe))
      print(l.msg)
      #break
      Sys.sleep(1.5)
      beepr::beep(3)
      Sys.sleep(3)
      end_scene(foe)
    }
  }
  return(win)
}

########## turnend / ai_opponent ########
## ai opponent will always try to attack back

turnend <- function(foe,hp){
  if (is.null(hp)) {
    #End GAME
    counter <- "Game END"
  }else{
    f.stat <- get_stats(foe)
    attack <- sample(1:length(f.stat[[5]]),1) #selects a random attack
    counter <- f.stat[[5]][attack]
  }
 return(counter)
}





















