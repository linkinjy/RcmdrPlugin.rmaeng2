anova_menu <- function(list.info=list()){
  ##########################
  ## PREAMBLE/INFORMATION ##
  ##########################
  dialogtitle <- "ANOVA Model"
  usetabs <- TRUE
  tabnames <- c("Repeat","No Repeat")
  helppage <- "plot"

  # Do not change these lines
  if(usetabs){ntabs <- length(tabnames)} else {ntabs <- 1}
  new.frames <- .initialize.new.frames(ntabs)
  grid.config <- .initialize.grid.config(ntabs)
  grid.rows <- .initialize.grid.rows(ntabs)

  ###
  ##################
  ## GRID BUTTONS ##
  ##################
  make.help.button <- TRUE
  make.setwd.button <- FALSE
  make.resetgws.button <- FALSE
  make.seed.button <- TRUE

  ###########
  ## TAB 1 ##
  ###########

  Tab <- 1

  ### 1. ADDING THE FRAMES ###

  #### LIST BOX FRAME ####

  type <- "listbox"

  # Change variables accordingly:
  frame.name <- "listboxframe1"
  arguments <- "listboxarg"
  argument.names <- ""
  argument.values <- ""
  argument.types <- "char"
  initial.values <- ""
  length <- 4
  select.multiple <- FALSE
  title <- "Independent Variable:"
  border <- TRUE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,
                           frame.name=frame.name,argument.names=argument.names,
                           arguments=arguments,argument.values=argument.values,
                           argument.types=argument.types, initial.values=initial.values,
                           length=length,select.multiple=select.multiple,
                           title=title,border=border,new.frames=new.frames)

  #### LIST BOX FRAME ####

  type <- "listbox"

  # Change variables accordingly:
  frame.name <- "listboxframe2"
  arguments <- "listboxarg"
  argument.names <- ""
  argument.values <- ""
  argument.types <- "char"
  initial.values <- ""
  length <- 4
  select.multiple <- FALSE
  title <- "Dependent Variable:"
  border <- TRUE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,
                           frame.name=frame.name,argument.names=argument.names,
                           arguments=arguments,argument.values=argument.values,
                           argument.types=argument.types, initial.values=initial.values,
                           length=length,select.multiple=select.multiple,
                           title=title,border=border,new.frames=new.frames)


  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "tukey"
  argument.names <- c("Tukey's HSD")
  arguments <- c("tukey")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "fisher"
  argument.names <- c("Fisher's LSD")
  arguments <- c("fisher")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "bonferroni"
  argument.names <- c("Bonferroni")
  arguments <- c("bonferroni")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "scheffe"
  argument.names <- c("Scheffe")
  arguments <- c("scheffe")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "duncan"
  argument.names <- c("Duncan")
  arguments <- c("duncan")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "newmankeuls"
  argument.names <- c("Newmankeuls")
  arguments <- c("newmankeuls")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)


  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "CI"
  argument.names <- c("Confidence Interval")
  arguments <- c("CI")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "maineffect"
  argument.names <- c("Main effect")
  arguments <- c("maineffect")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "interaction"
  argument.names <- c("Interaction")
  arguments <- c("interaction")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ######		  ENTRY FIELDS FRAME 				#####
  #							    		 			#

  type <- "entryfields"

  # Change variables accordingly:
  frame.name <- "cilevel"
  argument.names <- c("CI Level")
  argument.types <- c("num")
  arguments <- c("cilevel")
  initial.values <- c(95)
  title <- ""
  border <- FALSE
  entry.width <- c("3","3","3")

  # Do not change this line:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)

  #### MANUAL BUTTONS FRAME ####

  type <- "buttons"

  # Change variables accordingly:
  frame.name <- "okframe1"
  button.name <- "Ok"
  button.function <- "buttonfunction"
  button.data <- "x"
  button.object <-  "okresult"
  button.width <- "12"
  button.data.transf <- "matrix" # only matrix available here !

  arg.frames <- c("listboxframe1","listboxframe2","tukey","fisher","bonferroni","scheffe","duncan","newmankeuls","CI","cilevel","maineffect","interaction")

  save <- TRUE
  show.save <- TRUE
  show <- TRUE
  button.otherarg <- "arg1=1"  # always start with a ,

  # Do not change this line:
  new.frames <- .add.frame(Tab=Tab,frame.name=frame.name,
                           type=type,button.name=button.name,button.width=button.width,
                           button.data.transf=button.data.transf,
                           button.function=button.function,button.data=button.data,
                           button.object=button.object,button.otherarg=button.otherarg,
                           arg.frames=arg.frames,save=save,show=show,show.save=show.save,
                           new.frames=new.frames)


  ### 2. CONFIGURING THE GRID ###
  grid.config <- .grid.matrix(Tab = Tab, c(NA,"listboxframe1","listboxframe2",NA,"tukey","fisher","bonferroni","scheffe","duncan","newmankeuls",NA,NA,"CI","cilevel","maineffect","interaction",NA,NA,NA,"okframe1"),
                              byrow=TRUE, nrow=20, ncol=4, grid.config=grid.config)
  ### 3. COMBINING THE ROWS ###
  grid.rows <- .combine.rows(Tab = Tab, rows = c(1),title = "Variale: ",
                             border = TRUE, grid.rows=grid.rows, grid.config = grid.config)
  grid.rows <- .combine.rows(Tab = Tab, rows = c(2,3),title = "Post_hoc: ",
                             border = TRUE, grid.rows=grid.rows, grid.config = grid.config)
  grid.rows <- .combine.rows(Tab = Tab, rows = c(4),title = "Option: ",
                             border = TRUE, grid.rows=grid.rows, grid.config = grid.config)

  #############
  ### TAB 2 ###
  #############
  Tab <- 2

  ### 1. ADDING THE FRAMES ###

  #### LIST BOX FRAME ####

  type <- "listbox"

  # Change variables accordingly:
  frame.name <- "listboxframe1"
  arguments <- "listboxarg"
  argument.names <- ""
  argument.values <- ""
  argument.types <- "char"
  initial.values <- ""
  length <- 4
  select.multiple <- FALSE
  title <- "Independent Variable:"
  border <- TRUE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,
                           frame.name=frame.name,argument.names=argument.names,
                           arguments=arguments,argument.values=argument.values,
                           argument.types=argument.types, initial.values=initial.values,
                           length=length,select.multiple=select.multiple,
                           title=title,border=border,new.frames=new.frames)

  #### LIST BOX FRAME ####

  type <- "listbox"

  # Change variables accordingly:
  frame.name <- "listboxframe2"
  arguments <- "listboxarg"
  argument.names <- ""
  argument.values <- ""
  argument.types <- "char"
  initial.values <- ""
  length <- 4
  select.multiple <- FALSE
  title <- "Dependent Variable:"
  border <- TRUE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,
                           frame.name=frame.name,argument.names=argument.names,
                           arguments=arguments,argument.values=argument.values,
                           argument.types=argument.types, initial.values=initial.values,
                           length=length,select.multiple=select.multiple,
                           title=title,border=border,new.frames=new.frames)


  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "tukey"
  argument.names <- c("Tukey's HSD")
  arguments <- c("tukey")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "fisher"
  argument.names <- c("Fisher's LSD")
  arguments <- c("fisher")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "bonferroni"
  argument.names <- c("Bonferroni")
  arguments <- c("bonferroni")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "scheffe"
  argument.names <- c("Scheffe")
  arguments <- c("scheffe")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "duncan"
  argument.names <- c("Duncan")
  arguments <- c("duncan")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "newmankeuls"
  argument.names <- c("Newmankeuls")
  arguments <- c("newmankeuls")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)


  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "CI"
  argument.names <- c("Confidence Interval")
  arguments <- c("CI")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "maineffect"
  argument.names <- c("Main effect")
  arguments <- c("maineffect")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ####		CHECK BOXES FRAME 			  ####
  #                               			 #

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "interaction"
  argument.names <- c("Interaction")
  arguments <- c("interaction")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(Tab=Tab,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)

  ### 2. CONFIGURING THE GRID ###
  grid.config <- .grid.matrix(Tab = Tab, c(NA,"listboxframe1","listboxframe2",NA,"tukey","fisher","bonferroni","scheffe","duncan","newmankeuls",NA,NA,"CI","maineffect","interaction"),
                              byrow=TRUE, nrow=15, ncol=4, grid.config=grid.config)
  ### 3. COMBINING THE ROWS ###
  grid.rows <- .combine.rows(Tab = Tab, rows = c(1),title = "Variale: ",
                             border = TRUE, grid.rows=grid.rows, grid.config = grid.config)
  grid.rows <- .combine.rows(Tab = Tab, rows = c(2,3),title = "Post_hoc: ",
                             border = TRUE, grid.rows=grid.rows, grid.config = grid.config)
  grid.rows <- .combine.rows(Tab = Tab, rows = c(4),title = "Option: ",
                             border = TRUE, grid.rows=grid.rows, grid.config = grid.config)

  # Repeat what you did for tab 1 for as many tabs as you like...
  ##################################################################
  ## USE ALL THE ARGUMENTS IN THE GENERAL GUI_TEMPLATE FUNCTION ##
  ##################################################################
  GUI_template(dialogtitle = dialogtitle, helppage = helppage, make.resetgws.button =
                 make.resetgws.button, make.setwd.button = make.setwd.button,
               make.help.button = make.help.button, make.seed.button = make.seed.button,
               usetabs = usetabs, tabnames = tabnames, grid.config = grid.config, grid.rows =
                 grid.rows, new.frames = new.frames)
}
