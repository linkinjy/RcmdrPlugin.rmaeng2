anova_WINDOW <- function(){     # Change newmethod to your own method name

  new.frames <- .initialize.new.frames()
  grid.config <- .initialize.grid.config()
  grid.rows <- .initialize.grid.rows()


  #####################################################
  ## GENERAL INFORMATION ABOUT THE NEW METHOD/WINDOW ##
  #####################################################

  methodname <- "ANOVA"

  methodfunction <- "main.effect"
  data.arg <- "data, fac, y"
  methodshow <- TRUE
  methodsave <- TRUE
  other.arg <- ""
  methodhelp <- ""

  # Transform the data from data.arg
  data.transf <- "matrix" # Values: "matrix" (default), "ExprSet"

  # Extra Data Conversion Boxes
  data.discr <- FALSE
  data.bin <- FALSE

  # Possibility to give a seed ?
  methodseed <- TRUE

  ## COMPATIBILITY? ##

  # BcDiag
  bcdiag.comp <- FALSE

  # SuperBiclust
  superbiclust.comp <- FALSE


  ########################
  #### CLUSTERING TAB ####
  ########################

  input <- "repetition"

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
  new.frames <- .add.frame(input=input,type=type,
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
  new.frames <- .add.frame(input=input,type=type,
                           frame.name=frame.name,argument.names=argument.names,
                           arguments=arguments,argument.values=argument.values,
                           argument.types=argument.types, initial.values=initial.values,
                           length=length,select.multiple=select.multiple,
                           title=title,border=border,new.frames=new.frames)

  #### CHECK BOXES FRAME  ####

  type <- "checkboxes"

  # Change variables accordingly:
  frame.name <-  "tukey"
  argument.names <- c("Tukey's HSD")
  arguments <- c("tukey")
  initial.values <- c(0)
  title <- ""
  border <- FALSE

  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,new.frames=new.frames)

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
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,new.frames=new.frames)

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
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,new.frames=new.frames)

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
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,new.frames=new.frames)

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
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,new.frames=new.frames)

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
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,new.frames=new.frames)


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
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,new.frames=new.frames)

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
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,new.frames=new.frames)

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

  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,new.frames=new.frames)

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
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,entry.width=entry.width
                           ,argument.types=argument.types  ,new.frames=new.frames)

  ### 2. CONFIGURING THE GRID ###

  grid.config <- .grid.matrix(input=input, c(NA,"listboxframe1","listboxframe2",NA,"tukey","fisher","bonferroni","scheffe","duncan","newmankeuls",NA,NA,"CI","maineffect","interaction"),
                              byrow=TRUE, nrow=15, ncol=4, grid.config=grid.config)

  ### 3. COMBING THE ROWS ###

  grid.rows <- .combine.rows(input = input, rows = c(1),title = "Variale: ",
                             border = TRUE, grid.rows=grid.rows, grid.config = grid.config)
  grid.rows <- .combine.rows(input = input, rows = c(2,3),title = "Post_hoc: ",
                             border = TRUE, grid.rows=grid.rows, grid.config = grid.config)
  grid.rows <- .combine.rows(input = input, rows = c(4),title = "Option: ",
                             border = TRUE, grid.rows=grid.rows, grid.config = grid.config)



  ####################################
  #### PLOTTING & DIAGNOSTICS TAB ####
  ####################################

  input <- "no repetition"

  ### 1. ADDING THE FRAMES ###

  # Add frames here

  ### 2. CONFIGURING THE GRID ###

  grid.config <- .grid.matrix(input=input,c("frame5","frame6"),nrow=1,ncol=2,byrow=TRUE,grid.config=grid.config)

  ### 3. COMBING THE ROWS ###

  grid.rows <- .combine.rows(input=input,rows=c(1),title="Plot 1",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)

  ###################################################################
  ## USE ALL THE ARGUMENTS IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
  ###################################################################

  cluster_template(methodname=methodname,methodfunction=methodfunction,methodhelp=methodhelp,data.arg=data.arg,other.arg=other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames,superbiclust.comp=superbiclust.comp,bcdiag.comp=bcdiag.comp,data.transf=data.transf,data.discr=data.discr,data.bin=data.bin,methodshow=methodshow,methodsave=methodsave)

}
