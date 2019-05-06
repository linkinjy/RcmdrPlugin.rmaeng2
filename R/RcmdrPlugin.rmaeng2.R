simulateConfidenceIntervals <- function(){
  defaults <- list(muVar="100", sigmaVar="15", nVar="25", repsVar="50", confLevelVar=".95", sigmaKnownVar="1")
  dialog.values <- getDialog("simulateConfidenceIntervals", defaults)
  initializeDialog(title=gettextRcmdr("Confidence Intervals for the Mean"))
  muVar <- tclVar(dialog.values$muVar)
  muEntry <- tkentry(top, width="6", textvariable=muVar)
  sigmaVar <- tclVar(dialog.values$sigmaVar)
  sigmaEntry <- tkentry(top, width="6", textvariable=sigmaVar)
  nVar <- tclVar(dialog.values$nVar)
  nEntry <- tkentry(top, width="6", textvariable=nVar)
  repsVar <- tclVar(dialog.values$repsVar)
  repsEntry <- tkentry(top, width="6", textvariable=repsVar)
  confLevelVar <- tclVar(dialog.values$confLevelVar)
  confLevelEntry <- tkentry(top, width="6", textvariable=confLevelVar)
  sigmaKnownVar <- tclVar(dialog.values$sigmaKnownVar)
  sigmaKnownBox <- tkcheckbutton(top, variable=sigmaKnownVar)
  onOK <- function(){
    closeDialog()
    mu <- as.numeric(tclvalue(muVar))
    if (is.na(mu)){
      errorCondition(recall=simulateConfidenceIntervals, message="Population mean must be a number.")
      return()
    }
    sigma <- as.numeric(tclvalue(sigmaVar))
    if (is.na(sigma) || sigma <= 0){
      errorCondition(recall=simulateConfidenceIntervals,
                     message="Population standard deviation must be a positive number.")
      return()
    }
    n <- round(as.numeric(tclvalue(nVar)))
    if (is.na(n) || n <= 0){
      errorCondition(recall=simulateConfidenceIntervals, message="Sample size must be a positive integer.")
      return()
    }
    reps <- round(as.numeric(tclvalue(repsVar)))
    if (is.na(reps) || reps <= 0){
      errorCondition(recall=simulateConfidenceIntervals, message="Number of samples must be a positive integer.")
      return()
    }
    confLevel <- as.numeric(tclvalue(confLevelVar))
    if (is.na(confLevel) || confLevel <= 0 || confLevel >= 1){
      errorCondition(recall=simulateConfidenceIntervals, message="Confidence level must be between 0 and 1.")
      return()
    }
    sigmaKnown <- as.numeric(tclvalue(sigmaKnownVar))
    putDialog("simulateConfidenceIntervals", lapply(list(muVar=mu, sigmaVar=sigma, nVar=n, repsVar=reps,
                                                         confLevelVar=confLevel, sigmaKnownVar=sigmaKnown), as.character))
    command <- paste("ci.examp(mean.sim = ", mu, ", sd = ", sigma, ", n = ", n, ", reps = ", reps,
                     ", conf.level = ", confLevel, ", method = ", if (sigmaKnown) '"z"' else '"t"', ")", sep="")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="ci.examp", reset="simulateConfidenceIntervals", apply="simulateConfidenceIntervals")
  tkgrid(tklabel(top, text="Population mean"), muEntry, sticky="e")
  tkgrid(tklabel(top, text="Population standard deviation"), sigmaEntry, sticky="e")
  tkgrid(tklabel(top, text="Sample size"), nEntry, sticky="e")
  tkgrid(tklabel(top, text="Number of samples"), repsEntry, sticky="e")
  tkgrid(tklabel(top, text="Confidence level"), confLevelEntry, sticky="e")
  tkgrid(tklabel(top, text="Population standard deviation known"), sigmaKnownBox, sticky="e")
  tkgrid(buttonsFrame, sticky="w", columnspan=2)
  tkgrid.configure(muEntry, sticky="w")
  tkgrid.configure(sigmaEntry, sticky="w")
  tkgrid.configure(nEntry, sticky="w")
  tkgrid.configure(repsEntry, sticky="w")
  tkgrid.configure(confLevelEntry, sticky="w")
  tkgrid.configure(sigmaKnownBox, sticky="w")
  dialogSuffix(rows=7, columns=2, focus=muEntry)
}

centralLimitTheorem <- function(){
  Library("TeachingDemos")
  initializeDialog(title=gettextRcmdr("Central Limit Theorem"))
  defaults <- list(nVar="1", repsVar=10000, nclassVar="16")
  dialog.values <- getDialog("centralLimitTheorem", defaults)
  nVar <- tclVar(dialog.values$nVar)
  nEntry <- tkentry(top, width="6", textvariable=nVar)
  repsVar <- tclVar(dialog.values$repsVar)
  repsEntry <- tkentry(top, width="6", textvariable=repsVar)
  nclassVar <- tclVar(dialog.values$nclassVar)
  nclassEntry <- tkentry(top, width="6", textvariable=nclassVar)
  onOK <- function(){
    closeDialog()
    n <- round(as.numeric(tclvalue(nVar)))
    if (is.na(n) || n <= 0){
      errorCondition(recall=simulateConfidenceIntervals, message="Sample size must be a positive integer.")
      return()
    }
    reps <- round(as.numeric(tclvalue(repsVar)))
    if (is.na(reps) || reps <= 0){
      errorCondition(recall=simulateConfidenceIntervals, message="Number of samples must be a positive integer.")
      return()
    }
    nclass <- round(as.numeric(tclvalue(nclassVar)))
    if (is.na(nclass) || reps <= 0){
      errorCondition(recall=simulateConfidenceIntervals, message="Number of samples must be a positive integer.")
      return()
    }
    putDialog("centralLimitTheorem", lapply(list(nVar=n, repsVar=reps, nclassVar=nclass), as.character))
    command <- paste("clt.examp(n = ", n, ", reps = ", reps, ", nclass =", nclass, ")", sep="")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="clt.examp", reset="centralLimitTheorem", apply="centralLimitTheorem")
  tkgrid(tklabel(top, text="Sample size"), nEntry, sticky="e")
  tkgrid(tklabel(top, text="Number of samples"), repsEntry, sticky="e")
  tkgrid(tklabel(top, text="Approximate number of bins for histograms"), nclassEntry, sticky="e")
  tkgrid(buttonsFrame, sticky="w", columnspan=2)
  tkgrid.configure(nEntry, sticky="w")
  tkgrid.configure(repsEntry, sticky="w")
  tkgrid.configure(nclassEntry, sticky="w")
  dialogSuffix(rows=4, columns=2, focus=nEntry)
}

flipCoin <- function(){
  rgl::rgl.open()
  TeachingDemos::rgl.coin()
  TeachingDemos::flip.rgl.coin()
}

rollDie <- function(){
  rgl::rgl.open()
  TeachingDemos::rgl.die()
  TeachingDemos::roll.rgl.die()
}

powerExample <- function(){
  TeachingDemos::power.examp()
  TeachingDemos::run.power.examp()
}

correlationExample <- function(){
  TeachingDemos::run.cor.examp()
}

linearRegressionExample <- function(){
  put.points.demo()
}

visBinom <- function(){
  TeachingDemos::vis.binom()
}

visNormal <- function(){
  TeachingDemos::vis.normal()
}

vist <- function(){
  TeachingDemos::vis.t()
}

visGamma <- function(){
  TeachingDemos::vis.gamma()
}

run.cor.examp <- function (n = 100, seed)
  # slightly modified by J. Fox from the TeachingDemos package
{
  if (!missing(seed)) {
    set.seed(seed)
  }
  x <- scale(matrix(rnorm(2 * n, 0, 1), ncol = 2))
  x <- x %*% solve(chol(cor(x)))
  xr <- range(x)
  cor.refresh <- function(...) {
    r <- slider(no = 1)
    if (r == 1) {
      cmat <- matrix(c(1, 0, 1, 0), 2)
    }
    else if (r == -1) {
      cmat <- matrix(c(1, 0, -1, 0), 2)
    }
    else {
      cmat <- chol(matrix(c(1, r, r, 1), 2))
    }
    new.x <- x %*% cmat
    plot(new.x, xlab = "x", ylab = "y", xlim = xr, ylim = xr)
    title(paste("r = ", round(cor(new.x[, 1], new.x[, 2]),
                              3)))
  }
  slider(cor.refresh, "Correlation", -1, 1, 0.01, 0, title = "Correlation Demo")
  cor.refresh()
}

slider <- function (sl.functions, sl.names, sl.mins, sl.maxs, sl.deltas,
                    sl.defaults, but.functions, but.names, no, set.no.value,
                    obj.name, obj.value, reset.function, title)
  # slightly modified by J. Fox from the TeachingDemos package
{
  if (!missing(no))
    return(as.numeric(tclvalue(get(paste("slider", no, sep = ""),
                                   envir = getRcmdr("slider.env")))))
  if (!missing(set.no.value)) {
    try(eval(parse(text = paste("tclvalue(slider", set.no.value[1],
                                ")<-", set.no.value[2], sep = "")), envir = getRcmdr("slider.env")))
    return(set.no.value[2])
  }
  if (!missing(obj.name)) {
    if (!missing(obj.value))
      assign(obj.name, obj.value, envir = getRcmdr("slider.env"))
    else obj.value <- get(obj.name, envir = getRcmdr("slider.env"))
    return(obj.value)
  }
  if (missing(title))
    title <- "slider control widget"
  nt <- tktoplevel()
  tkwm.title(nt, title)
  tkwm.geometry(nt, "+0+0")
  if (missing(sl.names))
    sl.names <- NULL
  if (missing(sl.functions))
    sl.functions <- function(...) {
    }
  for (i in seq(sl.names)) {
    eval(parse(text = paste("assign('slider", i, "',tclVar(sl.defaults[i]), envir=getRcmdr('slider.env'))",
                            sep = "")))
    tkpack(fr <- tkframe(nt))
    lab <- tklabel(fr, text = sl.names[i], width = "25")
    sc <- tkscale(fr, from = sl.mins[i], to = sl.maxs[i],
                  showvalue = TRUE, resolution = sl.deltas[i], orient = "horiz")
    tkpack(lab, sc, side = "right")
    assign("sc", sc, envir = getRcmdr("slider.env"))
    eval(parse(text = paste("tkconfigure(sc,variable=slider",
                            i, ")", sep = "")), envir = getRcmdr("slider.env"))
    sl.fun <- if (length(sl.functions) > 1)
      sl.functions[[i]]
    else sl.functions
    if (!is.function(sl.fun))
      sl.fun <- eval(parse(text = paste("function(...){",
                                        sl.fun, "}")))
    tkconfigure(sc, command = sl.fun)
  }
  assign("slider.values.old", sl.defaults, envir = getRcmdr("slider.env"))
  tkpack(f.but <- tkframe(nt), fill = "x")
  tkpack(tkbutton(f.but, text = "Exit", command = function() tkdestroy(nt)),
         side = "right")
  if (!missing(reset.function)){
    reset.function <- function(...) print("relax")
    if (!is.function(reset.function))
      reset.function <- eval(parse(text = paste("function(...){",
                                                reset.function, "}")))
    tkpack(tkbutton(f.but, text = "Reset", command = function() {
      for (i in seq(sl.names)) eval(parse(text = paste("tclvalue(slider",
                                                       i, ")<-", sl.defaults[i], sep = "")), envir = getRcmdr("slider.env"))
      reset.function()
    }), side = "right")
  }
  if (missing(but.names))
    but.names <- NULL
  for (i in seq(but.names)) {
    but.fun <- if (length(but.functions) > 1)
      but.functions[[i]]
    else but.functions
    if (!is.function(but.fun))
      but.fun <- eval(parse(text = paste("function(...){",
                                         but.fun, "}")))
    tkpack(tkbutton(f.but, text = but.names[i], command = but.fun),
           side = "left")
    cat("button", i, "eingerichtet")
  }
  invisible(nt)
}

mmultiWayAnova <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL)
  dialog.values <- getDialog("multiWayAnova", defaults)
  initializeDialog(title = gettextRcmdr("Multi-Way Analysis of Variance"))
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))
  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)) {
      UpdateModelNumber(-1)
      errorCondition(recall = multiWayAnova, message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),
                                                               modelValue))
      return()
    }
    if (is.element(modelValue, listAOVModels())) {
      if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        UpdateModelNumber(-1)
        tkdestroy(top)
        multiWayAnova()
        return()
      }
    }
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    putDialog ("multiWayAnova", list (initial.group = groups, initial.response = response))
    closeDialog()
    if (length(groups) == 0) {
      errorCondition(recall = multiWayAnova, message = gettextRcmdr("You must select at least one factor."))
      return()
    }
    if (length(response) == 0) {
      errorCondition(recall = multiWayAnova, message = gettextRcmdr("You must select a response variable."))
      return()
    }
    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste(modelValue, " <- lm(", response,
                       " ~ ", paste(groups, collapse = "*"), ", data=",
                       .activeDataSet, ", contrasts=list(", paste(paste(groups, '="contr.Sum"'), collapse=", "), "))", sep = ""))
    doItAndPrint(paste("Anova(", modelValue, ")", sep = ""))
    doItAndPrint(paste("with(", .activeDataSet, ", (tapply(", response,
                       ", list(", groups.list, "), mean, na.rm=TRUE))) # means",
                       sep = ""))
    doItAndPrint(paste("with(", .activeDataSet, ", (tapply(", response,
                       ", list(", groups.list, "), sd, na.rm=TRUE))) # std. deviations",
                       sep = ""))
    # doItAndPrint(paste("with(", .activeDataSet, ", (tapply(", response,
    #                    ", list(", groups.list, "), function(x) sum(!is.na(x))))) # counts",
    #                    sep = ""))
    doItAndPrint(paste("xtabs(~ ", paste(groups, collapse=" + "), ", data=", .activeDataSet, ") # counts", sep=""))
    activeModel(modelValue)
    putRcmdr("modelWithSubset", FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "multiWayAnova", apply = "multiWayAnova")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix()
}

Menu.pb2level <- function(){
  initializeDialogDoE(title=gettextRcmdr("Create 2-level screening design ..."))
  ## function initializeDialogDoE assumes topdes2 as windowname
  ## last stored top left corner for window is stored under topleft2xy
  ## onRefresh still makes window walk a little

  if (exists("curindex", where="RcmdrEnv")) rm(curindex, pos="RcmdrEnv")

  if (!exists(".stored.design2pb",where="RcmdrEnv"))
    assign(".stored.design2pb", .default.design2pb,pos="RcmdrEnv")
  ## nameVar, nrunVar, nfacVar, nrepVar
  ## cbInitials containing repeat.onlyVariable, randomizeVariable,
  ##                       facnamesAutoVariable, faclevelsCommonVariable,
  ##                       nrunEntryVariable, estcbVariable
  ##                       specialcbVariable, replacecbVariable, MaxC2cbVariable
  ##                       res3cbVariable
  ## level1Var, level2Var, seedVar, specialrbVariable, hardVar, genVar,
  ## catlgVar, designVar, designrbVariable, destyperbVariable
  ## resVar, qualcritrbVariable, facnamlist,faclev1list,faclev2list, faclablist
  ## etyperbVariable, decimalrbVariable, dirVar, fileVar

  ## MaxC2cbVariable is now used for taguchi order checkbox

  ## define called functions
  infoClose <- function(){
    putRcmdr("infotxt",tclVar(""))
  }

  onHelpTab1 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.pb2levelTab1"))
  }
  onHelpTab2 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.FacDetails2Tab"))
  }
  onHelpTab6 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.exportTab"))
  }

  tabpos <- function(){
    ### get 0-based index of currently selected tab
    activestab.tn <- tclvalue(tcl(tn, "select"))
    activestab.tn <- strsplit(activestab.tn,".",fixed=TRUE)[[1]]
    activestab.tn <- as.numeric(activestab.tn[length(activestab.tn)])-1
    activestab.tn
  }

  storeRcmdr <- function(){
    hilf <- list(nameVar=tclvalue(nameVar),
                 nrunVar=tclvalue(nrunVar),nfacVar=tclvalue(nfacVar),nrepVar=tclvalue(nrepVar),
                 ncenterVar=tclvalue(ncenterVar),
                 cbInitials = c(tclvalue(repeat.onlyVariable), tclvalue(randomizeVariable),
                                tclvalue(facnameAutoVariable),tclvalue(faclevelCommonVariable),
                                1,0,
                                0,tclvalue(replacecbVariable),tclvalue(taguchicbVariable),
                                0
                 ),
                 level1Var=tclvalue(level1Var),level2Var=tclvalue(level2Var),seedVar=tclvalue(seedVar),
                 facnamlist=as.character(tclObj(facnamlist)),
                 faclev1list=as.character(tclObj(faclev1list)),
                 faclev2list=as.character(tclObj(faclev2list)),
                 faclablist=as.character(tclObj(faclablist)),
                 etyperbVariable=tclvalue(etyperbVariable),
                 decimalrbVariable=tclvalue(decimalrbVariable),
                 dirVar=tclvalue(dirVar), fileVar=tclvalue(fileVar))
    class(hilf) <- c("menu.design2pb","list")
    putRcmdr(".stored.design2pb",hilf)
  }

  onOK <- function(){
    onRefreshEnd()
    ## store entries so that users do not have to redo everything
    ## in case of stupid mistakes
    storeRcmdr()
    ## seed is not used from previously stored design
    closeDialog(window=topdes2)
    name <- tclvalue(nameVar)
    if (!is.valid.name(name)){
      errorCondition(window=topdes2,recall=Menu.pb2level,
                     message=paste('"', name, '" ', gettextRcmdr("is not a valid name."), sep=""))
      return()
    }
    if (is.element(name, listObjects()))
    {
      if ("no" == tclvalue(checkReplace(name, gettextRcmdr("Object"))))
      {
        errorCondition(window=topdes2,recall=Menu.pb2level,
                       gettextRcmdr("Introduce another name for the new data.frame, or allow replacing."))
        return()
      }
    }
    ###  further error messages with return to menu ?

    textfactornameslist.forcommand <- paste("factor.names=list(",paste(paste(as.character(tclObj(facnamlist)),"=c(",
                                                                             dquote(as.character(tclObj(faclev1list))), ",",
                                                                             dquote(as.character(tclObj(faclev2list))), ")",sep=""),
                                                                       collapse=","),")")

    ### not yet perfect, especially NULL entries are not possible
    ### for didactic reasons distinguish between usage of default.levels and other?
    command <- paste("pb(nruns=",tclvalue(nrunVar),",n12.taguchi=",
                     as.logical(as.numeric(tclvalue(taguchicbVariable))),
                     ",nfactors=",tclvalue(nrunVar),"-1, ncenter=",
                     tclvalue(ncenterVar), ", replications=", tclvalue(nrepVar),
                     ",repeat.only=",as.logical(as.numeric(tclvalue(repeat.onlyVariable))),
                     ",randomize=",as.logical(as.numeric(tclvalue(randomizeVariable))),",seed=",tclvalue(seedVar),
                     ",",textfactornameslist.forcommand,")")


    hilf <- justDoItDoE(command)
    if (class(hilf)[1]=="try-error") {
      Message(paste(gettextRcmdr("Offending command:"), "\n", command), type="error")
      errorCondition(window=topdes2,recall=Menu.pb2level, message=gettextRcmdr(hilf))
      rm(list=name, envir = .GlobalEnv)
      return()
    }
    logger(paste(name, "<-", command))
    logger("## creator element of design.info will be different, when using the command line command!")
    ## change creator to contain menu settings
    hilfatt <- design.info(hilf)
    hilfatt$creator <- .stored.design2pb
    class(hilfatt$creator) <- c("menu.design2pb", "list")
    design.info(hilf) <- hilfatt
    putRcmdr("hilf", hilf)
    justDoIt(paste(name, "<- getRcmdr(\"hilf\")"))
    rm("hilf", pos="RcmdrEnv")
    activeDataSet(name)
    ### exporting
    if (!tclvalue(etyperbVariable)=="none"){
      putRcmdr("path", tclvalue(dirVar))
      putRcmdr("filename", tclvalue(fileVar))
      if (!as.logical(as.numeric(tclvalue(replacecbVariable)))){
        lf <- tolower(list.files(path = path))
        if (tolower(paste(filename, "rda", sep = ".")) %in% lf)
          stop("file ", paste(filename, "rda", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="html" & tolower(paste(filename, "html", sep = ".")) %in% lf)
          stop("file ", paste(filename, "html", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="csv" & tolower(paste(filename, "csv", sep = ".")) %in% lf)
          stop("file ", paste(filename, "csv", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
      }
      if (tclvalue(decimalrbVariable)=="default") command <- paste("export.design(",name,
                                                                   ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                                                                   as.logical(as.numeric(tclvalue(replacecbVariable))),")",sep="")
      else command <- paste("export.design(",name,
                            ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                            as.logical(as.numeric(tclvalue(replacecbVariable))),", OutDec=", dquote(tclvalue(decimalrbVariable)),")",sep="")
      hilf <- justDoItDoE(command)
      if (class(hilf)[1]=="try-error") {
        errorCondition(window=topdes2,recall=Menu.pb2level, message=gettextRcmdr(hilf))
        return()
      }
      logger(command)
    }
    rm(activestab.tn, pos="RcmdrEnv")
    tkwm.deiconify(CommanderWindow())
    tkfocus(CommanderWindow())
  }

  listDesign2 <- function (envir = .GlobalEnv, ...)
  {
    Vars <- ls(envir = envir, all.names = TRUE)
    Vars[which(sapply(Vars, function(.x){
      aus <- FALSE
      if ("menu.design2pb" %in% class(get(.x, envir = envir))) aus <- TRUE
      else if ("design" %in% class(get(.x, envir = envir)))
        if ("menu.design2pb" %in% class(design.info(get(.x, envir = envir))$creator))
          aus <- TRUE
        aus
    }))]
  }


  onLoad <- function(){
    ## seems to work now, needs to be tested!
    hilf <- listDesign2()
    if (length(hilf)==0) {
      tkmessageBox(message=gettextRcmdr("There are no stored design inputs in this session."),
                   icon="error", type="ok", title="no stored design inputs")
      return()
    }
    putRcmdr("deschoose2",tktoplevel())
    tkwm.title(deschoose2, gettextRcmdr("Choose stored design form"))
    position <- if (is.SciViews())
      -1
    else position <- "+50+50"
    tkwm.geometry(deschoose2, position)
    putRcmdr("lb", variableListBox(deschoose2, variableList=hilf, title="Choose stored design form"))
    tkgrid(lb$frame)
    onOK <- function() {
      putRcmdr(".stored.design2pb",get(lb$varlist[as.numeric(tclvalue(tcl(lb$listbox, "curselection")))+1]))
      if ("design" %in% class(getRcmdr(".stored.design2pb")))
        putRcmdr(".stored.design2pb", design.info(getRcmdr(".stored.design2pb"))$creator)
      tkfocus(CommanderWindow())
      tkdestroy(topdes2)
      tkdestroy(deschoose2)
      Menu.pb2level()
    }
    OKCancelHelp(window=deschoose2)
    tkgrid(buttonsFrame, sticky="s")
    dialogSuffix(window=deschoose2, rows=1, columns=1,
                 focus=lb$listbox)
  }

  onRefreshEnd <- function(){
    nfacchange()
    storeRcmdr()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    putRcmdr("activestab.tn",tabpos())
    ID <- topdes2$ID
    putRcmdr("topleft2xy",as.numeric(c(tclvalue(.Tcl(paste("winfo rootx", ID))),
                                       tclvalue(.Tcl(paste("winfo rooty", ID))))))
    #        assign("activestab.tn",strsplit(activestab.tn,".",fixed=TRUE)[[1]],pos="RcmdrEnv")
    #        assign("activestab.tn",as.numeric(activestab.tn[length(activestab.tn)])-1,pos="RcmdrEnv")
  }

  onRefresh <- function(){
    #print(as.character(tclObj(tcl(tn, "select"))))
    onRefreshEnd()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.pb2level()
  }

  onStore <- function(){
    ## Speichernamen abfragen und hier ermöglichen (statt stored.design2pb)
    textentry() ## creates text string stored in savename.RcmdrPlugin.DoE
    if (!is.null(savename.RcmdrPlugin.DoE)){
      if (!is.valid.name(savename.RcmdrPlugin.DoE)){
        textcorrect(gettextRcmdr("This is not a valid name. Please correct:"))
        return()
      }
      if (is.element(savename.RcmdrPlugin.DoE, listObjects()))
      {
        if ("no" == tclvalue(checkReplace(savename.RcmdrPlugin.DoE, gettextRcmdr("Object"))))
        {
          textcorrect(gettextRcmdr("Please enter a new name:"))
          return()
        }
      }
      storeRcmdr()
      ## replace assign by justDoIt; assign(savename.RcmdrPlugin.DoE, getRcmdr(".stored.design2pb"), envir=.GlobalEnv)
      justDoIt(paste(savename.RcmdrPlugin.DoE, "<- getRcmdr(\".stored.design2pb\")"))
      message(gettextRcmdr("inputs have been stored"))
    }
  }

  onReset <- function(){
    assign(".stored.design2pb",.default.design2pb,pos="RcmdrEnv")
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.pb2level()
  }

  nfacchange <- function(){
    nfacold <- length(as.character(tclObj(varlistshort)))
    nfacnew <- as.numeric(tclvalue(nfacVar))
    if (nfacold==nfacnew) return()
    if (nfacnew < nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[1:nfacnew]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[1:nfacnew]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[1:nfacnew]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[1:nfacnew]))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(faclev1ListBox, listvariable=faclev1list, height=min(10,nfacnew))
      tkconfigure(faclev2ListBox, listvariable=faclev2list, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      if (selpos > nfacnew){
        tcl(fsel, "current", "0")
        factorsel()
      }
    }
    if (nfacnew > nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist", tclVar(c(as.character(tclObj(facnamlist)),
                                      getRcmdr("varlistshortt")[(nfacold+1):nfacnew])) )
      putRcmdr("faclev1list", tclVar(c(as.character(tclObj(faclev1list)),
                                       rep(tclvalue(level1Var),nfacnew-nfacold))))
      putRcmdr("faclev2list", tclVar(c(as.character(tclObj(faclev2list)),
                                       rep(tclvalue(level2Var),nfacnew-nfacold))))
      putRcmdr("faclablist", tclVar(c(as.character(tclObj(faclablist)),
                                      rep("",nfacnew-nfacold))))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      tkconfigure(faclev1ListBox, listvariable=faclev1list, height=min(10,nfacnew))
      tkconfigure(faclev2ListBox, listvariable=faclev2list, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))
    }
  }
  nameenter <- function(){
    if (identical(tclvalue(getRcmdr("fileVar")),tclvalue(getRcmdr("nameVar"))))
      putRcmdr("name.equal.filename", TRUE)
    else putRcmdr("name.equal.filename", FALSE)
  }
  namechange <- function(){
    if (is.valid.name(tclvalue(nameVar))){
      if (name.equal.filename){
        putRcmdr("fileVar", tclVar(tclvalue(nameVar)))  ## otherwise, variables would be directly tied
        #          putRcmdr("exportlabVar", tclVar(paste("Current design to be saved:", tclvalue(nameVar),"\n   ")))
        tkconfigure(fileEntry, textvariable=getRcmdr("fileVar"))
        #          tkconfigure(exportlab, textvariable=getRcmdr("exportlabVar"))
      }
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid design name")
  }
  factorsel<-function(){
    #### aendert die in der Textbox dargestellte Auswahl
    #### ruiniert aber leider auch wieder die korrekte Ueberschreibung der Werte
    putRcmdr("selpos", as.numeric(tclvalue(tcl(fsel, "current")))+1)
    putRcmdr("curfac", tclVar(as.character(tclObj(varlistshort))[selpos]))
    putRcmdr("curfnam", tclVar(as.character(tclObj(facnamlist))[selpos]))
    putRcmdr("curflev1", tclVar(as.character(tclObj(faclev1list))[selpos]))
    putRcmdr("curflev2", tclVar(as.character(tclObj(faclev2list))[selpos]))
    putRcmdr("curflab", tclVar(as.character(tclObj(faclablist))[selpos]))
    tkconfigure(fnam, textvariable=curfnam)
    tkconfigure(flev1, textvariable=curflev1)
    tkconfigure(flev2, textvariable=curflev2)
    tkconfigure(flab, textvariable=curflab)
  }
  fnamchange <- function(){
    ## selpos known from factorsel
    if (is.valid.name(tclvalue(curfnam))){
      hilf <- as.character(tclObj(facnamlist))
      hilf[selpos] <- tclvalue(curfnam)
      putRcmdr("facnamlist",tclVar(hilf))
      ### "facnamlist" is not automatically updated in the listbox
      ### therefore the tkconfigure
      tkconfigure(facnameListBox, listvariable=facnamlist)
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid factor name")
  }

  level1enter <- function(){
    putRcmdr("the.common.level1", tclvalue(getRcmdr("level1Var")))
  }
  level1change <- function(){
    if (identical(getRcmdr("the.common.level1"), tclvalue(getRcmdr("level1Var")))) return()
    onRefresh()
  }
  level2enter <- function(){
    putRcmdr("the.common.level2", tclvalue(getRcmdr("level2Var")))
  }
  level2change <- function(){
    if (identical(getRcmdr("the.common.level2"), tclvalue(getRcmdr("level2Var")))) return()
    onRefresh()
  }

  flev1change <- function(){
    ## selpos known from factorsel
    if (length(as.character(tclObj(curflev1)))==1){
      hilf <- as.character(tclObj(faclev1list))
      hilf[selpos] <- tclvalue(curflev1)
      putRcmdr("faclev1list",tclVar(hilf))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
    }
    else tkmessageBox(message="Empty entries or entries with blanks are not permitted, please correct!",
                      icon="error", type="ok", title="Invalid factor level")
  }
  flev2change <- function(){
    ## selpos known from factorsel
    if (length(as.character(tclObj(curflev2)))==1){
      hilf <- as.character(tclObj(faclev2list))
      hilf[selpos] <- tclvalue(curflev2)
      putRcmdr("faclev2list",tclVar(hilf))
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
    }
    else tkmessageBox(message="Empty entries or entries with blanks are not permitted, please correct!",
                      icon="error", type="ok", title="Invalid factor level")
  }
  flabchange <- function(){
    ## selpos known from factorsel
    ## for FocusOut event on flab
    ## still problematic, if Focus out occurs with tab
    ## as there is also a tab key event
    hilf <- as.character(tclObj(faclablist))
    hilf[selpos] <- tclvalue(curflab)
    putRcmdr("faclablist",tclVar(hilf))
    tkconfigure(faclabListBox, listvariable=faclablist)
  }

  tabflab <- function(){
    ## for Tab key event on flab
    ## the traversal still jumps to the first traversable control on the sheet
    ## (rather than staying with fnam, if asked by tkfocus to do so)
    ## takefocus has so far been set to 0 for all widgets except the factor detail ones on this tab
    flabchange()  ## otherwise, not carried out!
    hilf <- as.numeric(tclvalue(tcl(fsel,"current")))+1
    if (hilf  >= as.numeric(tclvalue(nfacVar))) return()
    tcl(fsel,"current", hilf)
    factorsel()
    #tkfocus(fnam)
    #tcl(fnam, "selection", "range", 1, "end")
    #tcl("break")
  }


  swap <- function(a,b){
    hilf <- 1:as.numeric(tclvalue(nfacVar))
    hilf[b] <- a
    hilf[a] <- b
    hilf
  }

  indexchange <- function(){
    if (curindex < as.numeric(tclvalue(nfacVar)))
      putRcmdr("orderDown",swap(curindex, curindex+1))
    if (curindex > 1)
      putRcmdr("orderUp",swap(curindex, curindex-1))
    tcl(fsel, "current", curindex-1)
    factorsel()
  }

  checkIndexShort <- function(){
    putRcmdr("curindex", as.numeric(tcl(facshortListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexNam <- function(){
    putRcmdr("curindex", as.numeric(tcl(facnameListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLev1 <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclev1ListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLev2 <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclev2ListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLab <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclabListBox,"curselection"))+1)
    indexchange()
  }


  onUp <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex=="1" | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderUp]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[orderUp]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[orderUp]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderUp]))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex-1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(faclev1ListBox,"selection","set",curindex-1)
      tcl(faclev2ListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  onDown <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex==as.numeric(tclvalue(nfacVar)) | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderDown]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[orderDown]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[orderDown]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderDown]))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex+1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(faclev1ListBox,"selection","set",curindex-1)
      tcl(faclev2ListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  dquote <- function(obj){
    ## quote vector elements for use as character vector in a command
    aus <- rep("",length(obj))
    wopt <- options("warn")[[1]]
    options(warn=-1)
    for (i in 1:length(obj)) if (is.na(as.numeric(obj[i]))) {
      if (length(grep('"',obj[i])>0))
        aus[i] <- paste("'",obj[i],"'",sep="")
      else
        aus[i] <- paste('"',obj[i],'"',sep="")
    }
    else aus[i] <- obj[i]
    options(warn=wopt)
    aus
  }


  onChangeDir <- function(){
    putRcmdr("direct",tclvalue(tkchooseDirectory()))
    if (!direct=="") {
      putRcmdr("dirVar", tclVar(direct))
      tkconfigure(dirEntry, textvariable = dirVar)
    }
  }

  ######## end define functions


  ##### define userform
  #tn <- ttknotebook(top,height=100, width=500)

  putRcmdr("tn",ttknotebook(topdes2))
  #tn <- ttknotebook(topdes2)

  putRcmdr("tab1",ttkframe(tn))
  putRcmdr("tab2",ttkframe(tn))
  putRcmdr("tab6",ttkframe(tn))## called 6 because of parallel treatment with
  ## fractional factorial menu

  tkadd(tn,tab1,text="Base Settings")   ### tabid=0
  tkadd(tn,tab2,text="Factor Details")  ### tabid=1
  tkadd(tn,tab6,text="Export") ### tabid=5

  tkconfigure(tn, takefocus=0)

  nameFrame <- ttkframe(tab1)
  baseFrame <- ttklabelframe(tab1,text=gettextRcmdr("Size and randomization"))

  ### widgets for tab1 and base frame
  putRcmdr("nameVar", tclVar(.stored.design2pb$nameVar))
  nameEntry <- tkentry(nameFrame, width="20", textvariable=nameVar)
  tkbind(nameEntry, "<FocusIn>", nameenter)
  tkbind(nameEntry, "<FocusOut>", namechange)

  nrunVar <- tclVar(.stored.design2pb$nrunVar)
  nrunEntry <- tkentry(baseFrame, width="8", textvariable=nrunVar)
  nrunHint <- ttklabel(baseFrame, text="(multiple of 4, >=8)", foreground="#888888")
  nfacVar <- tclVar(.stored.design2pb$nfacVar)
  nfacEntry <- tkentry(baseFrame, width="8", textvariable=nfacVar)
  nfacHint <- ttklabel(baseFrame, text="(< number of runs)", foreground="#888888")
  tkbind(nfacEntry,"<FocusOut>",nfacchange)
  ncenterVar <- tclVar(.stored.design2pb$ncenterVar)
  ncenterEntry <- tkentry(baseFrame, width="8", textvariable=ncenterVar)
  nrepVar <- tclVar(.stored.design2pb$nrepVar)
  nrepEntry <- tkentry(baseFrame, width="8", textvariable=nrepVar)
  randomizeVariable <-  tclVar(.stored.design2pb$cbInitials[2])
  randomizecb <- ttkcheckbutton(baseFrame,text=gettextRcmdr("Randomization"),variable=randomizeVariable)
  tkconfigure(randomizecb, takefocus=0)
  taguchicbVariable <-  tclVar(.stored.design2pb$cbInitials[9])
  taguchicb <- ttkcheckbutton(baseFrame,text=gettextRcmdr("12 run design in Taguchi order"),variable=taguchicbVariable)
  tkconfigure(taguchicb, takefocus=0)
  seedVar <- tclVar(sample(31999,1))  ## always new
  seedEntry <- tkentry(baseFrame, width="8", textvariable=seedVar)
  tkconfigure(seedEntry, takefocus=0)
  repeat.onlyVariable <- tclVar(.stored.design2pb$cbInitials[1])
  repeat.onlycb <- ttkcheckbutton(baseFrame,text=gettextRcmdr("Repeat only"),variable=repeat.onlyVariable)
  tkconfigure(repeat.onlycb, takefocus=0)

  ## preparations for bottom frame
  bottomFrame <- tkframe(topdes2)

  ## grid base frame
  tkgrid(nrunlab <- tklabel(baseFrame, text=gettextRcmdr("Number of runs")), nrunEntry, nrunHint, sticky="w")
  tkgrid(taguchicb, sticky="w")
  ## omitted nfaccb, on form, nfactors must always be specified
  tkgrid(nfaclab <- tklabel(baseFrame, text=gettextRcmdr("Number of factors")), nfacEntry, nfacHint, sticky="w")
  tkgrid.configure(nfaclab, pady=15)
  tkgrid(ncenterlab <- tklabel(baseFrame, text=gettextRcmdr("Number of center points")), ncenterEntry, sticky="w")
  tkgrid.configure(ncenterlab, pady=15)
  tkgrid(nreplab <- tklabel(baseFrame, text=gettextRcmdr("Replications")), nrepEntry, repeat.onlycb, sticky="w")
  tkgrid.configure(nreplab, pady=15)
  tkgrid(randlab <- tklabel(baseFrame, text="You normally do not need to change randomization settings"),sticky="w",columnspan=3)
  tkgrid(seedlab <- tklabel(baseFrame, text=gettextRcmdr("Seed for randomization")), seedEntry,
         randomizecb, sticky="w")

  helptab1Button <- buttonRcmdr(nameFrame, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab1,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab1Button, takefocus=0)

  ### Finalize tab1
  tkgrid(tklabel(nameFrame, text="Name of new design"), nameEntry, helptab1Button, sticky="w")
  tkgrid(nameFrame, sticky="w", columnspan=4)
  tkgrid.configure(nameFrame, pady=40)
  tkgrid.configure(helptab1Button, sticky="ne")
  tkgrid(baseFrame, sticky="nw",columnspan=3)

  ## Factor Details Tab
  ## factor details frame
  ### facnameAutoVariable (not needed any more) and faclevelCommonVariable

  ## default levels frame
  deflevFrame <- ttklabelframe(tab2,text="Default levels")
  facnameAutoVariable <- tclVar(.stored.design2pb$cbInitials[3])
  faclevelCommonVariable <- tclVar(.stored.design2pb$cbInitials[4])
  faclevelCommonButton <- ttkcheckbutton(deflevFrame,text=gettextRcmdr("Common factor levels"),
                                         variable=faclevelCommonVariable,command=onRefresh)
  tkconfigure(faclevelCommonButton,takefocus=0)
  putRcmdr("level1Var", tclVar(.stored.design2pb$level1Var))
  level1Entry <- ttkentry(deflevFrame, width="20", textvariable=level1Var)
  tkconfigure(level1Entry,takefocus=0)
  tkbind(level1Entry, "<FocusIn>", level1enter)
  tkbind(level1Entry, "<FocusOut>", level1change)
  putRcmdr("level2Var", tclVar(.stored.design2pb$level2Var))
  level2Entry <- tkentry(deflevFrame, width="20", textvariable=level2Var)
  tkconfigure(level2Entry,takefocus=0)
  tkbind(level2Entry, "<FocusIn>", level2enter)
  tkbind(level2Entry, "<FocusOut>", level2change)
  tkgrid(faclevelCommonButton,sticky="w",columnspan=3)
  faclevCommonLab<-tklabel(deflevFrame,text=gettextRcmdr("CAUTION: Checking this box overwrites all custom factor levels."))
  if (!as.logical(as.numeric(tclvalue(faclevelCommonVariable)))){
    tkgrid(faclevCommonLab,sticky="w", columnspan=3)
    tkgrid.configure(faclevCommonLab,pady=10)
  }
  tkgrid(tklabel(deflevFrame, text=gettextRcmdr("First Level")),tklabel(deflevFrame,text="  ",width=2),tklabel(deflevFrame, text=gettextRcmdr("Second Level")),sticky="e")
  tkgrid(level1Entry, tklabel(deflevFrame,text="  ",width=2),level2Entry, sticky="e")

  ## factor details
  ## values as vectors
  facnamlistt <- .stored.design2pb$facnamlist
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))) {
    faclev1listt <- rep(tclvalue(level1Var),tclvalue(nfacVar))
    faclev2listt <- rep(tclvalue(level2Var),tclvalue(nfacVar))
  } else{
    faclev1listt <- .stored.design2pb$faclev1list
    faclev2listt <- .stored.design2pb$faclev2list
  }
  faclablistt <- .stored.design2pb$faclablist
  varlistshortt <- if (as.numeric(tclvalue(nfacVar))<=50)
    Letters[1:tclvalue(nfacVar)] else paste("F",1:tclvalue(nfacVar),sep="")

  enterlistFrame <- ttkframe(tab2)
  listFrame <- ttklabelframe(enterlistFrame, text="Factor Details")
  putRcmdr("selpos", 1)
  putRcmdr("curfac", tclVar(varlistshortt[1]))
  putRcmdr("curfnam", tclVar(facnamlistt[1]))
  putRcmdr("curflev1", tclVar(faclev1listt[1]))
  putRcmdr("curflev2", tclVar(faclev2listt[1]))
  putRcmdr("curflab", tclVar(faclablistt[1]))

  ## fsel must select the right factor
  ## this should be highlighted in factor lists
  ##    and all related entries shown for changing in text boxes fnam etc.
  enterFrame <- ttklabelframe(enterlistFrame, text=gettextRcmdr("Modify factor details for selected factor"))
  fsel <- ttkcombobox(enterFrame, textvariable=curfac, width=5, values=varlistshortt, state="readonly")
  tkbind(fsel, "<<ComboboxSelected>>", factorsel)
  #fnam <- ttkentry(listFrame, textvariable=curfnam, width=20,validate="focusout", validatecommand=fnamchange)
  fnam <- ttkentry(enterFrame, textvariable=curfnam, width=15)
  tkbind(fnam, "<FocusOut>", fnamchange)
  flev1 <- ttkentry(enterFrame, textvariable=curflev1, width=15)
  tkbind(flev1, "<FocusOut>", flev1change)
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))){
    tkconfigure(flev1,state="disabled")
  }
  flev2 <- ttkentry(enterFrame, textvariable=curflev2, width=15)
  tkbind(flev2, "<FocusOut>", flev2change)
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))){
    tkconfigure(flev2,state="disabled")
  }
  flab <- ttkentry(enterFrame, textvariable=curflab, width=20)
  tkbind(flab, "<FocusOut>", flabchange)
  tkbind(flab, "<Key-Tab>", tabflab)
  tkgrid(tklabel(enterFrame,text=gettextRcmdr("Select"),width=6),
         tklabel(enterFrame,text=gettextRcmdr("Factor name"), width=15),
         tklabel(enterFrame,text=gettextRcmdr("First level"), width=15),
         tklabel(enterFrame,text=gettextRcmdr("Second level"), width=15),
         tklabel(enterFrame,text=gettextRcmdr("Comment or label \n(for html export only)"), width=20),
         sticky="w")
  tkgrid(fsel,fnam, flev1, flev2, flab, sticky="w")

  putRcmdr("facnamlist", tclVar(facnamlistt))
  putRcmdr("varlistshort", tclVar(varlistshortt))
  putRcmdr("faclev1list", tclVar(faclev1listt))
  putRcmdr("faclev2list", tclVar(faclev2listt))
  putRcmdr("faclablist", tclVar(faclablistt))

  facshortListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                               selectmode = single, exportselection = "TRUE", listvariable=varlistshort,
                               width = 6, background="#EBEBDC")
  tkbind(facshortListBox, "<<TraverseIn>>",function() tkfocus(fsel))

  facnameListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=facnamlist,
                              width = 15, background="#EBEBDC")
  faclev1ListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=faclev1list,
                              width = 15, background="#EBEBDC")
  faclev2ListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=faclev2list,
                              width = 15, background="#EBEBDC")
  faclabListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                             selectmode = single, exportselection = "TRUE", listvariable=faclablist,
                             width = 20, background="#EBEBDC")

  ## determine current index and reordering for onUp and onDown
  tkbind(facshortListBox, "<<ListboxSelect>>", checkIndexShort)
  tkbind(facnameListBox, "<<ListboxSelect>>", checkIndexNam)
  tkbind(faclev1ListBox, "<<ListboxSelect>>", checkIndexLev1)
  tkbind(faclev2ListBox, "<<ListboxSelect>>", checkIndexLev2)
  tkbind(faclabListBox, "<<ListboxSelect>>", checkIndexLab)


  ### funktioniert, ist aber noch nicht schön
  scrollbar <- ttkscrollbar(listFrame, command = function(...) {
    tkyview(facshortListBox, ...)
    tkyview(facnameListBox, ...)
    tkyview(faclev1ListBox, ...)
    tkyview(faclev2ListBox, ...)
    tkyview(faclabListBox, ...)
  })

  #    tkgrid(tklabel(enterlistFrame,text="  ", width=5),enterFrame, sticky="w")
  tkgrid(enterFrame, sticky="w", columnspan=5)
  tkgrid.configure(enterFrame, pady=10)
  ## Hoch-/Runterschieben von Einträgen ermöglichen

  downupFrame <- ttkframe(listFrame)
  moveDownButton <- buttonRcmdr(downupFrame, text = gettextRcmdr("Move Down"),
                                foreground = "darkgreen", command = onDown,
                                default = "normal", borderwidth = 3, width=12)
  moveUpButton <- buttonRcmdr(downupFrame, text = gettextRcmdr("Move Up"),
                              foreground = "darkgreen", command = onUp,
                              default = "normal", borderwidth = 3, width=12)
  tkgrid(moveDownButton, sticky="w")
  tkgrid(moveUpButton, sticky="w")

  tkgrid(scrollbar, facshortListBox, facnameListBox, faclev1ListBox, faclev2ListBox, faclabListBox, downupFrame, sticky = "nw")
  tkgrid.configure(scrollbar, sticky = "wns")
  tkgrid.configure(facnameListBox, sticky = "new")

  helptab2Button <- buttonRcmdr(tab2, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab2,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab2Button, takefocus=0)

  ## finalize tab2 Factor details
  tkgrid(helptab2Button, sticky="e")
  tkgrid(deflevFrame, sticky="nw")
  tkgrid.configure(deflevFrame, pady=10)
  tkgrid(listFrame, columnspan=6,sticky="w")
  tkgrid(enterlistFrame, columnspan=6,sticky="w")

  ## tab6 for exporting
  helptab6Button <- buttonRcmdr(tab6, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab6,
                                default = "normal", borderwidth = 3)

  exportlabVar <- nameVar
  exportlab <- ttklabel(tab6, textvariable=exportlabVar)
  tkgrid(ttklabel(tab6,text="Current design to be saved:"),exportlab,helptab6Button,sticky="w")
  tkgrid.configure(exportlab, pady=15)
  tkgrid.configure(helptab6Button, sticky="ne")

  ## radio buttons for choosing export type
  etradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("(How to) Export ?"))
  etyperbVariable <- tclVar(.stored.design2pb$etyperbVariable)
  noexprb <- tkradiobutton(etradioFrame,text=gettextRcmdr("no export"),variable=etyperbVariable,value="none")
  allrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("all file types"),variable=etyperbVariable,value="all")
  rdarb <- tkradiobutton(etradioFrame,text=gettextRcmdr("rda only"),variable=etyperbVariable,value="rda")
  htmlrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("html and rda"),variable=etyperbVariable,value="html")
  csvrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("csv and rda"),variable=etyperbVariable,value="csv")
  tkgrid(noexprb, sticky="w")
  tkgrid(allrb, sticky="w")
  tkgrid(rdarb, sticky="w")
  tkgrid(htmlrb, sticky="w")
  tkgrid(csvrb, sticky="w")

  ## radio buttons for choosing export decimal separator
  decimalradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("Decimal Separator ?"))
  decimalrbVariable <- tclVar(.stored.design2pb$decimalrbVariable)
  defaultrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("default"),variable=decimalrbVariable, value="default")
  pointrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("."),variable=decimalrbVariable, value=".")
  commarb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr(","),variable=decimalrbVariable, value=",")
  tkgrid(defaultrb, sticky="w")  ## in this case, leave default option from options
  tkgrid(pointrb, sticky="w")
  tkgrid(commarb, sticky="w")

  ## export directory
  dirFrame <- ttklabelframe(tab6, text=gettextRcmdr("Storage Directory"))
  putRcmdr("dirVar", tclVar(.stored.design2pb$dirVar))
  dirEntry <- tkentry(dirFrame, width="50", textvariable=dirVar)
  dirButton <- buttonRcmdr(dirFrame, text = gettextRcmdr("Change directory"),
                           foreground = "darkgreen", width = "20", command = onChangeDir,
                           default = "normal", borderwidth = 3)
  tkgrid(dirEntry, tklabel(dirFrame, text="   "), dirButton, sticky="w")

  ## export file name
  putRcmdr("fileVar", tclVar(.stored.design2pb$fileVar))
  fileEntry <- tkentry(tab6, width="20", textvariable=fileVar)
  efnamelabel <- tklabel(tab6,text=gettextRcmdr("Export file names: name below with appropriate endings (html or csv, and rda)"))
  replacecbVariable <- tclVar(.stored.design2pb$cbInitials[8])
  replacecb <- ttkcheckbutton(tab6,text=gettextRcmdr("Replace file(s), if exists"),variable=replacecbVariable)

  ## always grid details, as otherwise default file name does not work
  ## design name info and help button have already been gridded above
  tkgrid(etradioFrame, decimalradioFrame, sticky="nw")
  tkgrid(dirFrame, sticky="w", columnspan=5)
  tkgrid.configure(dirFrame, pady=15)
  tkgrid(efnamelabel, sticky="w", columnspan=5)
  tkgrid(fileEntry, sticky="w", columnspan=5)
  tkgrid(replacecb, sticky="w", columnspan=5)


  ## add buttons outside the notebook
  buttonFrame <- tkframe(topdes2)
  ## die sind aber nicht dunkelgruen ...
  refreshButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Refresh form"),
                               foreground = "darkgreen", width = "12", command = onRefresh,
                               default = "normal", borderwidth = 3)
  storeButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Store form"),
                             foreground = "darkgreen", width = "12", command = onStore,
                             default = "normal", borderwidth = 3)
  loadButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Load form"),
                            foreground = "darkgreen", width = "12", command = onLoad,
                            default = "normal", borderwidth = 3)
  resetButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Reset form"),
                             foreground = "darkgreen", width = "12", command = onReset,
                             default = "normal", borderwidth = 3)
  #        tkgrid(refreshButton,sticky="w")
  #        tkgrid(tklabel(buttonFrame,text="  "),sticky="w")
  tkgrid(storeButton,sticky="w")
  tkgrid(loadButton,sticky="w")
  tkgrid(resetButton,sticky="w")

  tkconfigure(refreshButton, takefocus=0)
  tkconfigure(storeButton, takefocus=0)
  tkconfigure(loadButton, takefocus=0)
  tkconfigure(resetButton, takefocus=0)

  ## storage buttons to the right of the notebook
  tkgrid(tn, buttonFrame, sticky="w", columnspan=2)

  OKCancelHelp(window=topdes2, helpSubject="Menu.pb2level")
  tkconfigure(OKbutton, takefocus=0)
  tkconfigure(cancelButton, takefocus=0)
  tkconfigure(helpButton, takefocus=0)

  tkgrid(buttonsFrame, bottomFrame, sticky="ew")


  ### relations among widgets
  if (!as.logical(as.numeric(tclvalue(randomizeVariable)))){
    tkconfigure(seedEntry, state="disabled")
    tkconfigure(seedlab, state="disabled")
  }else {
    tkconfigure(seedEntry, state="normal")
    tkconfigure(seedlab, state="normal")
  }
  if (exists("activestab.tn", where="RcmdrEnv")){
    tcl(tn, "select", activestab.tn)
    rm(activestab.tn, pos="RcmdrEnv")
  }

  dialogSuffix(window=topdes2, rows=2, columns=2, focus=tn, bindReturn=FALSE)

}

Menu.FrF2level <- function(){

  initializeDialogDoE(title=gettext("Create regular 2-level design ..."))
  ## function initializeDialogDoE assumes topdes2 as windowname
  ## last stored top left corner for window is stored under topleft2xy
  ## onRefresh still makes window walk a little

  if (exists("curindex", where="RcmdrEnv")) rm(curindex, pos="RcmdrEnv")

  if (!exists(".stored.design2FrF", where="RcmdrEnv"))
    assign(".stored.design2FrF", .default.design2,pos="RcmdrEnv")
  ## nameVar, nrunVar, nfacVar, nrepVar
  ## cbInitials containing repeat.onlyVariable, randomizeVariable,
  ##                       aliasblock2fiVariable, faclevelsCommonVariable,
  ##                       nrunEntryVariable, estcbVariable
  ##                       specialcbVariable, replacecbVariable, MaxC2cbVariable
  ##                       res3cbVariable
  ## level1Var, level2Var, seedVar, specialrbVariable, hardVar, genVar,
  ## catlgVar, designVar, designrbVariable, destyperbVariable
  ## resVar, qualcritrbVariable, facnamlist,faclev1list,faclev2list, faclablist
  ## estrbVariable, maxtimeVar, est2fislist,
  ## etyperbVariable, decimalrbVariable, dirVar, fileVar

  ## MaxC2cbVariable is free again (no. 9 of cbInitials)

  ## define called functions
  infoClose <- function(){
    putRcmdr("infotxt",tclVar(""))
  }

  onHelpTab1 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.FrF2levelTab1"))
  }
  onHelpTab2 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.FacDetails2Tab"))
  }
  onHelpTab6 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.exportTab"))
  }

  onHelpTabEstimable <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.FrF2levelTabEstimable"))
  }

  tabpos <- function(){
    ### get 0-based index of currently selected tab
    activestab.tn <- tclvalue(tcl(tn, "select"))
    activestab.tn <- strsplit(activestab.tn,".",fixed=TRUE)[[1]]
    activestab.tn <- as.numeric(activestab.tn[length(activestab.tn)])-1
    activestab.tn
  }

  storeRcmdr <- function(){
    hilf <- list(nameVar=tclvalue(nameVar),
                 nrunVar=tclvalue(nrunVar),nfacVar=tclvalue(nfacVar),nrepVar=tclvalue(nrepVar),
                 nblockVar=tclvalue(nblockVar), ncenterVar=tclvalue(ncenterVar),
                 cbInitials = c(tclvalue(repeat.onlyVariable), tclvalue(randomizeVariable),
                                tclvalue(aliasblock2fiVariable),tclvalue(faclevelCommonVariable),
                                tclvalue(nrunEntryVariable),0,
                                tclvalue(specialcbVariable),tclvalue(replacecbVariable),0,
                                tclvalue(res3cbVariable)
                 ),
                 level1Var=tclvalue(level1Var),level2Var=tclvalue(level2Var),seedVar=tclvalue(seedVar),
                 specialrbVariable=tclvalue(specialrbVariable),hardVar=tclvalue(hardVar),
                 designrbVariable=tclvalue(designrbVariable),
                 genVar=tclvalue(genVar),catlgVar=tclvalue(catlgVar),designVar=tclvalue(designVar),
                 resVar=tclvalue(resVar),
                 qualcritrbVariable=tclvalue(qualcritrbVariable),
                 comprclassVar=tclvalue(comprclassVar),
                 facnamlist=as.character(tclObj(facnamlist)),
                 faclev1list=as.character(tclObj(faclev1list)),
                 faclev2list=as.character(tclObj(faclev2list)),
                 faclablist=as.character(tclObj(faclablist)),
                 estrbVariable=tclvalue(estrbVariable),
                 comprrbVariable=tclvalue(comprrbVariable),
                 maxtimeVar=tclvalue(maxtimeVar),est2fislist=est2fislist,
                 etyperbVariable=tclvalue(etyperbVariable),
                 decimalrbVariable=tclvalue(decimalrbVariable),
                 dirVar=tclvalue(dirVar), fileVar=tclvalue(fileVar))
    class(hilf) <- c("menu.design2FrF","list")
    putRcmdr(".stored.design2FrF", hilf)
  }

  onOK <- function(){
    onRefreshEnd()
    ## store entries so that users do not have to redo everything
    ## in case of stupid mistakes
    ## seed is not used from previously stored design
    storeRcmdr()
    closeDialog(window=topdes2)
    name <- tclvalue(nameVar)
    if (!is.valid.name(name)) {
      errorCondition(window=topdes2,recall=Menu.FrF2level,
                     message=paste('"', name, '" ', gettext("is not a valid name."), sep=""))
      return()
    }
    if (is.element(name, listObjects()))
    {
      if ("no" == tclvalue(checkReplace(name, gettext("Object"))))
      {
        errorCondition(window=topdes2,recall=Menu.FrF2level,
                       message=gettext("Introduce another name for the new data.frame, or allow replacing."))
        return()
      }
    }

    ###  further error messages with return to menu ?

    textfactornameslist.forcommand <- paste("factor.names=list(",paste(paste(as.character(tclObj(facnamlist)),"=c(",
                                                                             dquote(as.character(tclObj(faclev1list))), ",",
                                                                             dquote(as.character(tclObj(faclev2list))), ")",sep=""),
                                                                       collapse=","),")")

    ### not yet perfect, especially NULL entries are not possible
    ### also, not very didactical, as default settings are unnecessarily included
    ###

    ## do always
    MaxC2 <- FALSE
    if (tclvalue(qualcritrbVariable)=="MaxC2") MaxC2 <- TRUE
    nrun.forcommand <- tclvalue(nrunVar)
    resolution <- "NULL"
    alias.block.2fis <- "FALSE"
    if (as.logical(as.numeric(as.character(tclvalue(aliasblock2fiVariable)))))
      alias.block.2fis <- TRUE
    if (!as.logical(as.numeric(as.character(tclvalue(nrunEntryVariable))))){
      nrun.forcommand <- "NULL"
      resolution <- 3
      if (tclvalue(resVar)=="IV") resolution <- 4
      if (tclvalue(resVar)=="V+") resolution <- 5
    }

    if (!as.logical(as.numeric(as.character(tclvalue(specialcbVariable))))){
      ### separate statements because of repeat.only replications in blocked designs
      if (!(as.logical(as.numeric(tclvalue(repeat.onlyVariable))) & as.numeric(tclvalue(nblockVar))>0))
        command <- paste("FrF2(nruns=",nrun.forcommand,",nfactors=",tclvalue(nfacVar),", blocks=", tclvalue(nblockVar),
                         ", alias.block.2fis =", alias.block.2fis,
                         ", ncenter=", tclvalue(ncenterVar), ", MaxC2 =", MaxC2, ", resolution =", resolution,
                         ",replications=",tclvalue(nrepVar),",repeat.only=",as.logical(as.numeric(tclvalue(repeat.onlyVariable))),
                         ",randomize=",as.logical(as.numeric(tclvalue(randomizeVariable))),",seed=",tclvalue(seedVar),
                         ",",textfactornameslist.forcommand,")")
      else
        command <- paste("FrF2(nruns=",nrun.forcommand,",nfactors=",tclvalue(nfacVar),", blocks=", tclvalue(nblockVar),
                         ", alias.block.2fis =", alias.block.2fis,
                         ", ncenter=", tclvalue(ncenterVar), ", MaxC2 =", MaxC2, ", resolution =", resolution,
                         ",wbreps=",tclvalue(nrepVar),",repeat.only=TRUE, randomize=",
                         as.logical(as.numeric(tclvalue(randomizeVariable))),",seed=",tclvalue(seedVar),
                         ",",textfactornameslist.forcommand,")")
    }
    else{
      estimable <- "NULL"
      ## only do if special cases present
      if (!tclvalue(estrbVariable)=="none" & length(est2fislist)>0){
        ## only if selected and specified!!
        ### estimable interactions
        ## compromise plans
        if (tclvalue(comprrbVariable) == "compr") {
          command <- paste("compromise(",tclvalue(nfacVar),", c(",
                           paste(dquote(which(Letters  %in% notest2fislist)),collapse=","),
                           "), ",substr(tclvalue(comprclassVar),1,1),")")
          hilf <- justDoItDoE(command)
          if (class(hilf)[1]=="try-error") {
            Message(paste(gettext("Offending command:"), "\n", command), type="error")
            errorCondition(window=topdes2,recall=Menu.FrF2level, message=gettext(hilf))
            return()
          }
          ## replace assign by justDoIt; assign("calc.estim", hilf, envir=.GlobalEnv)
          putRcmdr("hilf", hilf)
          justDoIt("calc.estim <- getRcmdr(\"hilf\")")
          rm("hilf", pos="RcmdrEnv")

          #compromise(as.numeric(tclvalue(nfacVar)), which(Letters  %in% notest2fislist),
          #        as.numeric(substr(tclvalue(comprclassVar),1,1))))
          logger(paste("calc.estim <-", command))
          estimable <- "calc.estim$requirement"
          if (tclvalue(estrbVariable)=="distinct") estimable <- paste(estimable, ", perms=calc.estim$perms.full")
        }
        else estimable <- paste("c(",paste(dquote(est2fislist),collapse=","),")")

        if (!(tclvalue(hardVar)=="0" & tclvalue(designrbVariable)=="default" & tclvalue(nblockVar)=="1"))
          tk_messageBox(message=gettext("estimable has taken precedence, not all other requests have been granted!"),type="ok")
        clear <- "TRUE"
        if (tclvalue(estrbVariable)=="distinct") clear <- "FALSE"
        res3 <- as.logical(as.numeric(as.character(tclvalue(res3cbVariable))))
        command <- paste("FrF2(nruns=",nrun.forcommand,",nfactors=",tclvalue(nfacVar), ", MaxC2 =", MaxC2,
                         ",replications=",tclvalue(nrepVar),",repeat.only=",as.logical(as.numeric(tclvalue(repeat.onlyVariable))),
                         ",randomize=",as.logical(as.numeric(tclvalue(randomizeVariable))),",seed=",tclvalue(seedVar),
                         ",",textfactornameslist.forcommand,
                         ", estimable=", estimable, ", clear =", clear, ", res3 =", res3, ", max.time =", tclvalue(maxtimeVar),
                         ", select.catlg =", tclvalue(catlgVar),       ")")
      }
      else {
        hard.forcommand <- tclvalue(hardVar)
        if (hard.forcommand == "0") hard.forcommand <- "NULL"
        generators <- "NULL"
        design <- "NULL"
        if (tclvalue(designrbVariable)=="gen") generators <- paste("c(",paste(dquote(unlist(strsplit(tclvalue(genVar),","))),collapse=","),")")
        if (tclvalue(designrbVariable)=="design") design <- dquote(tclvalue(designVar))

        command <- paste("FrF2(nruns =",nrun.forcommand,",nfactors =",tclvalue(nfacVar),
                         ", blocks =", tclvalue(nblockVar), ", alias.block.2fis =", alias.block.2fis,
                         ", ncenter =", tclvalue(ncenterVar),
                         ", hard=",hard.forcommand, ", generators =", generators,
                         ", design =", design,
                         ",replications=",tclvalue(nrepVar),",repeat.only=",as.logical(as.numeric(tclvalue(repeat.onlyVariable))),
                         ",randomize=",as.logical(as.numeric(tclvalue(randomizeVariable))),",seed=",tclvalue(seedVar),
                         ",",textfactornameslist.forcommand, ", select.catlg =", tclvalue(catlgVar), ")")
      }
    }       ## end of special
    hilf <- justDoItDoE(command)
    if (tclvalue(estrbVariable)=="distinct" & length(est2fislist)>0){
      diagcommand <- paste("print(",dQuote(paste(gettext("Design search in progress: you allowed up to"),
                                                 tclvalue(maxtimeVar), gettext("seconds"))), ")")
      doItAndPrint(diagcommand, log=FALSE)
    }
    if (class(hilf)[1]=="try-error") {
      Message(paste(gettext("Offending command:"), "\n", command), type="error")
      errorCondition(window=topdes2,recall=Menu.FrF2level, message=gettext(hilf))
      #            if (tclvalue(estrbVariable)=="distinct" & length(est2fislist)>0){
      #                 diagcommand <- paste("print(",dQuote(paste(gettext("No design found in"),
      #                       tclvalue(maxtimeVar), gettext("seconds"))), ")")
      #                 doItAndPrint(diagcommand, log=FALSE)
      #                 diagcommand <- paste("print(",dQuote(gettext("Experts may try to speed up the search using command line programming (?estimable.2fis).")), ")")
      #                 doItAndPrint(diagcommand, log=FALSE)
      #                 }
      return()
    }
    logger(paste(name, "<-", command))
    logger("## creator element of design.info will be different, when using the command line command!")
    ## change creator to contain menu settings
    hilfatt <- design.info(hilf)
    hilfatt$creator <- .stored.design2FrF
    class(hilfatt$creator) <- c("menu.design2FrF", "list")

    attr(hilf, "design.info") <- hilfatt
    ## replace assign by justDoIt; assign(name, hilf, envir=.GlobalEnv)
    putRcmdr("hilf", hilf)
    justDoIt(paste(name, "<- getRcmdr(\"hilf\")"))
    rm("hilf", pos="RcmdrEnv")
    activeDataSet(name)
    ## remove calc.estim
    if (as.logical(as.numeric(as.character(tclvalue(specialcbVariable)))) & tclvalue(comprrbVariable)=="compr"){
      rm(calc.estim, envir=.GlobalEnv)
      logger("rm(calc.estim)")
    }
    ### exporting
    if (!tclvalue(etyperbVariable)=="none"){
      putRcmdr("path", tclvalue(dirVar))
      putRcmdr("filename", tclvalue(fileVar))
      if (!as.logical(as.numeric(tclvalue(replacecbVariable)))){
        lf <- tolower(list.files(path = path))
        if (tolower(paste(filename, "rda", sep = ".")) %in% lf)
          stop("file ", paste(filename, "rda", "."), " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="html" & tolower(paste(filename, "html", sep = ".")) %in% lf)
          stop("file ", paste(filename, "html", "."), " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="csv" & tolower(paste(filename, "csv", sep = ".")) %in% lf)
          stop("file ", paste(filename, "csv", "."), " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
      }
      if (tclvalue(decimalrbVariable)=="default") command <- paste("export.design(",name,
                                                                   ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                                                                   as.logical(as.numeric(tclvalue(replacecbVariable))),")",sep="")
      else command <- paste("export.design(",name,
                            ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                            as.logical(as.numeric(tclvalue(replacecbVariable))),", OutDec=", dquote(tclvalue(decimalrbVariable)),")",sep="")
      hilf <- justDoItDoE(command)
      if (class(hilf)[1]=="try-error") {
        errorCondition(window=topdes2,recall=Menu.FrF2level, message=gettext(hilf))
        return()
      }
      logger(command)
    }
    rm(activestab.tn, pos="RcmdrEnv")
    tkwm.deiconify(CommanderWindow())
    tkfocus(CommanderWindow())
  }

  listDesign2 <- function (envir = .GlobalEnv, ...)
  {
    Vars <- ls(envir = envir, all.names = TRUE)
    Vars[which(sapply(Vars, function(.x){
      aus <- FALSE
      if ("menu.design2FrF" %in% class(get(.x, envir = envir))) aus <- TRUE
      else if ("design" %in% class(get(.x, envir = envir)))
        if ("menu.design2FrF" %in% class(design.info(get(.x, envir = envir))$creator))
          aus <- TRUE
        aus
    }))]
  }


  onLoad <- function(){
    ## seems to work now, needs to be tested!
    hilf <- listDesign2()
    if (length(hilf)==0) {
      tkmessageBox(message=gettext("There are no stored design inputs in this session."),icon="error", type="ok", title="no stored design inputs")
      return()
    }
    putRcmdr("deschoose2",tktoplevel())
    tkwm.title(deschoose2, gettext("Choose stored design form"))
    position <- if (is.SciViews())
      -1
    else position <- "+50+50"
    tkwm.geometry(deschoose2, position)
    putRcmdr("lb", variableListBox(deschoose2, variableList=hilf, title="Choose stored design form"))
    tkgrid(lb$frame)
    onOK <- function() {
      putRcmdr(".stored.design2FrF",get(lb$varlist[as.numeric(tclvalue(tcl(lb$listbox, "curselection")))+1]))
      if ("design" %in% class(getRcmdr(".stored.design2FrF")))
        putRcmdr(".stored.design2FrF", design.info(getRcmdr(".stored.design2FrF"))$creator)
      tkfocus(CommanderWindow())
      tkdestroy(topdes2)
      tkdestroy(deschoose2)
      Menu.FrF2level()
    }
    OKCancelHelp(window=deschoose2)
    tkgrid(buttonsFrame, sticky="s")
    dialogSuffix(window=deschoose2, rows=1, columns=1,
                 focus=lb$listbox)
  }

  onRefreshEnd <- function(){
    nfacchange()
    storeRcmdr()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    putRcmdr("activestab.tn",tabpos())
    ID <- topdes2$ID
    putRcmdr("topleft2xy",as.numeric(c(tclvalue(.Tcl(paste("winfo rootx", ID))),
                                       tclvalue(.Tcl(paste("winfo rooty", ID))))))
    #        assign("activestab.tn",strsplit(activestab.tn,".",fixed=TRUE)[[1]],pos="RcmdrEnv")
    #        assign("activestab.tn",as.numeric(activestab.tn[length(activestab.tn)])-1,pos="RcmdrEnv")
  }

  onRefresh <- function(){
    onRefreshEnd()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.FrF2level()
  }

  onestrb <- function(){
    onestrb.worefresh()
    onRefresh()
  }

  onestrb.worefresh <- function(){
    if (!tclvalue(estrbVariable)=="none"){
      tkconfigure(selectButton, state="normal")
      tkconfigure(deselectButton, state="normal")
      tkconfigure(comprestrb, state="normal")
      tkconfigure(manualestrb, state="normal")
      tkconfigure(comprclassEntry, state="normal")
    }
    else {
      tkconfigure(selectButton, state="disabled")
      tkconfigure(deselectButton, state="disabled")
      tkconfigure(comprestrb, state="disabled")
      tkconfigure(manualestrb, state="disabled")
      tkconfigure(comprclassEntry, state="disabled")
    }
  }


  oncomprestrb <- function(){
    oncomprestrb.worefresh
    onRefresh()
  }
  oncomprestrb.worefresh <- function(){
    if (tclvalue(comprrbVariable)=="compr"){
      tkconfigure(comprclassEntry, state="normal")
    }
    else {
      tkconfigure(comprclassEntry, state="disabled")
    }
  }

  onSpecialcb <- function(){
    if (tclvalue(specialcbVariable)=="0"){
      if (tabpos() %in% c(2,3,4)) tcl(tn,"select",0)
      putRcmdr("estrbVariable", tclVar("none"))
      putRcmdr("specialrbVariable", tclVar("none"))
      putRcmdr("hardVar", tclVar("0"))
      putRcmdr("genVar", tclVar("NULL"))
      putRcmdr("catlgVar", tclVar("catlg"))
      putRcmdr("designVar", tclVar("NULL"))
      putRcmdr("estimable2fis", "")
      putRcmdr("estrbVariable", tclVar("none"))
    }
    tkconfigure(noestrb, variable=estrbVariable)
    tkconfigure(clearrb, variable=estrbVariable)
    tkconfigure(distinctrb, variable=estrbVariable)
    tkconfigure(defaultrb, variable=designrbVariable)
    tkconfigure(genrb, variable=designrbVariable)
    tkconfigure(catlgrb, variable=designrbVariable)
    tkconfigure(designrb, variable=designrbVariable)
    tkconfigure(genEntry, textvariable=genVar)
    tkconfigure(catlgEntry, textvariable=catlgVar)
    tkconfigure(designEntry, textvariable=designVar)
    # tkconfigure(nonerb, variable=specialrbVariable)
    # tkconfigure(hardrb, variable=specialrbVariable)
    # tkconfigure(debarrb, variable=specialrbVariable)
    onRefresh()
  }

  onStore <- function(){
    ## Speichernamen abfragen und hier ermöglichen (statt stored.design2)
    textentry() ## creates text string stored in savename.RcmdrPlugin.DoE
    if (!is.null(savename.RcmdrPlugin.DoE)){
      if (!is.valid.name(savename.RcmdrPlugin.DoE)) {
        textcorrect(gettext("This is not a valid name. Please correct:"))
        return()
      }
      if (is.element(savename.RcmdrPlugin.DoE, listObjects()))
      {
        if ("no" == tclvalue(checkReplace(savename.RcmdrPlugin.DoE, gettext("Object"))))
        {
          textcorrect(gettext("Please enter a new name:"))
          return()
        }
      }
      storeRcmdr()
      ## replace assign by justDoIt; assign(savename.RcmdrPlugin.DoE, getRcmdr(".stored.design2FrF"), envir=.GlobalEnv)
      justDoIt(paste(savename.RcmdrPlugin.DoE, "<- getRcmdr(\".stored.design2FrF\")"))
      message(gettext("inputs have been stored"))
    }
  }

  onReset <- function(){
    assign(".stored.design2FrF",.default.design2,pos="RcmdrEnv")
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.FrF2level()
  }

  nfacchange <- function(){
    nfacold <- length(as.character(tclObj(varlistshort)))
    nfacnew <- as.numeric(tclvalue(nfacVar))
    if (nfacold==nfacnew) return()
    if (as.logical(as.numeric(as.character(tclvalue(nrunEntryVariable)))))
      putRcmdr("infoknopftext", tclVar(paste(gettext("Show best 10 designs for"), tclvalue(nfacVar),
                                             gettext("factors in"), tclvalue(nrunVar), gettext("runs\n     The menu remains open, \n     fetch it back after looking at designs"),sep=" ")))
    else
      putRcmdr("infoknopftext", tclVar(paste(gettext("Show best 10 designs for"), tclvalue(nfacVar),
                                             gettext("factors\n     The menu remains open, \n     fetch it back after looking at designs"),sep=" ")))
    tkconfigure(infoButton1, textvariable=infoknopftext)
    if (nfacnew < nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[1:nfacnew]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[1:nfacnew]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[1:nfacnew]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[1:nfacnew]))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(faclev1ListBox, listvariable=faclev1list, height=min(10,nfacnew))
      tkconfigure(faclev2ListBox, listvariable=faclev2list, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      if (selpos > nfacnew){
        tcl(fsel, "current", "0")
        factorsel()
      }
      if (tclvalue(specialcbVariable)=="1") onRefresh()
    }
    if (nfacnew > nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist",tclVar(c(as.character(tclObj(facnamlist)),
                                     getRcmdr("varlistshortt")[(nfacold+1):nfacnew])))
      putRcmdr("faclev1list", tclVar(c(as.character(tclObj(faclev1list)),
                                       rep(tclvalue(level1Var),nfacnew-nfacold))))
      putRcmdr("faclev2list", tclVar(c(as.character(tclObj(faclev2list)),
                                       rep(tclvalue(level2Var),nfacnew-nfacold))))
      putRcmdr("faclablist", tclVar(c(as.character(tclObj(faclablist)),
                                      rep("",nfacnew-nfacold))))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      tkconfigure(faclev1ListBox, listvariable=faclev1list, height=min(10,nfacnew))
      tkconfigure(faclev2ListBox, listvariable=faclev2list, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))
      if (tclvalue(specialcbVariable)=="1") onRefresh()
    }
  }
  nameenter <- function(){
    if (identical(tclvalue(getRcmdr("fileVar")),tclvalue(getRcmdr("nameVar"))))
      putRcmdr("name.equal.filename", TRUE)
    else putRcmdr("name.equal.filename", FALSE)
  }
  namechange <- function(){
    if (is.valid.name(tclvalue(nameVar))){
      if (name.equal.filename){
        putRcmdr("fileVar", tclVar(tclvalue(nameVar)))  ## otherwise, variables would be directly tied
        #          putRcmdr("exportlabVar", tclVar(paste("Current design to be saved:", tclvalue(nameVar),"\n   ")))  ## otherwise, variables would be directly tied
        tkconfigure(fileEntry, textvariable=getRcmdr("fileVar"))
        #          tkconfigure(exportlab, textvariable=getRcmdr("exportlabVar"))
      }
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid design name")
  }

  catlgnamechange <- function(){
    if (is.valid.name(tclvalue(catlgVar))){
      if (!exists(tclvalue(catlgVar)))
        tkmessageBox(message="catalogue does not exist",icon="error", type="ok", title="Specified catalogue does not exist")
      else{
        if (!"catlg" %in% class(get(tclvalue(catlgVar))))
          tkmessageBox(message="is not a design catalogue of class catlg",icon="error", type="ok", title="invalid catalogue specified")
        else onRefresh()
        #              else { pick <- nfac(get(tclvalue(catlgVar)))==as.numeric(tclvalue(nfacVar))
        #                    if (tclvalue(resVar)=="IV") pick <- pick & res(get(tclvalue(catlgVar))) > 3 else
        #                    if (tclvalue(resVar)=="V+") pick <- pick & res(get(tclvalue(catlgVar))) > 4
        #                    if (tclvalue(nrunEntryVariable)=="1") pick <- pick & nruns(get(tclvalue(catlgVar))) == as.numeric(tclvalue(nrunVar))
        #                    if (any(pick)) putRcmdr("catlgliste", names(get(tclvalue(catlgVar)))[pick])
        #                    else putRcmdr("catlgliste", "")
        #                    tkconfigure(designEntry, values=catlgliste)
        #              }
      }
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid design name")
  }

  nrunnow <- function(){
    putRcmdr("nrunOld", tclvalue(nrunVar))
  }
  nrunchange <- function(){
    if (!tclvalue(nrunVar)==nrunOld){
      if(!2^round(log2(as.numeric(tclvalue(nrunVar))))==as.numeric(tclvalue(nrunVar))){
        tk_messageBox(caption="invalid run number",message = gettext("invalid run number, must be power of 2"), type = "ok")
        return()}
      if (as.logical(as.numeric(as.character(tclvalue(nrunEntryVariable)))))
        putRcmdr("infoknopftext", tclVar(paste(gettext("Show best 10 designs for"), tclvalue(nfacVar),
                                               gettext("factors in"), tclvalue(nrunVar), gettext("runs\n     The menu remains open, \n     fetch it back after looking at designs"),sep=" ")))
      else
        putRcmdr("infoknopftext", tclVar(paste(gettext("Show best 10 designs for"), tclvalue(nfacVar),
                                               gettext("factors\n     The menu remains open, \n     fetch it back after looking at designs"),sep=" ")))
      tkconfigure(infoButton1, textvariable=infoknopftext)
      #onRefresh()
    }
  }
  factorsel<-function(){
    #### aendert die in der Textbox dargestellte Auswahl
    #### ruiniert aber leider auch wieder die korrekte Ueberschreibung der Werte
    putRcmdr("selpos", as.numeric(tclvalue(tcl(fsel, "current")))+1)
    putRcmdr("curfac", tclVar(as.character(tclObj(varlistshort))[selpos]))
    putRcmdr("curfnam", tclVar(as.character(tclObj(facnamlist))[selpos]))
    putRcmdr("curflev1", tclVar(as.character(tclObj(faclev1list))[selpos]))
    putRcmdr("curflev2", tclVar(as.character(tclObj(faclev2list))[selpos]))
    putRcmdr("curflab", tclVar(as.character(tclObj(faclablist))[selpos]))
    tkconfigure(fnam, textvariable=curfnam)
    tkconfigure(flev1, textvariable=curflev1)
    tkconfigure(flev2, textvariable=curflev2)
    tkconfigure(flab, textvariable=curflab)
  }
  fnamchange <- function(){
    ## selpos known from factorsel
    if (is.valid.name(tclvalue(curfnam))){
      hilf <- as.character(tclObj(facnamlist))
      hilf[selpos] <- tclvalue(curfnam)
      putRcmdr("facnamlist",tclVar(hilf))
      ### "facnamlist" is not automatically updated in the listbox
      ### therefore the tkconfigure
      tkconfigure(facnameListBox, listvariable=facnamlist)
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid factor name")
  }

  level1enter <- function(){
    putRcmdr("the.common.level1", tclvalue(getRcmdr("level1Var")))
  }
  level1change <- function(){
    if (identical(getRcmdr("the.common.level1"), tclvalue(getRcmdr("level1Var")))) return()
    onRefresh()
  }
  level2enter <- function(){
    putRcmdr("the.common.level2", tclvalue(getRcmdr("level2Var")))
  }
  level2change <- function(){
    if (identical(getRcmdr("the.common.level2"), tclvalue(getRcmdr("level2Var")))) return()
    onRefresh()
  }


  flev1change <- function(){
    ## selpos known from factorsel
    if (length(as.character(tclObj(curflev1)))==1){
      hilf <- as.character(tclObj(faclev1list))
      hilf[selpos] <- tclvalue(curflev1)
      putRcmdr("faclev1list",tclVar(hilf))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
    }
    else tkmessageBox(message="Empty entries or entries with blanks are not permitted, please correct!",
                      icon="error", type="ok", title="Invalid factor level")
  }
  flev2change <- function(){
    ## selpos known from factorsel
    if (length(as.character(tclObj(curflev2)))==1){
      hilf <- as.character(tclObj(faclev2list))
      hilf[selpos] <- tclvalue(curflev2)
      putRcmdr("faclev2list",tclVar(hilf))
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
    }
    else tkmessageBox(message="Empty entries or entries with blanks are not permitted, please correct!",
                      icon="error", type="ok", title="Invalid factor level")
  }
  flabchange <- function(){
    ## selpos known from factorsel
    ## for FocusOut event on flab
    ## still problematic, if Focus out occurs with tab
    ## as there is also a tab key event
    hilf <- as.character(tclObj(faclablist))
    hilf[selpos] <- tclvalue(curflab)
    ### updating hilf does work
    ### but "varlist" is not automatically updated in the listbox
    ### therefore the tkconfigure
    putRcmdr("faclablist",tclVar(hilf))
    tkconfigure(faclabListBox, listvariable=faclablist)
  }

  tabflab <- function(){
    ## for Tab key event on flab
    ## the traversal still jumps to the first traversable control on the sheet
    ## (rather than staying with fnam, if asked by tkfocus to do so)
    ## takefocus has so far been set to 0 for all widgets except the factor detail ones on this tab
    flabchange()  ## otherwise, not carried out!
    hilf <- as.numeric(tclvalue(tcl(fsel,"current")))+1
    if (hilf  >= as.numeric(tclvalue(nfacVar))) return()
    tcl(fsel,"current", hilf)
    factorsel()
    #tkfocus(fnam)
    #tcl(fnam, "selection", "range", 1, "end")
    #tcl("break")
  }


  swap <- function(a,b){
    hilf <- 1:as.numeric(tclvalue(nfacVar))
    hilf[b] <- a
    hilf[a] <- b
    hilf
  }

  indexchange <- function(){
    if (curindex < as.numeric(tclvalue(nfacVar)))
      putRcmdr("orderDown",swap(curindex, curindex+1))
    if (curindex > 1)
      putRcmdr("orderUp",swap(curindex, curindex-1))
    tcl(fsel, "current", curindex-1)
    factorsel()
  }

  checkIndexShort <- function(){
    putRcmdr("curindex", as.numeric(tcl(facshortListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexNam <- function(){
    putRcmdr("curindex", as.numeric(tcl(facnameListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLev1 <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclev1ListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLev2 <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclev2ListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLab <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclabListBox,"curselection"))+1)
    indexchange()
  }


  onUp <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex=="1" | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderUp]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[orderUp]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[orderUp]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderUp]))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex-1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(faclev1ListBox,"selection","set",curindex-1)
      tcl(faclev2ListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  onDown <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex==as.numeric(tclvalue(nfacVar)) | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderDown]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[orderDown]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[orderDown]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderDown]))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex+1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(faclev1ListBox,"selection","set",curindex-1)
      tcl(faclev2ListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  dquote <- function(obj){
    ## quote vector elements for use as character vector in a command
    aus <- rep("",length(obj))
    wopt <- options("warn")[[1]]
    options(warn=-1)
    for (i in 1:length(obj)) if (is.na(as.numeric(obj[i]))) {
      if (length(grep('"',obj[i])>0))
        aus[i] <- paste("'",obj[i],"'",sep="")
      else
        aus[i] <- paste('"',obj[i],'"',sep="")
    }
    else aus[i] <- obj[i]
    options(warn=wopt)
    aus
  }


  onInfo <- function(){
    if (tclvalue(qualcritrbVariable)=="MA") onMAinfo()
    else onMaxC2info()
  }

  onInfo1 <- function(){
    ## all designs for nfacVar factors in nrunVar runs
    resmin <- 3
    if (tclvalue(resVar)=="IV") resmin <- 4
    if (tclvalue(resVar)=="V+") resmin <- 5
    if (!as.logical(as.numeric(as.character(tclvalue(nrunEntryVariable)))))
      nrun <- "all"
    else nrun <- tclvalue(nrunVar)
    command <- paste("print(",tclvalue(catlgVar),", nruns =",dquote(nrun),
                     ", nfactors =", tclvalue(nfacVar),
                     ", show.alias = TRUE, MaxC2 =", as.logical(tclvalue(qualcritrbVariable)=="MaxC2"),
                     ", res.min =", resmin, ")")
    hilf <- justDoItDoE(command)
    if (class(hilf)[1]=="try-error") {
      if (!as.numeric(tclvalue(nfacVar))<=round(log2(as.numeric(tclvalue(nrunVar)))))
        logger("# There is no such design in the catalogue. See button Available designs.")
      if (as.numeric(tclvalue(nfacVar))==round(log2(as.numeric(tclvalue(nrunVar)))))
        logger("# This is a full factorial design.")
      if (as.numeric(tclvalue(nfacVar))<round(log2(as.numeric(tclvalue(nrunVar)))))
        logger("# This is a replicated full factorial design.")
      return()
    }
    ## logger(command)  ## braucht man nur für justDoIt
    tkgrab.release(topdes2)
    doItAndPrint(command)
  }

  onInfo2 <- function(){
    ## generators for current designs
    command="dc()"
    ## logger(command)  ## braucht man nur für justDoIt
    doItAndPrint(command)
  }

  onInfo3 <- function(){
    ## alias pattern for current designs
    command="dc()"
    ## logger(command)  ## braucht man nur für justDoIt
    doItAndPrint(command)
  }

  curdes <- function(){
    ## find current design
  }

  onMAinfo <- function(){
    putRcmdr("info.window",tktoplevel())
    tkwm.title(info.window, gettext("Requested information"))
    position <- if (is.SciViews())
      -1
    else position <- "+50+50"
    tkwm.geometry(info.window, position)

    tcinfo <- ttklabel(info.window,text="You must CLOSE this window for continuing work with the design dialogue.")
    tkgrid(tcinfo)
    tcl("image",  "create", "photo", "resolution.image",
        file=system.file( "images", "resolutionimage.gif", package = "RcmdrPlugin.DoE" ) )
    tc <- tklabel(info.window, image="resolution.image")
    tkgrid(tc)
    #catlginfo <- ttklabel(info.window,text="Additional regular designs from this menu:")
    #catlginfo1 <- ttklabel(info.window,text="32 runs (up to 31 factors, res. III), 64 runs (up to 32 factors res. IV, 33 to 63 factors res. III),")
    #catlginfo2 <- ttklabel(info.window,text="128 runs (up to 64 factors, res. IV, 65 to 127 factors, res. III)")
    catlginfo3 <- ttklabel(info.window,text="Additional irregular resolution V designs from general orthogonal arrays menu:")
    catlginfo4 <- ttklabel(info.window,text="128 runs (up to 15 factors), 256 runs (up to 19 factors), 2048 runs (up to 63 factors)")

    #tkgrid(catlginfo,sticky="w")
    #tkgrid(catlginfo1)
    #tkgrid(catlginfo2)
    tkgrid(catlginfo3, sticky="w")
    tkgrid(catlginfo4)

    dialogSuffix(window=info.window, rows=1, columns=1,
                 focus=tc,
                 onOK=function() "")
  }

  onMaxC2info <- function(){
    if (tclvalue(resVar)=="III") tcl("image",  "create", "photo", "MaxC2.image",
                                     file=system.file( "images", "MaxC2res3image.gif", package = "RcmdrPlugin.DoE" )  )
    else if (tclvalue(resVar)=="IV") tcl("image",  "create", "photo", "MaxC2.image",
                                         file=system.file( "images", "MaxC2res4image.gif", package = "RcmdrPlugin.DoE" ))
    else {tkmessageBox(message="All 2fis are clear for designs of resolution V or more.")
      return()}
    putRcmdr("info.window",tktoplevel())
    tkwm.title(info.window, gettext("Requested information"))
    position <- if (is.SciViews())
      -1
    else position <- "+50+50"
    tkwm.geometry(info.window, position)


    tcinfo <- ttklabel(info.window,text="You must CLOSE this window for continuing work with the design dialogue.")
    tkgrid(tcinfo)
    if (tclvalue(resVar)=="III") tkgrid(ttklabel(info.window,text="NOTE: red: resolution III, yellow: resolution IV, green: resolution V+"))
    if (tclvalue(resVar)=="IV") tkgrid(ttklabel(info.window,text="NOTE: yellow: resolution IV, green: resolution V+"))
    tc <- tklabel(info.window, image="MaxC2.image")
    tkgrid(tc)
    if (tclvalue(resVar)=="III") catlginfo <- ttklabel(info.window,text=gettext("Cell entries for resolution III and IV designs:"))
    else catlginfo <- ttklabel(info.window,text=gettext("Cell entries for resolution IV designs:"))
    catlginfo2 <- ttklabel(info.window,text=gettext("maximum number of clear 2fis and maximum number of factors with all 2fis clear."))
    tkgrid(catlginfo)
    tkgrid(catlginfo2)

    dialogSuffix(window=info.window, rows=1, columns=1,
                 focus=tc,
                 onOK=function() "")
  }

  onChangeDir <- function(){
    putRcmdr("direct",tclvalue(tkchooseDirectory()))
    if (!direct=="") {
      putRcmdr("dirVar", tclVar(direct))
      tkconfigure(dirEntry, textvariable = dirVar)
    }
  }

  onSelect <- function(){
    if (tclvalue(tcl(notest2fis$listbox, "curselection"))=="") return()
    ## curselection is a character string with blank separated selection positions
    add <- notest2fislist[as.numeric(unlist(strsplit(tclvalue(tcl(notest2fis$listbox, "curselection")), " ")))+1]
    putRcmdr("notest2fislist", setdiff(notest2fislist,add))
    putRcmdr("est2fislist", intaclistt[intaclistt %in% c(est2fislist,add)])
    add <- NULL
    tcl(notest2fis$listbox, "selection", "clear", "0", "999")
    tkconfigure(notest2fis$listbox, listvariable=tclVar(paste(notest2fislist,collapse=" ")))
    notest2fis$varlist <- notest2fislist
    tkconfigure(est2fis$listbox, listvariable=tclVar(paste(est2fislist,collapse=" ")))
    est2fis$varlist <- est2fislist
  }
  onDeselect <- function(){
    ## curselection is a character string with blank separated selection positions
    if (tclvalue(tcl(est2fis$listbox, "curselection"))=="") return()
    add <- est2fislist[as.numeric(unlist(strsplit(tclvalue(tcl(est2fis$listbox, "curselection")), " ")))+1]
    putRcmdr("est2fislist", setdiff(est2fislist,add))
    putRcmdr("notest2fislist", intaclistt[intaclistt %in% c(notest2fislist,add)])
    add <- NULL
    tcl(est2fis$listbox, "selection", "clear", "0", "999")
    tkconfigure(notest2fis$listbox, listvariable=tclVar(paste(notest2fislist,collapse=" ")))
    notest2fis$varlist <- notest2fislist
    tkconfigure(est2fis$listbox, listvariable=tclVar(paste(est2fislist,collapse=" ")))
    est2fis$varlist <- est2fislist
  }
  ######## end define functions


  ##### define userform
  #tn <- ttknotebook(top,height=100, width=500)

  putRcmdr("tn",ttknotebook(topdes2))
  #tn <- ttknotebook(topdes2)

  putRcmdr("tab1",ttkframe(tn))
  putRcmdr("tab2",ttkframe(tn))
  putRcmdr("tab3",ttkframe(tn))
  putRcmdr("tab4",ttkframe(tn))
  putRcmdr("tab5",ttkframe(tn))
  putRcmdr("tab6",ttkframe(tn))

  tkadd(tn,tab1,text="Base Settings")   ### tabid=0
  tkadd(tn,tab2,text="Factor Details")  ### tabid=1
  tkadd(tn,tab3,text="Estimable Model") ### tabid=2
  tkadd(tn,tab4,text="Block & Split-Plot") ### tabid=3
  tkadd(tn,tab5,text="Special") ### tabid=4
  tkadd(tn,tab6,text="Export") ### tabid=5

  tkconfigure(tn, takefocus=0)

  ## define canvas for showing possibilities
  ### image file location must be changed!!!
  ### widgets for tab1 and base frame
  putRcmdr("nameVar", tclVar(.stored.design2FrF$nameVar))
  nameEntry <- tkentry(tab1, width="20", textvariable=nameVar)
  tkbind(nameEntry, "<FocusIn>", nameenter)
  tkbind(nameEntry, "<FocusOut>", namechange)
  ### wird pbcbVariable noch gebraucht ?
  ##pbcbVariable <- tclVar(.stored.design2FrF$cbInitials[8])
  #pbcb <- ttkcheckbutton(desinfoFrame,text=gettext("Screening design (Plackett-Burman)"),variable=pbcbVariable)

  baseFrame <- ttklabelframe(tab1,text=gettext("Size and randomization"))

  nrunVar <- tclVar(.stored.design2FrF$nrunVar)
  nrunEntry <- tkentry(baseFrame, width="8", textvariable=nrunVar)
  tkbind(nrunEntry,"<FocusIn>",nrunnow)
  tkbind(nrunEntry,"<FocusOut>",nrunchange)
  nfacVar <- tclVar(.stored.design2FrF$nfacVar)
  nfacEntry <- tkentry(baseFrame, width="8", textvariable=nfacVar)
  tkbind(nfacEntry,"<FocusOut>",nfacchange)
  ncenterVar <- tclVar(.stored.design2FrF$ncenterVar)
  ncenterEntry <- tkentry(baseFrame, width="8", textvariable=ncenterVar)
  nblockVar <- tclVar(.stored.design2FrF$nblockVar)
  nblockEntry <- tkentry(baseFrame, width="8", textvariable=nblockVar)
  aliasblock2fiVariable <- tclVar(.stored.design2FrF$cbInitials[3])
  aliasblock2fi <- ttkcheckbutton(baseFrame,text=gettext("blocks may be \naliased with 2fis"),variable=aliasblock2fiVariable)
  tkconfigure(aliasblock2fi, takefocus=0)
  nrepVar <- tclVar(.stored.design2FrF$nrepVar)
  nrepEntry <- tkentry(baseFrame, width="8", textvariable=nrepVar)
  randomizeVariable <-  tclVar(.stored.design2FrF$cbInitials[2])
  randomizecb <- ttkcheckbutton(baseFrame,text=gettext("Randomization"),variable=randomizeVariable)
  tkconfigure(randomizecb, takefocus=0)
  seedVar <- tclVar(sample(31999,1))  ## always new
  seedEntry <- tkentry(baseFrame, width="8", textvariable=seedVar)
  tkconfigure(seedEntry, takefocus=0)
  nrunEntryVariable <- tclVar(.stored.design2FrF$cbInitials[5])
  nruncb <- ttkcheckbutton(baseFrame,text=gettext("Specify nruns"),variable=nrunEntryVariable, command=onRefresh)
  tkconfigure(nruncb, takefocus=0)
  repeat.onlyVariable <- tclVar(.stored.design2FrF$cbInitials[1])
  repeat.onlycb <- ttkcheckbutton(baseFrame,text=gettext("Repeat only"),variable=repeat.onlyVariable)
  tkconfigure(repeat.onlycb, takefocus=0)

  despropFrame <- ttklabelframe(tab1,text="Design properties")
  descritFrame <- ttkframe(despropFrame)
  desinfoFrame <- ttkframe(despropFrame)

  resVar <- tclVar(.stored.design2FrF$resVar)
  #resEntry <- tkentry(despropframe, textvariable=resVar)
  resEntry <- ttkcombobox(descritFrame, textvariable=resVar, width=5, values=c("III","IV","V+"), state="readonly")
  tkbind(resEntry, "<<ComboboxSelected>>", onRefresh)

  qualcritrbVariable <- tclVar(.stored.design2FrF$qualcritrbVariable)
  MArb <- tkradiobutton(descritFrame,text=gettext("MA (Maximum resolution and minimum aberration)"),variable=qualcritrbVariable,value="MA")
  MaxC2rb <- tkradiobutton(descritFrame,text=gettext("MaxC2 (Maximum number of clear 2fis)"),variable=qualcritrbVariable,value="MaxC2")

  ## grid descritFrame
  tkgrid(resEntry, tklabel(descritFrame,text=gettext("Minimum resolution\nNOTE: affects design generation\nfor MaxC2 choice\nOR unspecified number of runs only"),justify="left"))
  #tkgrid(tklabel(descritFrame,text=gettext("NOTE: affects design generation for MaxC2 choice")), columnspan=2, sticky="w")
  #tkgrid(tklabel(descritFrame,text=gettext("OR unspecified number of runs only")), columnspan=2, sticky="w")

  tkgrid(MArb, columnspan=2, sticky="w")
  tkgrid(MaxC2rb, columnspan=2, sticky="w")
  tkgrid(tklabel(descritFrame,text=gettext("  ")), columnspan=2)

  if (as.logical(as.numeric(as.character(tclvalue(nrunEntryVariable)))))
    putRcmdr("infoknopftext", tclVar(paste(gettext("Show best 10 designs for"), tclvalue(nfacVar),
                                           gettext("factors in"), tclvalue(nrunVar), gettext("runs\n     The menu remains open, \n     fetch it back after looking at designs"),sep=" ")))
  else
    putRcmdr("infoknopftext", tclVar(paste(gettext("Show best 10 designs for"), tclvalue(nfacVar),
                                           gettext("factors\n     The menu remains open, \n     fetch it back after looking at designs"),sep=" ")))

  infoButton <- buttonRcmdr(desinfoFrame, text = gettext("Show available designs"),
                            foreground = "darkgreen", command = onInfo,
                            default = "normal", borderwidth = 3)
  infoButton1 <- buttonRcmdr(desinfoFrame, textvariable = infoknopftext,
                             foreground = "darkgreen", command = onInfo1,
                             default = "normal", borderwidth = 3)
  infoButton2 <- buttonRcmdr(desinfoFrame, text = gettext("Show generators for current design"),
                             foreground = "darkgreen", command = onInfo2,
                             default = "normal", borderwidth = 3)
  infoButton3 <- buttonRcmdr(desinfoFrame, text = gettext("Show alias pattern for current design"),
                             foreground = "darkgreen", command = onInfo3,
                             default = "normal", borderwidth = 3)

  ## grid desinfoFrame
  tkgrid(infoButton,sticky="we", columnspan=2)
  tkgrid(infoButton1,sticky="we", columnspan=2)
  #tkgrid(infoButton2,sticky="we", columnspan=2)
  #tkgrid(infoButton3,sticky="we", columnspan=2)

  ## grid despropFrame
  tkgrid(descritFrame, desinfoFrame, sticky="w", columnspan=3)

  ## grid design frame
  ## (for expert choices)
  designFrame <- ttklabelframe(tab1,text=gettext("Expert choices"))
  ### widgets for design frame

  catlab <- tklabel(designFrame, text=gettext("Catalogue of designs:"))
  putRcmdr("catlgVar", tclVar(.stored.design2FrF$catlgVar))
  ## set to valid default, if invalid entry
  if (!exists(tclvalue(catlgVar))){
    putRcmdr("catlgVar", tclVar("catlg"))
    message("The specified design catalogue does not exist and has been replaced by the default!")
  }
  else{
    if (!"catlg" %in% class(get(tclvalue(catlgVar)))){
      putRcmdr("catlgVar", tclVar("catlg"))
      message("The specified design catalogue is invalid and has been replaced by the default!")
    }
  }
  designrbVariable <- tclVar(.stored.design2FrF$designrbVariable)
  designrbFrame <- ttkframe(designFrame)
  defaultrb <- tkradiobutton(designrbFrame,text=gettext("None"),variable=designrbVariable,value="default", command=onRefresh)
  genrb <- tkradiobutton(designrbFrame,text=gettext("Specify Generators (generators option)"),variable=designrbVariable,value="gen", command=onRefresh)
  catlgrb <- tkradiobutton(designrbFrame,text=gettext("Specify catalogue name (select.catlg option)"),variable=designrbVariable,value="catlg", command=onRefresh)
  designrb <- tkradiobutton(designrbFrame,text=gettext("Specify Design name (design option)"),variable=designrbVariable,value="design", command=onRefresh)
  tkgrid(defaultrb,sticky="w")
  tkgrid(genrb,sticky="w")
  tkgrid(catlgrb,sticky="w")
  tkgrid(designrb,sticky="w")
  tkgrid(tklabel(designrbFrame,text="  "))

  genVar <- tclVar(.stored.design2FrF$genVar)
  designVar <- tclVar(.stored.design2FrF$designVar)
  genEntry <- tkentry(designFrame, textvariable=genVar, width=50)

  tkgrid(designrbFrame,columnspan=4,sticky="n")

  ## prepare list of catalogue names
  ## catlgliste (without s) is list of catlg entries of current catalogue
  putRcmdr("catlgsliste", listCatlgs())
  catlgEntry <- ttkcombobox(designFrame, textvariable=catlgVar, width=24, values=catlgsliste)
  tkbind(catlgEntry, "<<ComboboxSelected>>", catlgnamechange)


  ## prepare list of design names, based on current setting for the catalogue
  pick <- nfac(get(tclvalue(catlgVar)))==as.numeric(tclvalue(nfacVar))
  if (tclvalue(resVar)=="IV") pick <- pick & res(get(tclvalue(catlgVar))) > 3 else
    if (tclvalue(resVar)=="V+") pick <- pick & res(get(tclvalue(catlgVar))) > 4
  if (tclvalue(nrunEntryVariable)=="1") pick <- pick & nruns(get(tclvalue(catlgVar))) == as.numeric(tclvalue(nrunVar))
  if (any(pick)) putRcmdr("catlgliste", names(get(tclvalue(catlgVar)))[pick])
  else putRcmdr("catlgliste", "")
  if (!tclvalue(designVar) %in% catlgliste & length(catlgliste) > 0) designVar <- tclVar(catlgliste[1])


  designEntry <- ttkcombobox(designFrame, textvariable=designVar, values=catlgliste, width=24, state="readonly")

  if (tclvalue(designrbVariable)=="gen")
  {   tkgrid(tklabel(designFrame,text=gettext("Type in generators")), columnspan=2, sticky="w")
    tkgrid(genEntry, columnspan=2)
    tkgrid(tklabel(designFrame,text=gettext("comma-separated column numbers of Yates matrix (e.g. 7, 13, 11)\nor comma-separated interaction columns (e.g. ABC, ACD, ABD)"), wraplength=500),columnspan=2)
  }
  if (tclvalue(designrbVariable)=="catlg")
  {  tkgrid(catlab,catlgEntry,sticky="nw")
  }
  if (tclvalue(designrbVariable)=="design")
  {  tkgrid(catlab,catlgEntry,sticky="nw")
    tkgrid(tklabel(designFrame,text=gettext("Select design\n(preselected according to numbers of runs and factors and resolution):"), wraplength=500, justify="left"),columnspan=2, sticky="w")
    tkgrid(designEntry, sticky="w", padx=15)
  }

  ## preparations for bottom frame
  bottomFrame <- tkframe(topdes2)
  specialcbVariable <- tclVar(.stored.design2FrF$cbInitials[7])
  specialcb <- ttkcheckbutton(bottomFrame,text=gettext("Activate Special Choices"),
                              variable=specialcbVariable, command = onSpecialcb)


  helptab1Button <- buttonRcmdr(tab1, text = gettext("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab1,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab1Button, takefocus=0)


  ### Finalize tab1
  tkgrid(tklabel(tab1, text="Name of new design"), nameEntry, helptab1Button, sticky="w", pady=10)
  tkgrid.configure(helptab1Button, sticky="e")

  ## grid base frame
  tkgrid(nrunlab <- tklabel(baseFrame, text=gettext("Number of runs")), nrunEntry, nruncb, sticky="w")
  ## omitted nfaccb, on form, nfactors must always be specified
  tkgrid(nfaclab <- tklabel(baseFrame, text=gettext("Number of factors")), nfacEntry, sticky="w")
  tkgrid(ncenterlab <- tklabel(baseFrame, text=gettext("Number of center points")), ncenterEntry, sticky="w")
  tkgrid.configure(ncenterlab, pady=10)
  tkgrid(tklabel(baseFrame, text=gettext("Number of blocks")), nblockEntry, aliasblock2fi, sticky="w")
  tkgrid(tklabel(baseFrame, text=gettext("Replications")), nrepEntry, repeat.onlycb, sticky="w",pady=5)
  tkgrid(tklabel(baseFrame, text="You normally do not need to change randomization settings"),sticky="w",columnspan=3)
  tkgrid(seedlab <- tklabel(baseFrame, text=gettext("Seed for randomization")), seedEntry,
         randomizecb, sticky="w")



  ## base FrF2
  ## expert choices for FrF2 in case of special
  if (as.logical(as.numeric(tclvalue(specialcbVariable)))){
    tkgrid(baseFrame, designFrame, sticky="w", columnspan=2)
    #tkgrid.configure(baseFrame, columnspan=2)
  }
  else tkgrid(baseFrame,sticky="w",columnspan=2)
  ## design properties with lookup
  tkgrid(despropFrame, sticky="w",columnspan=3, pady=10)

  ## Factor Details Tab
  ## factor details frame
  ### faclevelCommonVariable

  ## default levels frame
  deflevFrame <- ttklabelframe(tab2,text="Default levels")
  faclevelCommonVariable <- tclVar(.stored.design2FrF$cbInitials[4])
  faclevelCommonButton <- ttkcheckbutton(deflevFrame,text=gettext("Common factor levels"),
                                         variable=faclevelCommonVariable,command=onRefresh)
  tkconfigure(faclevelCommonButton,takefocus=0)
  putRcmdr("level1Var", tclVar(.stored.design2FrF$level1Var))
  level1Entry <- ttkentry(deflevFrame, width="20", textvariable=level1Var)
  tkconfigure(level1Entry,takefocus=0)
  tkbind(level1Entry, "<FocusIn>", level1enter)
  tkbind(level1Entry, "<FocusOut>", level1change)

  putRcmdr("level2Var", tclVar(.stored.design2FrF$level2Var))
  level2Entry <- tkentry(deflevFrame, width="20", textvariable=level2Var)
  tkconfigure(level2Entry,takefocus=0)
  tkbind(level2Entry, "<FocusIn>", level2enter)
  tkbind(level2Entry, "<FocusOut>", level2change)
  tkgrid(faclevelCommonButton,sticky="w",columnspan=3, padx=15)
  faclevCommonLab<-tklabel(deflevFrame,text=gettext("CAUTION: Checking this box overwrites all custom factor levels."))
  if (!as.logical(as.numeric(tclvalue(faclevelCommonVariable)))) tkgrid(faclevCommonLab,sticky="w", columnspan=3,pady=5)
  tkgrid(tklabel(deflevFrame, text=gettext("First Level")),tklabel(deflevFrame, text=gettext("Second Level")),sticky="e",padx=15)
  tkgrid(level1Entry, level2Entry, sticky="e",padx=15)

  ## factor details
  ## values as vectors
  facnamlistt <- .stored.design2FrF$facnamlist
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))) {
    faclev1listt <- rep(tclvalue(level1Var),tclvalue(nfacVar))
    faclev2listt <- rep(tclvalue(level2Var),tclvalue(nfacVar))
  } else{
    faclev1listt <- .stored.design2FrF$faclev1list
    faclev2listt <- .stored.design2FrF$faclev2list
  }
  faclablistt <- .stored.design2FrF$faclablist
  varlistshortt <- if (as.numeric(tclvalue(nfacVar))<=50)
    Letters[1:tclvalue(nfacVar)] else paste("F",1:tclvalue(nfacVar),sep="")

  ### this deletes all user entries, if user changes number of factor levels after
  ### inputting everything
  ### and all updating facilities go wrong
  #if (!length(facnamlistt)==length(varlistshortt)){
  #       faclev1listt <- rep(tclvalue(level1Var),tclvalue(nfacVar))
  #       faclev2listt <- rep(tclvalue(level2Var),tclvalue(nfacVar))
  #       facnamlistt <- varlistshortt
  #       faclablistt <- rep("",as.numeric(tclvalue(nfacVar)))
  #    }
  enterlistFrame <- ttkframe(tab2)
  listFrame <- ttklabelframe(enterlistFrame, text="Factor Details")
  putRcmdr("selpos", 1)
  putRcmdr("curfac", tclVar(varlistshortt[1]))
  putRcmdr("curfnam", tclVar(facnamlistt[1]))
  putRcmdr("curflev1", tclVar(faclev1listt[1]))
  putRcmdr("curflev2", tclVar(faclev2listt[1]))
  putRcmdr("curflab", tclVar(faclablistt[1]))

  ## fsel must select the right factor
  ## this should be highlighted in factor lists
  ##    and all related entries shown for changing in text boxes fnam etc.
  enterFrame <- ttklabelframe(enterlistFrame, text=gettext("Modify factor details for selected factor"))
  fsel <- ttkcombobox(enterFrame, textvariable=curfac, width=5, values=varlistshortt, state="readonly")
  tkbind(fsel, "<<ComboboxSelected>>", factorsel)
  #fnam <- ttkentry(listFrame, textvariable=curfnam, width=20,validate="focusout", validatecommand=fnamchange)
  fnam <- ttkentry(enterFrame, textvariable=curfnam, width=15)
  tkbind(fnam, "<FocusOut>", fnamchange)
  flev1 <- ttkentry(enterFrame, textvariable=curflev1, width=15)
  tkbind(flev1, "<FocusOut>", flev1change)
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))){
    tkconfigure(flev1,state="disabled")
  }
  flev2 <- ttkentry(enterFrame, textvariable=curflev2, width=15)
  tkbind(flev2, "<FocusOut>", flev2change)
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))){
    tkconfigure(flev2,state="disabled")
  }
  flab <- ttkentry(enterFrame, textvariable=curflab, width=20)
  tkbind(flab, "<FocusOut>", flabchange)
  tkbind(flab, "<Key-Tab>", tabflab)
  tkgrid(tklabel(enterFrame,text=gettext("Select"),width=6),
         tklabel(enterFrame,text=gettext("Factor name"), width=15),
         tklabel(enterFrame,text=gettext("First level"), width=15),
         tklabel(enterFrame,text=gettext("Second level"), width=15),
         tklabel(enterFrame,text=gettext("Comment or label \n(for html export only)"), width=20),
         sticky="w")
  tkgrid(fsel,fnam, flev1, flev2, flab, sticky="w")

  putRcmdr("facnamlist", tclVar(facnamlistt))
  putRcmdr("varlistshort", tclVar(varlistshortt))
  putRcmdr("faclev1list", tclVar(faclev1listt))
  putRcmdr("faclev2list", tclVar(faclev2listt))
  putRcmdr("faclablist", tclVar(faclablistt))

  facshortListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                               selectmode = single, exportselection = "TRUE", listvariable=varlistshort,
                               width = 6, background="#EBEBDC")
  tkbind(facshortListBox, "<<TraverseIn>>",function() tkfocus(fsel))

  facnameListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=facnamlist,
                              width = 15, background="#EBEBDC")
  faclev1ListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=faclev1list,
                              width = 15, background="#EBEBDC")
  faclev2ListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=faclev2list,
                              width = 15, background="#EBEBDC")
  faclabListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                             selectmode = single, exportselection = "TRUE", listvariable=faclablist,
                             width = 20, background="#EBEBDC")

  ## determine current index and reordering for onUp and onDown
  tkbind(facshortListBox, "<<ListboxSelect>>", checkIndexShort)
  tkbind(facnameListBox, "<<ListboxSelect>>", checkIndexNam)
  tkbind(faclev1ListBox, "<<ListboxSelect>>", checkIndexLev1)
  tkbind(faclev2ListBox, "<<ListboxSelect>>", checkIndexLev2)
  tkbind(faclabListBox, "<<ListboxSelect>>", checkIndexLab)

  ### funktioniert, ist aber noch nicht schön
  scrollbar <- ttkscrollbar(listFrame, command = function(...) {
    tkyview(facshortListBox, ...)
    tkyview(facnameListBox, ...)
    tkyview(faclev1ListBox, ...)
    tkyview(faclev2ListBox, ...)
    tkyview(faclabListBox, ...)
  })

  #    tkgrid(tklabel(enterlistFrame,text="  ", width=5),enterFrame, sticky="w")
  tkgrid(enterFrame, sticky="w", columnspan=5)
  #    tkgrid.configure(enterFrame, columnspan=5)
  ## Hoch-/Runterschieben von Einträgen ermöglichen

  downupFrame <- ttkframe(listFrame)
  moveDownButton <- buttonRcmdr(downupFrame, text = gettext("Move Down"),
                                foreground = "darkgreen", command = onDown,
                                default = "normal", borderwidth = 3, width=12)
  moveUpButton <- buttonRcmdr(downupFrame, text = gettext("Move Up"),
                              foreground = "darkgreen", command = onUp,
                              default = "normal", borderwidth = 3, width=12)
  tkgrid(moveDownButton, sticky="w")
  tkgrid(moveUpButton, sticky="w")

  tkgrid(scrollbar, facshortListBox, facnameListBox, faclev1ListBox, faclev2ListBox, faclabListBox, downupFrame, sticky = "nw")
  tkgrid.configure(scrollbar, sticky = "wns")
  tkgrid.configure(facnameListBox, sticky = "new")

  ## hard frame, to be displayed if special choices are activated
  putRcmdr("hardVar", tclVar(.stored.design2FrF$hardVar))
  hardFrame <- ttklabelframe(tab2, text=gettext("How many factors are hard to change ?"))
  hardEntry <- tkentry(hardFrame, width="3", textvariable=hardVar)
  tkconfigure(hardEntry,takefocus=0)

  hardlabelFrame <- ttklabelframe(hardFrame, text=gettext("WARNING"))

  tkgrid(tklabel(hardFrame,text="The first "),hardEntry,tklabel(hardFrame,text=" factors are hard to change."))
  tkgrid(tklabel(hardFrame,text="If necessary, modify the factor order on the Factor Details tab!"),columnspan=3)
  tkgrid(tklabel(hardlabelFrame,text=gettext("Only specify this, if some factors are   r e a l l y   hard to change.\nThe hard-to-change factors arrangement of experimental runs bears the risk of misjudging effects of these factors!"),wraplength=300))
  tkgrid(hardlabelFrame, sticky="w",columnspan=3)

  helptab2Button <- buttonRcmdr(tab2, text = gettext("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab2,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab2Button, takefocus=0)

  ## finalize tab2 Factor details
  tkgrid(helptab2Button, sticky="e")
  if (as.logical(as.numeric(tclvalue(specialcbVariable)))){
    tkgrid(deflevFrame, hardFrame, sticky="nw")
  }
  else tkgrid(deflevFrame, sticky="nw")
  tkgrid(listFrame, columnspan=6,sticky="w",pady=10)
  tkgrid(enterlistFrame, columnspan=6,sticky="w")

  ## tab3
  helptab3Button <- buttonRcmdr(tab3, text = gettext("Tab Help"),
                                foreground = "darkgreen", command = onHelpTabEstimable,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab3Button, takefocus=0)
  tkgrid(helptab3Button, sticky="e", columnspan=6)

  estradioFrame <- ttklabelframe(tab3, text=gettext("Mode of requesting estimable 2-factor interactions"))
  putRcmdr("estrbVariable", tclVar(.stored.design2FrF$estrbVariable))
  noestrb <- tkradiobutton(estradioFrame,text=gettext("None"),variable=estrbVariable,value="none",command=onestrb)
  clearrb <- tkradiobutton(estradioFrame,text=gettext("Selected interactions must be clear of aliasing with ANY 2-factor interactions"),variable=estrbVariable,value="clear",wraplength="500",justify="left",command=onestrb)
  distinctrb <- tkradiobutton(estradioFrame,text=gettext("Selected interactions must be clear of aliasing with EACH OTHER"),variable=estrbVariable,value="distinct",wraplength="500",justify="left",command=onestrb)
  ## deactivate all fields on this tab, if this box is not checked
  estlabel <- tklabel(estradioFrame, text=gettext("NOTE: Resolution entry on tab Base Settings is ignored, if choice is not None"))
  tkgrid(noestrb, sticky="w")
  tkgrid(clearrb, sticky="w")
  tkgrid(distinctrb, sticky="w")
  tkgrid(estlabel, sticky="w")

  resoFrame <- ttklabelframe(tab3,text=gettext("Minimum Resolution"))
  resolabel <- tklabel(resoFrame, text=gettext("Per default, at least resolution IV is required. \nThis is natural for most applications."),
                       justify="left")
  res3cbVariable <- tclVar(.stored.design2FrF$cbInitials[10])
  res3cb <- ttkcheckbutton(resoFrame,text=gettext("Permit a resolution III design"),variable=res3cbVariable)
  tkgrid(resolabel, sticky="w", columnspan=5)
  tkgrid(res3cb, sticky="w", columnspan=5)

  if (tclvalue(estrbVariable)=="none") tkgrid(estradioFrame, sticky="w", columnspan=4)
  else tkgrid(estradioFrame, resoFrame, sticky="w", columnspan=5)

  ## create empty row
  tkgrid(tklabel(tab3,text="   "))
  selFrame <- ttklabelframe(tab3, text=gettext("Select 2-factor interactions"))
  comprradioFrame <- ttklabelframe(selFrame, text=gettext("Type of specification"))

  ## design from older version of RcmdrPlugin.DoE
  if (is.null(.stored.design2FrF$comprrbVariable))
    .stored.design2FrF$comprrbVariable <- "manual"
  putRcmdr("comprrbVariable", tclVar(.stored.design2FrF$comprrbVariable))
  manualestrb <- tkradiobutton(comprradioFrame,text=gettext("Select manually"),
                               variable=comprrbVariable,value="manual",command=oncomprestrb)
  comprestrb <- tkradiobutton(comprradioFrame,text=gettext("Pre-specified structure from two groups of factors"),
                              variable=comprrbVariable,value="compr",wraplength="500",justify="left",command=oncomprestrb)

  varlistshortt=(strsplit(tclvalue(varlistshort)," ")[[1]])
  if (tclvalue(comprrbVariable)=="manual"){
    hilf <- combn(length(varlistshortt),2)
    intaclistt <- paste(varlistshortt[hilf[1,]],varlistshortt[hilf[2,]],sep="")
    putRcmdr("est2fislist", setdiff(.stored.design2FrF$est2fislist,setdiff(.stored.design2FrF$est2fislist,intaclistt)))
    ## omit clicked already selected interactions that must be lost because of a reduction in nfactors
  }
  else{
    intaclistt <- varlistshortt
    putRcmdr("est2fislist", setdiff(.stored.design2FrF$est2fislist,setdiff(.stored.design2FrF$est2fislist,intaclistt)))
    ## omit clicked already selected factors that must be lost because of a reduction in nfactors
  }

  ## design from older version of RcmdrPlugin.DoE
  if (is.null(.stored.design2FrF$comprclassVar))
    .stored.design2FrF$comprclassVar <- "3: all interactions of group 1"
  putRcmdr("comprclassVar", tclVar(.stored.design2FrF$comprclassVar))

  #resEntry <- tkentry(despropframe, textvariable=resVar)
  putRcmdr("comprclassEntry", ttkcombobox(comprradioFrame, textvariable=comprclassVar, width=50,
                                          values=c("1: interactions within group1",
                                                   "2: interactions within groups 1 and groups 2",
                                                   "3: all interactions of group 1",
                                                   "4: interactions between groups 1 and 2"), state="readonly"))
  #tkbind(comprclassEntry, "<<ComboboxSelected>>", onRefresh)

  tkgrid(manualestrb, sticky="w")
  tkgrid(comprestrb, comprclassEntry, sticky="w")
  tkgrid(comprradioFrame, sticky="w", columnspan=6)
  if (!tclvalue(comprrbVariable)=="compr") tkconfigure(comprclassEntry, state="disabled")

  estbuttonFrame <- ttkframe(selFrame)
  selectButton <- buttonRcmdr(estbuttonFrame, text = gettext(">"),
                              foreground = "darkgreen", command = onSelect,
                              default = "normal", borderwidth = 3)
  tkgrid(selectButton)
  deselectButton <- buttonRcmdr(estbuttonFrame, text = gettext("<"),
                                foreground = "darkgreen", command = onDeselect,
                                default = "normal", borderwidth = 3)
  tkgrid(deselectButton)
  onestrb.worefresh()

  putRcmdr("notest2fislist", setdiff(intaclistt, est2fislist))

  ## define variable list boxes for selection
  ## intaclistt is the master list

  ## make sure that both lists are of equal length and long enough
  ## ## make selection offering depend on choice of compromise settings
  if (tclvalue(comprrbVariable)=="compr"){
    TITEL.LINKS <- "Group 1 (at least 1 element)"
    TITEL.RECHTS <- "Group 2 (at least 1 element)"
  }
  else {
    TITEL.LINKS <- "Available 2-factor interactions"
    TITEL.RECHTS <- "Selected 2-factor interactions"
  }
  putRcmdr("est2fis", variableListBox(selFrame, variableList=intaclistt, listHeight=15,
                                      title=TITEL.RECHTS, selectmode="multiple"))
  putRcmdr("notest2fis", variableListBox(selFrame, variableList=intaclistt, listHeight=15,
                                         title=TITEL.LINKS, selectmode="multiple"))
  tkconfigure(notest2fis$listbox, listvariable=tclVar(paste(notest2fislist,collapse=" ")))
  notest2fis$varlist <- notest2fislist
  tkconfigure(est2fis$listbox, listvariable=tclVar(paste(est2fislist,collapse=" ")))
  est2fis$varlist <- est2fislist


  maxtimeFrame <- ttklabelframe(selFrame, text=gettext("Limit search time"))
  maxtimelabel <- tklabel(maxtimeFrame,
                          text=gettext("The search can take very long.\nIf it times out unsuccessfully, \nexpert users may try the command line mode of function FrF2 that allows more control options."),
                          justify="left", wraplength="280")
  maxtimeentrylabel <- tklabel(maxtimeFrame, text=gettext("Maximum search time in seconds"))
  maxtimeVar <- tclVar(.stored.design2FrF$maxtimeVar)
  maxtimeEntry <- tkentry(maxtimeFrame, textvariable=maxtimeVar)
  tkgrid(maxtimeentrylabel, sticky="w")
  tkgrid(maxtimeEntry, sticky="e")
  tkgrid(maxtimelabel,sticky="w")
  ## create empty row
  tkgrid(tklabel(tab3,text="   "))
  #tkgrid(maxtimeFrame, sticky="w", columnspan=5)

  if (tclvalue(estrbVariable)=="distinct") {tkgrid(notest2fis$frame, estbuttonFrame, est2fis$frame, maxtimeFrame, sticky="w")
    tkgrid.configure(maxtimeFrame, sticky="e")
  }
  else tkgrid(notest2fis$frame, estbuttonFrame, est2fis$frame, sticky="w")
  tkgrid(selFrame, sticky="ew", columnspan=6)

  #if (tclvalue(estrbVariable)=="none"){
  #    tkconfigure(selectButton, state="disabled")
  #    tkconfigure(deselectButton, state="disabled")
  #    tkconfigure(maxtimeentrylabel, state="disabled")
  #    tkconfigure(maxtimeEntry, state="disabled")
  #    tkconfigure(maxtimelabel, state="disabled")
  #    tkconfigure(resolabel, state="disabled")
  #    tkconfigure(res3cb, state="disabled")
  #}
  #else{
  #    tkconfigure(selectButton, state="normal")
  #    tkconfigure(deselectButton, state="normal")
  #    tkconfigure(resolabel, state="normal")
  #    tkconfigure(res3cb, state="normal")
  #    if (tclvalue(estrbVariable)=="distinct") {
  #    tkconfigure(maxtimeentrylabel, state="normal")
  #    tkconfigure(maxtimeEntry, state="normal")
  #    tkconfigure(maxtimelabel, state="normal")
  #        }
  #    else {
  #    tkconfigure(maxtimeentrylabel, state="disabled")
  #    tkconfigure(maxtimeEntry, state="disabled")
  #    tkconfigure(maxtimelabel, state="disabled")
  #        }
  #}

  tkgrid(tklabel(tab4, text=gettext("This tab will accomodate block or split-plot functionality.")), sticky="n")

  ### tab5
  ## define frames for special activities on specials tab
  specialFrame <- ttklabelframe(tab5,text=gettext("Any special non-standard requirements ?"))

  ### widgets for special frame
  putRcmdr("specialrbVariable", tclVar(.stored.design2FrF$specialrbVariable) )
  ## for some reason, doesn't work with initialValue directly as text string
  specialrbFrame <- ttkframe(specialFrame)
  nonerb <- tkradiobutton(specialrbFrame,text=gettext("None"),variable=specialrbVariable,value="none", command=onRefresh)
  hardrb <- tkradiobutton(specialrbFrame,text=gettext("(Some) Hard to change factor(s)"),variable=specialrbVariable,value="hard", command=onRefresh)
  debarrb <- tkradiobutton(specialrbFrame,text=gettext("Some combinations impossible"),variable=specialrbVariable,value="debar", command=onRefresh)
  tkgrid(nonerb,sticky="w")
  tkgrid(hardrb,sticky="w")
  tkgrid(debarrb,sticky="w")
  tkgrid(tklabel(specialrbFrame,text="  "))

  tkgrid(specialrbFrame, columnspan=4,sticky="w")
  if (tclvalue(specialrbVariable)=="none") tkgrid(specialFrame, sticky="nw",columnspan=5)

  #if (tclvalue(specialrbVariable)=="hard"){
  #   tkgrid(specialFrame, hardlabelFrame, sticky="w",columnspan=5)
  #   tkgrid(hardFrame, sticky="w")
  #}

  if (tclvalue(specialrbVariable)=="debar"){
    debarlabelFrame <- ttklabelframe(tab5,text=gettext("WARNING"))

    nrunEntryDebar <- ttkentry(tab5, textvariable=nrunVar)
    debarFrame <- ttklabelframe(tab5, text=gettext("Conditions for excluding combinations"))
    tkgrid(tklabel(debarFrame,text="must think of a simple way of specifying conditions with clicking them together (or typing at users choice)",wraplength=500))
    tkgrid(tklabel(debarlabelFrame,text="If some experimental combinations are impossible, standard designs cannot be used. Please try to avoid this.\n\nIf it is unavoidable, estimable effects must be specified on the Estimable Model tab, with the third radio button in the top left frame activated.", wraplength=500, justify="left"),sticky="w")
    tkgrid(specialFrame, debarlabelFrame, sticky="w")

    tkgrid(ttklabel(tab5,text="  "),sticky="w")
    tkgrid(ttklabel(tab5,text=gettext("Number of runs: ")),nrunEntryDebar,sticky="w")
    tkgrid(ttklabel(tab5,text=gettext("The number of runs need not be a power of 2 any more, as the design is determined as a non-orthogonal D-optimal design."), justify="left", wraplength=500),sticky="w",columnspan=2)
    tkgrid(ttklabel(tab5,text="  "),sticky="w")

    tkgrid(debarFrame, columnspan=2,sticky="w")
    ### here, we need the possibility to enter conditions to debar combinations
  }

  ## finalize tab5


  ## tab6 for exporting
  helptab6Button <- buttonRcmdr(tab6, text = gettext("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab6,
                                default = "normal", borderwidth = 3)

  exportlabVar <- nameVar
  exportlab <- ttklabel(tab6, textvariable=exportlabVar)
  tkgrid(ttklabel(tab6,text="Current design to be saved:"),exportlab,helptab6Button,sticky="w", pady=10)

  ## radio buttons for choosing export type
  etradioFrame <- ttklabelframe(tab6, text=gettext("(How to) Export ?"))
  etyperbVariable <- tclVar(.stored.design2FrF$etyperbVariable)
  noexprb <- tkradiobutton(etradioFrame,text=gettext("no export"),variable=etyperbVariable,value="none")
  allrb <- tkradiobutton(etradioFrame,text=gettext("all file types"),variable=etyperbVariable,value="all")
  rdarb <- tkradiobutton(etradioFrame,text=gettext("rda only"),variable=etyperbVariable,value="rda")
  htmlrb <- tkradiobutton(etradioFrame,text=gettext("html and rda"),variable=etyperbVariable,value="html")
  csvrb <- tkradiobutton(etradioFrame,text=gettext("csv and rda"),variable=etyperbVariable,value="csv")
  tkgrid(noexprb, sticky="w")
  tkgrid(allrb, sticky="w")
  tkgrid(rdarb, sticky="w")
  tkgrid(htmlrb, sticky="w")
  tkgrid(csvrb, sticky="w")

  ## radio buttons for choosing export decimal separator
  decimalradioFrame <- ttklabelframe(tab6, text=gettext("Decimal Separator ?"))
  decimalrbVariable <- tclVar(.stored.design2FrF$decimalrbVariable)
  defaultrb <- tkradiobutton(decimalradioFrame,text=gettext("default"),variable=decimalrbVariable, value="default")
  pointrb <- tkradiobutton(decimalradioFrame,text=gettext("."),variable=decimalrbVariable, value=".")
  commarb <- tkradiobutton(decimalradioFrame,text=gettext(","),variable=decimalrbVariable, value=",")
  tkgrid(defaultrb, sticky="w")  ## in this case, leave default option from options
  tkgrid(pointrb, sticky="w")
  tkgrid(commarb, sticky="w")

  ## export directory
  dirFrame <- ttklabelframe(tab6, text=gettext("Storage Directory"))
  putRcmdr("dirVar", tclVar(.stored.design2FrF$dirVar))
  dirEntry <- tkentry(dirFrame, width="50", textvariable=dirVar)
  dirButton <- buttonRcmdr(dirFrame, text = gettext("Change directory"),
                           foreground = "darkgreen", width = "20", command = onChangeDir,
                           default = "normal", borderwidth = 3)
  tkgrid(dirEntry, dirButton, sticky="w")
  tkgrid.configure(dirEntry,padx=15)

  ## export file name
  putRcmdr("fileVar", tclVar(.stored.design2FrF$fileVar))
  fileEntry <- tkentry(tab6, width="20", textvariable=fileVar)
  efnamelabel <- tklabel(tab6,text=gettext("Export file names: name below with appropriate endings (html or csv, and rda)"))
  putRcmdr("replacecbVariable", tclVar(.stored.design2FrF$cbInitials[8]))
  replacecb <- ttkcheckbutton(tab6,text=gettext("Replace file(s), if exists"),variable=replacecbVariable)

  ## always grid details, as otherwise default file name does not work
  ## design name info and help button have already been gridded above
  tkgrid(etradioFrame, decimalradioFrame, sticky="w")
  tkgrid(dirFrame, sticky="w", columnspan=5)
  tkgrid.configure(dirFrame, pady=15)
  tkgrid(efnamelabel, sticky="w", columnspan=5)
  tkgrid(fileEntry, sticky="w", columnspan=5)
  tkgrid(replacecb, sticky="w", columnspan=5)


  ## add buttons outside the notebook
  buttonFrame <- tkframe(topdes2)
  ## die sind aber nicht dunkelgruen ...
  refreshButton <- buttonRcmdr(buttonFrame, text = gettext("Refresh form"),
                               foreground = "darkgreen", width = "12", command = onRefresh,
                               default = "normal", borderwidth = 3)
  storeButton <- buttonRcmdr(buttonFrame, text = gettext("Store form"),
                             foreground = "darkgreen", width = "12", command = onStore,
                             default = "normal", borderwidth = 3)
  loadButton <- buttonRcmdr(buttonFrame, text = gettext("Load form"),
                            foreground = "darkgreen", width = "12", command = onLoad,
                            default = "normal", borderwidth = 3)
  resetButton <- buttonRcmdr(buttonFrame, text = gettext("Reset form"),
                             foreground = "darkgreen", width = "12", command = onReset,
                             default = "normal", borderwidth = 3)
  #        tkgrid(refreshButton,sticky="w")
  #        tkgrid(tklabel(buttonFrame,text="  "),sticky="w")
  tkgrid(storeButton,sticky="w")
  tkgrid(loadButton,sticky="w")
  tkgrid(resetButton,sticky="w")

  tkconfigure(refreshButton, takefocus=0)
  tkconfigure(storeButton, takefocus=0)
  tkconfigure(loadButton, takefocus=0)
  tkconfigure(resetButton, takefocus=0)

  ## storage buttons to the right of the notebook
  tkgrid(tn, buttonFrame, sticky="w", columnspan=2)

  OKCancelHelp(window=topdes2, helpSubject="Menu.FrF2level")
  tkconfigure(OKbutton, takefocus=0)
  tkconfigure(cancelButton, takefocus=0)
  tkconfigure(helpButton, takefocus=0)

  #tkbind(specialcb, "<ButtonRelease-1>", onReset)
  tkgrid(specialcb, sticky="e")
  tkconfigure(specialcb, takefocus=0)

  tkgrid(buttonsFrame, bottomFrame, sticky="ew")


  ### relations among widgets
  if (!as.logical(as.numeric(tclvalue(randomizeVariable)))){
    tkconfigure(seedEntry, state="disabled")
    tkconfigure(seedlab, state="disabled")
  }else {
    tkconfigure(seedEntry, state="normal")
    tkconfigure(seedlab, state="normal")
  }
  if (!as.logical(as.numeric(tclvalue(nrunEntryVariable)))){
    tkconfigure(nrunEntry, state="disabled")
    tkconfigure(nrunlab, state="disabled")
  }else {
    tkconfigure(nrunEntry, state="normal")
    tkconfigure(nrunlab, state="normal")
  }
  ##if (!as.logical(as.numeric(tclvalue(nfacEntryVariable)))){
  ##tkconfigure(nfacEntry, state="disabled")
  ##tkconfigure(nfaclab, state="disabled")
  ##}else {
  ##tkconfigure(nfacEntry, state="normal")
  ##tkconfigure(nfaclab, state="normal")
  ##}


  if (as.logical(as.numeric(tclvalue(specialcbVariable)))){
    tcl(tn, "tab", 2, state="normal")
    tcl(tn, "tab", 3, state="hidden")
    tcl(tn, "tab", 4, state="hidden")
    ## hide tabs that are not yet usable
  }
  else {
    tcl(tn, "tab", 2, state="hidden")
    tcl(tn, "tab", 3, state="hidden")
    tcl(tn, "tab", 4, state="hidden")
  }


  #### former tab5: special
  ## do last in order to have it overwrite the others rather than
  ## have others disable special again
  #if (as.logical(as.numeric(tclvalue(specialcbVariable)))){
  #if (tclvalue(specialrbVariable) == "block" || tclvalue(specialrbVariable) == "splitplot")
  #RcmdrTkmessageBox(gettext("You cannot declare impossible combinations for blocked or split-plot designs. Please resolve."),
  #     icon="error", type="ok", title="Conflict")
  #  tcl(tn, "tab", 4, state="normal")
  #if (!tclvalue(hardVar) == "0")
  #     RcmdrTkmessageBox(gettext("You cannot declare impossible combinations for designs with hard-to-change factors. Please resolve."),
  #     icon="error", type="ok", title="Conflict")
  #  tcl(tn, "tab", 4, state="normal")
  #}else {
  #  tcl(tn, "tab", 4, state="hidden")
  #}

  #if (as.numeric(tclvalue(hardVar))>0){
  #if (tclvalue(specialrbVariable) == "block" || tclvalue(specialrbVariable) == "splitplot")
  #RcmdrTkmessageBox(gettext("You cannot combine hard-to-change variables with blocked or split-plot designs. (A split-plot design in itself can often take care of hard-to-change variables.) Please resolve."),
  #     icon="error", type="ok", title="Conflict")
  #if (tclvalue(specialrbVariable) == "estimable")
  #RcmdrTkmessageBox(gettext("You cannot combine hard-to-change variables with estimable models. Please resolve."),
  #     icon="error", type="ok", title="Conflict")
  #}
  if (exists("activestab.tn", where="RcmdrEnv")){
    tcl(tn, "select", activestab.tn)
    rm(activestab.tn, pos="RcmdrEnv")
  }

  dialogSuffix(window=topdes2, rows=2, columns=2, focus=tn, bindReturn=FALSE)

}
###
# End of Menu.FrF2level
###

Menu.loadcatlg <- function(){
  ## FrF2.catlg128 is loaded
  candlist <- ""
  logger("require(FrF2.catlg128)")
  if (require(FrF2.catlg128)){
    candlist <- c("catlg128.8to15",paste("catlg128",16:23,sep="."))}
  if (identical(candlist, "")) {
    Message(message=gettextRcmdr("There are no catalogues from which to choose."),
            type="error")
    tkfocus(CommanderWindow())
    return()
  }

  initializeDialog(title=gettextRcmdr("Select catalogue of regular fractional factorial 2-level designs"))
  catlgBox <- variableListBox(top, candlist, title=gettextRcmdr("Catalogues (pick one)"),
                              initialSelection=NULL)
  ## create button that calls further menu
  ## cascade menu in the menu entry
  onOK <- function(){
    command <- paste("data(",getSelection(catlgBox),")")
    logger(command)
    justDoItDoE(command)
    closeDialog()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="Menu.loadcatlg")
  tkgrid(getFrame(catlgBox), sticky="n")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(rows=2, columns=1)
}

Menu.fac <- function(){
  initializeDialogDoE(title=gettextRcmdr("Create full factorial design ..."))
  ## function initializeDialogDoE assumes topdes2 as windowname
  ## last stored top left corner for window is stored under topleft2xy
  ## onRefresh still makes window walk a little

  ## Lesen der level klappt noch nicht, jedenfalls Fehler dass nlevels und factor.names nicht zusammenpassen

  ## menus must be edited

  if (exists("curindex", where="RcmdrEnv")) rm(curindex, pos="RcmdrEnv")

  if (!exists(".stored.designfac",where="RcmdrEnv"))
    assign(".stored.designfac", .default.designfac,pos="RcmdrEnv")
  ## nameVar, nrunVar, nfacVar, nrepVar, nblockVar,
  ## cbInitials containing repeat.onlyVariable, randomizeVariable,
  ##                       facnamesAutoVariable, faclevelsCommonVariable,
  ##                       nrunVar, estcbVariable
  ##                       specialcbVariable, replacecbVariable, MaxC2cbVariable
  ##                       res3cbVariable
  ## nlevVar, level2Var, seedVar, specialrbVariable, hardVar, genVar,
  ## catlgVar, designVar, designrbVariable, destyperbVariable
  ## resVar, qualcritrbVariable, facnamlist,nlevlist,faclevlist, faclablist
  ## etyperbVariable, decimalrbVariable, dirVar, fileVar
  ## support change o implementing nblockVar
  if (is.null(getRcmdr(".stored.designfac")))
    putRcmdr(".stored.designfac",
             c(list(nblockVar="1"), getRcmdr(".stored.designfac")))

  ## MaxC2cbVariable is free again (no. 9 of cbInitials)

  ## define called functions
  infoClose <- function(){
    putRcmdr("infotxt",tclVar(""))
  }

  onHelpTab1 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.facTab1"))
  }
  onHelpTab2 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.FacDetailsGenTab"))
  }
  onHelpTab6 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.exportTab"))
  }

  tabpos <- function(){
    ### get 0-based index of currently selected tab
    activestab.tn <- tclvalue(tcl(tn, "select"))
    activestab.tn <- strsplit(activestab.tn,".",fixed=TRUE)[[1]]
    activestab.tn <- as.numeric(activestab.tn[length(activestab.tn)])-1
    activestab.tn
  }

  storeRcmdr <- function(){
    hilf <- list(nameVar=tclvalue(nameVar),
                 nrunVar=tclvalue(nrunVar),nfacVar=tclvalue(nfacVar),nrepVar=tclvalue(nrepVar),
                 nblockVar=tclvalue(nblockVar),
                 cbInitials = c(tclvalue(repeat.onlyVariable), tclvalue(randomizeVariable),
                                0,0,
                                1,0,
                                0,tclvalue(replacecbVariable),0,
                                0
                 ),
                 seedVar=tclvalue(seedVar),
                 facnamlist=as.character(tclObj(facnamlist)),
                 nlevlist=as.character(tclObj(nlevlist)),
                 faclevlist=as.character(tclObj(faclevlist)),
                 faclablist=as.character(tclObj(faclablist)),
                 etyperbVariable=tclvalue(etyperbVariable),
                 decimalrbVariable=tclvalue(decimalrbVariable),
                 dirVar=tclvalue(dirVar), fileVar=tclvalue(fileVar))
    class(hilf) <- c("menu.designfac","list")
    putRcmdr(".stored.designfac",hilf)
  }

  onOK <- function(){
    onRefreshEnd()
    ## store entries so that users do not have to redo everything
    ## in case of stupid mistakes
    storeRcmdr()
    ## seed is not used from previously stored design
    closeDialog(window=topdes2)
    name <- tclvalue(nameVar)
    if (!is.valid.name(name)){
      errorCondition(window=topdes2,recall=Menu.fac,
                     message=paste('"', name, '" ', gettextRcmdr("is not a valid name."), sep=""))
      return()
    }
    if (is.element(name, listObjects()))
    {
      if ("no" == tclvalue(checkReplace(name, gettextRcmdr("Object")))){
        errorCondition(window=topdes2,recall=Menu.fac,
                       gettextRcmdr("Introduce another name for the new data.frame, or allow replacing."))
        return()
      }
    }
    if (any(getRcmdr(".stored.designfac")$nlevlist=="") )
    {
      errorCondition(window=topdes2,recall=Menu.fac,
                     gettextRcmdr("Factor details are not completely specified."))
      return()
    }
    if (any(getRcmdr(".stored.designfac")$faclevlist=="") )
    {
      errorCondition(window=topdes2,recall=Menu.fac,
                     gettextRcmdr("Factor details are not completely specified."))
      return()
    }
    ###  further error messages with return to menu ?

    textfactornameslist.forcommand <- paste("factor.names=list(",paste(paste(as.character(tclObj(facnamlist)),"=c(",
                                                                             sapply(strsplit(as.character(tclObj(faclevlist))," "),function(obj) paste(dquote(obj),collapse=",")),
                                                                             ")",sep=""),collapse=","),")")

    ### not yet perfect, especially NULL entries are not possible
    command <- paste("fac.design(nfactors=",tclvalue(nfacVar),",replications=",
                     tclvalue(nrepVar),",repeat.only=",as.logical(as.numeric(tclvalue(repeat.onlyVariable))),
                     ",blocks=",tclvalue(nblockVar),
                     ",randomize=",as.logical(as.numeric(tclvalue(randomizeVariable))),",seed=",tclvalue(seedVar),
                     ",nlevels=c(", paste(as.character(tclObj(nlevlist)),collapse=","),
                     "),",textfactornameslist.forcommand,")")


    hilf <- justDoItDoE(command)
    if (class(hilf)[1]=="try-error") {
      Message(paste(gettextRcmdr("Offending command:"), "\n", command), type="error")
      errorCondition(window=topdes2,recall=Menu.fac, message=gettextRcmdr(hilf))
      return()
    }
    logger(paste(name, "<-", command))
    logger("## creator element of design.info will be different, when using the command line command!")
    ## change creator to contain menu settings
    hilfatt <- design.info(hilf)
    hilfatt$creator <- .stored.designfac
    class(hilfatt$creator) <- c("menu.designfac", "list")
    attr(hilf, "design.info") <- hilfatt
    putRcmdr("hilf", hilf)
    ## replace assign by justDoIt; assign(name, hilf, envir=.GlobalEnv)
    justDoIt(paste(name, "<- getRcmdr(\"hilf\")"))
    rm("hilf", pos="RcmdrEnv")
    activeDataSet(name)
    ### exporting
    if (!tclvalue(etyperbVariable)=="none"){
      putRcmdr("path", tclvalue(dirVar))
      putRcmdr("filename", tclvalue(fileVar))
      if (!as.logical(as.numeric(tclvalue(replacecbVariable)))){
        lf <- tolower(list.files(path = path))
        if (tolower(paste(filename, "rda", sep = ".")) %in% lf)
          stop("file ", paste(filename, "rda", "."), " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="html" & tolower(paste(filename, "html", sep = ".")) %in% lf)
          stop("file ", paste(filename, "html", "."), " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="csv" & tolower(paste(filename, "csv", sep = ".")) %in% lf)
          stop("file ", paste(filename, "csv", "."), " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
      }
      if (tclvalue(decimalrbVariable)=="default") command <- paste("export.design(",name,
                                                                   ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                                                                   as.logical(as.numeric(tclvalue(replacecbVariable))),")",sep="")
      else command <- paste("export.design(",name,
                            ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                            as.logical(as.numeric(tclvalue(replacecbVariable))),", OutDec=", dquote(tclvalue(decimalrbVariable)),")",sep="")
      hilf <- justDoItDoE(command)
      if (class(hilf)[1]=="try-error") {
        errorCondition(window=topdes2,recall=Menu.fac, message=gettextRcmdr(hilf))
        return()
      }
      logger(command)
    }
    rm(activestab.tn, pos="RcmdrEnv")
    tkwm.deiconify(CommanderWindow())
    tkfocus(CommanderWindow())
  }

  listDesignfac <- function (envir = .GlobalEnv, ...)
  {
    Vars <- ls(envir = envir, all.names = TRUE)
    Vars[which(sapply(Vars, function(.x){
      aus <- FALSE
      if ("menu.designfac" %in% class(get(.x, envir = envir))) aus <- TRUE
      else if ("design" %in% class(get(.x, envir = envir)))
        if ("menu.designfac" %in% class(design.info(get(.x, envir = envir))$creator))
          aus <- TRUE
        aus
    }))]
  }


  onLoad <- function(){
    ## seems to work now, needs to be tested!
    hilf <- listDesignfac()
    if (length(hilf)==0) {
      tkmessageBox(message=gettextRcmdr("There are no stored design inputs in this session."),icon="error", type="ok", title="no stored design inputs")
      return()
    }
    putRcmdr("deschoosefac",tktoplevel())
    tkwm.title(deschoosefac, gettextRcmdr("Choose stored design form"))
    position <- if (is.SciViews())
      -1
    else position <- "+50+50"
    tkwm.geometry(deschoosefac, position)
    putRcmdr("lb", variableListBox(deschoosefac, variableList=hilf, title="Choose stored design form"))
    tkgrid(lb$frame)
    onOK <- function() {
      putRcmdr(".stored.designfac",
               get(lb$varlist[as.numeric(tclvalue(tcl(lb$listbox, "curselection")))+1]))
      if ("design" %in% class(getRcmdr(".stored.designfac")))
        putRcmdr(".stored.designfac", design.info(getRcmdr(".stored.designfac"))$creator)
      if (is.null(getRcmdr(".stored.designfac")$nblockVar))
        putRcmdr(".stored.designfac",
                 c(list(nblockVar="1"), getRcmdr(".stored.designfac")))
      tkfocus(CommanderWindow())
      tkdestroy(topdes2)
      tkdestroy(deschoosefac)
      Menu.fac()
    }
    OKCancelHelp(window=deschoosefac)
    tkgrid(buttonsFrame, sticky="s")
    dialogSuffix(window=deschoosefac, rows=1, columns=1,
                 focus=lb$listbox)
  }

  onRefreshEnd <- function(){
    nfacchange()
    storeRcmdr()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    putRcmdr("activestab.tn",tabpos())
    ID <- topdes2$ID
    putRcmdr("topleft2xy",as.numeric(c(tclvalue(.Tcl(paste("winfo rootx", ID))),
                                       tclvalue(.Tcl(paste("winfo rooty", ID))))))
    #        assign("activestab.tn",strsplit(activestab.tn,".",fixed=TRUE)[[1]],pos="RcmdrEnv")
    #        assign("activestab.tn",as.numeric(activestab.tn[length(activestab.tn)])-1,pos="RcmdrEnv")
  }

  onRefresh <- function(){
    #print(as.character(tclObj(tcl(tn, "select"))))
    onRefreshEnd()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.fac()
  }

  onStore <- function(){
    ## Speichernamen abfragen und hier ermöglichen (statt stored.designfac)
    textentry() ## creates text string stored in savename.RcmdrPlugin.DoE
    if (!is.null(savename.RcmdrPlugin.DoE)){
      if (!is.valid.name(savename.RcmdrPlugin.DoE)){
        textcorrect(gettextRcmdr("This is not a valid name. Please correct:"))
        return()
      }
      if (is.element(savename.RcmdrPlugin.DoE, listObjects()))
      {
        if ("no" == tclvalue(checkReplace(savename.RcmdrPlugin.DoE, gettextRcmdr("Object"))))
        {
          textcorrect(gettextRcmdr("Please enter a new name:"))
          return()
        }
      }
      storeRcmdr()
      ## replace assign by justDoIt; assign(savename.RcmdrPlugin.DoE, getRcmdr(".stored.designfac"), envir=.GlobalEnv)
      justDoIt(paste(savename.RcmdrPlugin.DoE, "<- getRcmdr(\".stored.designfac\")"))
      message(gettextRcmdr("inputs have been stored"))
    }
  }

  onReset <- function(){
    assign(".stored.designfac",.default.designfac,pos="RcmdrEnv")
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.fac()
  }


  nfacchange <- function(){
    nfacold <- length(as.character(tclObj(varlistshort)))
    nfacnew <- as.numeric(tclvalue(nfacVar))
    if (nfacold==nfacnew) return()
    if (nfacnew < nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[1:nfacnew]))
      putRcmdr("nlevlist", tclVar(as.character(tclObj(nlevlist))[1:nfacnew]))
      putRcmdr("faclevlist", tclVar(as.character(tclObj(faclevlist))[1:nfacnew]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[1:nfacnew]))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(nlevListBox, listvariable=nlevlist, height=min(10,nfacnew))
      tkconfigure(faclevListBox, listvariable=faclevlist, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      if (selpos > nfacnew){
        tcl(fsel, "current", "0")
        factorsel()
      }
    }
    if (nfacnew > nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist", tclVar(c(as.character(tclObj(facnamlist)),
                                      getRcmdr("varlistshortt")[(nfacold+1):nfacnew])) )
      putRcmdr("nlevlist", tclVar(c(as.character(tclObj(nlevlist)),
                                    rep("",nfacnew-nfacold))))
      putRcmdr("faclevlist", tclVar(c(as.character(tclObj(faclevlist)),
                                      rep("",nfacnew-nfacold))))
      putRcmdr("faclablist", tclVar(c(as.character(tclObj(faclablist)),
                                      rep("",nfacnew-nfacold))))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      tkconfigure(nlevListBox, listvariable=nlevlist, height=min(10,nfacnew))
      tkconfigure(faclevListBox, listvariable=faclevlist, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))
    }
  }
  nameenter <- function(){
    if (identical(tclvalue(getRcmdr("fileVar")),tclvalue(getRcmdr("nameVar"))))
      putRcmdr("name.equal.filename", TRUE)
    else putRcmdr("name.equal.filename", FALSE)
  }
  namechange <- function(){
    if (is.valid.name(tclvalue(nameVar))){
      if (name.equal.filename){
        putRcmdr("fileVar", tclVar(tclvalue(nameVar)))  ## otherwise, variables would be directly tied
        #          putRcmdr("exportlabVar", tclVar(paste("Current design to be saved:", tclvalue(nameVar),"\n   ")))
        tkconfigure(fileEntry, textvariable=getRcmdr("fileVar"))
        #          tkconfigure(exportlab, textvariable=getRcmdr("exportlabVar"))
      }
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid design name")
  }
  factorsel<-function(){
    #### aendert die in der Textbox dargestellte Auswahl
    #### ruiniert aber leider auch wieder die korrekte Ueberschreibung der Werte
    putRcmdr("selpos", as.numeric(tclvalue(tcl(fsel, "current")))+1)
    putRcmdr("curfac", tclVar(as.character(tclObj(varlistshort))[selpos]))
    putRcmdr("curfnam", tclVar(as.character(tclObj(facnamlist))[selpos]))
    putRcmdr("curnlev", tclVar(as.character(tclObj(nlevlist))[selpos]))
    putRcmdr("curflev", tclVar(as.character(tclObj(faclevlist))[selpos]))
    putRcmdr("curflab", tclVar(as.character(tclObj(faclablist))[selpos]))
    tkconfigure(fnam, textvariable=curfnam)
    tkconfigure(nlev, textvariable=curnlev)
    tkconfigure(flev, textvariable=curflev)
    tkconfigure(flab, textvariable=curflab)
  }
  fnamchange <- function(){
    ## selpos known from factorsel
    if (is.valid.name(tclvalue(curfnam))){
      hilf <- as.character(tclObj(facnamlist))
      hilf[selpos] <- tclvalue(curfnam)
      ### "facnamlist" is not automatically updated in the listbox
      ### therefore the tkconfigure
      putRcmdr("facnamlist",tclVar(hilf))
      tkconfigure(facnameListBox, listvariable=facnamlist)
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid factor name")
  }
  nlevchange <- function(){
    ## selpos known from factorsel
    if (length(as.character(tclObj(curnlev)))==1){
      hilf <- as.character(tclObj(nlevlist))
      hilf[selpos] <- tclvalue(curnlev)
      putRcmdr("nlevlist",tclVar(hilf))
      tkconfigure(nlevListBox, listvariable=nlevlist)
    }
    else tkmessageBox(message="Empty entries or entries with blanks are not permitted, please correct!",
                      icon="error", type="ok", title="Invalid number of factor levels")
    if (is.na(as.numeric(tclvalue(curnlev))) | !floor(as.numeric(tclvalue(curnlev)))==as.numeric(tclvalue(curnlev)))
      tkmessageBox(message="an integer number was expected, please correct!",
                   icon="error", type="ok", title="Invalid number of levels")
    nrunVar <- tclVar(prod(as.numeric(hilf)))
    tkconfigure(nrunShow, textvariable=nrunVar)
    if (length(as.character(tclObj(curflev)))==0){
      flevchange()
      tkconfigure(flev, textvariable=curflev)}
  }
  flevchange <- function(){
    ## selpos known from factorsel
    nlev <- as.numeric(tclvalue(curnlev))
    nlevspec <- length(as.character(tclObj(curflev)))
    if (nlevspec==nlev){
      hilf <- as.character(tclObj(faclevlist))
      hilf[selpos] <- tclvalue(curflev)
      ### updating hilf does work
      ### but "varlist" is not automatically updated in the listbox
      ### therefore the tkconfigure
      putRcmdr("faclevlist",tclVar(hilf))
      tkconfigure(faclevListBox, listvariable=faclevlist)
    }
    else{
      if (nlevspec==0){
        putRcmdr("curflev", tclVar(paste(1:nlev, sep=" ")))
        hilf <- as.character(tclObj(faclevlist))
        hilf[selpos] <- tclvalue(curflev)
        putRcmdr("faclevlist",tclVar(hilf))
        tkconfigure(faclevListBox, listvariable=faclevlist)
      }
      else tkmessageBox(message=gettextRcmdr(nlev," levels entries needed, \nyou specified ", nlevspec, " levels, please correct!"),
                        icon="error", type="ok", title="Invalid number of level entries")
    }
  }
  flabchange <- function(){
    ## selpos known from factorsel
    ## for FocusOut event on flab
    ## still problematic, if Focus out occurs with tab
    ## as there is also a tab key event
    hilf <- as.character(tclObj(faclablist))
    hilf[selpos] <- tclvalue(curflab)
    putRcmdr("faclablist",tclVar(hilf))
    tkconfigure(faclabListBox, listvariable=faclablist)
  }

  tabflab <- function(){
    ## for Tab key event on flab
    ## the traversal still jumps to the first traversable control on the sheet
    ## (rather than staying with fnam, if asked by tkfocus to do so)
    ## takefocus has so far been set to 0 for all widgets except the factor detail ones on this tab
    flabchange()  ## otherwise, not carried out!
    hilf <- as.numeric(tclvalue(tcl(fsel,"current")))+1
    if (hilf  >= as.numeric(tclvalue(nfacVar))) return()
    tcl(fsel,"current", hilf)
    factorsel()
    #tkfocus(fnam)
    #tcl(fnam, "selection", "range", 1, "end")
    #tcl("break")
  }


  swap <- function(a,b){
    hilf <- 1:as.numeric(tclvalue(nfacVar))
    hilf[b] <- a
    hilf[a] <- b
    hilf
  }

  indexchange <- function(){
    if (curindex < as.numeric(tclvalue(nfacVar)))
      putRcmdr("orderDown",swap(curindex, curindex+1))
    if (curindex > 1)
      putRcmdr("orderUp",swap(curindex, curindex-1))
    tcl(fsel, "current", curindex-1)
    factorsel()
  }

  checkIndexShort <- function(){
    putRcmdr("curindex", as.numeric(tcl(facshortListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexNam <- function(){
    putRcmdr("curindex", as.numeric(tcl(facnameListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexNlev <- function(){
    putRcmdr("curindex", as.numeric(tcl(nlevListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexFlev <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclevListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLab <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclabListBox,"curselection"))+1)
    indexchange()
  }

  onUp <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex=="1" | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderUp]))
      putRcmdr("nlevlist", tclVar(as.character(tclObj(nlevlist))[orderUp]))
      putRcmdr("faclevlist", tclVar(as.character(tclObj(faclevlist))[orderUp]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderUp]))
      tkconfigure(nlevListBox, listvariable=nlevlist)
      tkconfigure(faclevListBox, listvariable=faclevlist)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex-1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(nlevListBox,"selection","set",curindex-1)
      tcl(faclevListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  onDown <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex==as.numeric(tclvalue(nfacVar)) | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderDown]))
      putRcmdr("nlevlist", tclVar(as.character(tclObj(nlevlist))[orderDown]))
      putRcmdr("faclevlist", tclVar(as.character(tclObj(faclevlist))[orderDown]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderDown]))
      tkconfigure(nlevListBox, listvariable=nlevlist)
      tkconfigure(faclevListBox, listvariable=faclevlist)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex+1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(nlevListBox,"selection","set",curindex-1)
      tcl(faclevListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  dquote <- function(obj){
    ## quote vector elements for use as character vector in a command
    aus <- rep("",length(obj))
    wopt <- options("warn")[[1]]
    options(warn=-1)
    for (i in 1:length(obj)) if (is.na(as.numeric(obj[i]))) {
      if (length(grep('"',obj[i])>0))
        aus[i] <- paste("'",obj[i],"'",sep="")
      else
        aus[i] <- paste('"',obj[i],'"',sep="")
    }
    else aus[i] <- obj[i]
    options(warn=wopt)
    aus
  }


  onChangeDir <- function(){
    putRcmdr("direct",tclvalue(tkchooseDirectory()))
    if (!direct=="") {
      putRcmdr("dirVar", tclVar(direct))
      tkconfigure(dirEntry, textvariable = dirVar)
    }
  }

  ######## end define functions


  ##### define userform
  #tn <- ttknotebook(top,height=100, width=500)


  putRcmdr("tn",ttknotebook(topdes2))
  #tn <- ttknotebook(topdes2)

  putRcmdr("tab1",ttkframe(tn))
  putRcmdr("tab2",ttkframe(tn))
  putRcmdr("tab6",ttkframe(tn))## called 6 because of parallel treatment with
  ## fractional factorial menu

  tkadd(tn,tab1,text="Base Settings")   ### tabid=0
  tkadd(tn,tab2,text="Factor Details")  ### tabid=1
  tkadd(tn,tab6,text="Export") ### tabid=5

  tkconfigure(tn, takefocus=0)

  nameFrame <- ttkframe(tab1)
  baseFrame <- ttklabelframe(tab1,text=gettextRcmdr("Size and randomization"))

  ### widgets for tab1 and base frame
  putRcmdr("nameVar", tclVar(.stored.designfac$nameVar))
  nameEntry <- tkentry(nameFrame, width="20", textvariable=nameVar)
  tkbind(nameEntry, "<FocusIn>", nameenter)
  tkbind(nameEntry, "<FocusOut>", namechange)

  nrunVar <- tclVar(.stored.designfac$nrunVar)
  nrunShow <- ttklabel(baseFrame, width="8", textvariable=nrunVar)
  nrunHint <- ttklabel(baseFrame, text="(product of all numbers of factor levels)", foreground="#888888")
  nfacVar <- tclVar(.stored.designfac$nfacVar)
  nfacEntry <- tkentry(baseFrame, width="8", textvariable=nfacVar)
  tkbind(nfacEntry,"<FocusOut>",nfacchange)
  nrepVar <- tclVar(.stored.designfac$nrepVar)
  nrepEntry <- tkentry(baseFrame, width="8", textvariable=nrepVar)
  nblockVar <- tclVar(.stored.designfac$nblockVar)
  randomizeVariable <-  tclVar(.stored.designfac$cbInitials[2])
  nblockEntry <- tkentry(baseFrame, width="8", textvariable=nblockVar)
  randomizecb <- ttkcheckbutton(baseFrame,text=gettextRcmdr("Randomization"),variable=randomizeVariable)
  tkconfigure(randomizecb, takefocus=0)
  seedVar <- tclVar(sample(31999,1))  ## always new
  seedEntry <- tkentry(baseFrame, width="8", textvariable=seedVar)
  tkconfigure(seedEntry, takefocus=0)
  repeat.onlyVariable <- tclVar(.stored.designfac$cbInitials[1])
  repeat.onlycb <- ttkcheckbutton(baseFrame,text=gettextRcmdr("Repeat only"),variable=repeat.onlyVariable)
  tkconfigure(repeat.onlycb, takefocus=0)

  ## grid base frame
  tkgrid(nrunlab <- tklabel(baseFrame, text=gettextRcmdr("Number of runs")), nrunShow, nrunHint, sticky="w")
  ## omitted nfaccb, on form, nfactors must always be specified
  tkgrid(nfaclab <- tklabel(baseFrame, text=gettextRcmdr("Number of factors")), nfacEntry, sticky="w")
  tkgrid(nblocklab <- tklabel(baseFrame, text=gettextRcmdr("Number of blocks")), nblockEntry, sticky="w")
  tkgrid(nreplab <- tklabel(baseFrame, text=gettextRcmdr("Replications")), nrepEntry, repeat.onlycb, sticky="w")
  tkgrid.configure(nreplab, pady=15)
  tkgrid(randlab <- tklabel(baseFrame, text="You normally do not need to change randomization settings"),sticky="w",columnspan=3)
  tkgrid(seedlab <- tklabel(baseFrame, text=gettextRcmdr("Seed for randomization")), seedEntry,
         randomizecb, sticky="w")

  helptab1Button <- buttonRcmdr(nameFrame, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab1,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab1Button, takefocus=0)

  ### Finalize tab1
  tkgrid(tklabel(nameFrame, text="Name of new design"), nameEntry, helptab1Button, sticky="w")
  tkgrid(nameFrame, sticky="w", columnspan=4)
  tkgrid.configure(nameFrame, pady=40)
  tkgrid.configure(helptab1Button, sticky="ne")
  tkgrid(baseFrame, sticky="nw",columnspan=3)


  ## Factor Details Tab
  ## factor details frame
  ### facnameAutoVariable (not needed any more) and faclevelCommonVariable

  ## factor details
  ## values as vectors
  facnamlistt <- .stored.designfac$facnamlist
  nlevlistt <- .stored.designfac$nlevlist
  faclevlistt <- .stored.designfac$faclevlist
  faclablistt <- .stored.designfac$faclablist
  varlistshortt <- if (as.numeric(tclvalue(nfacVar))<=50)
    Letters[1:tclvalue(nfacVar)] else paste("F",1:tclvalue(nfacVar),sep="")

  enterlistFrame <- ttkframe(tab2)
  listFrame <- ttklabelframe(enterlistFrame, text="Factor Details")
  putRcmdr("selpos", 1)
  putRcmdr("curfac", tclVar(varlistshortt[1]))
  putRcmdr("curfnam", tclVar(facnamlistt[1]))
  putRcmdr("curnlev", tclVar(nlevlistt[1]))
  putRcmdr("curflev", tclVar(faclevlistt[1]))
  putRcmdr("curflab", tclVar(faclablistt[1]))

  ## fsel must select the right factor
  ## this should be highlighted in factor lists
  ##    and all related entries shown for changing in text boxes fnam etc.
  enterFrame <- ttklabelframe(enterlistFrame, text=gettextRcmdr("Modify factor details for selected factor"))
  fsel <- ttkcombobox(enterFrame, textvariable=curfac, width=5, values=varlistshortt, state="readonly")
  tkbind(fsel, "<<ComboboxSelected>>", factorsel)
  #fnam <- ttkentry(listFrame, textvariable=curfnam, width=20,validate="focusout", validatecommand=fnamchange)
  fnam <- ttkentry(enterFrame, textvariable=curfnam, width=15)
  tkbind(fnam, "<FocusOut>", fnamchange)
  nlev <- ttkentry(enterFrame, textvariable=curnlev, width=6)
  tkbind(nlev, "<FocusOut>", nlevchange)
  flev <- ttkentry(enterFrame, textvariable=curflev, width=20)
  tkbind(flev, "<FocusOut>", flevchange)
  flab <- ttkentry(enterFrame, textvariable=curflab, width=20)
  tkbind(flab, "<FocusOut>", flabchange)
  tkbind(flab, "<Key-Tab>", tabflab)
  tkgrid(tklabel(enterFrame,text=gettextRcmdr("Select"),width=6),
         tklabel(enterFrame,text=gettextRcmdr("Factor name"), width=15),
         tklabel(enterFrame,text=gettextRcmdr("No. of \nlevels"), width=6),
         tklabel(enterFrame,text=gettextRcmdr("Levels \n(separate with blanks)"), width=20),
         tklabel(enterFrame,text=gettextRcmdr("Comment or label \n(for html export only)"), width=20),
         sticky="w")
  tkgrid(fsel,fnam, nlev, flev, flab, sticky="w")

  putRcmdr("facnamlist", tclVar(facnamlistt))
  putRcmdr("varlistshort", tclVar(varlistshortt))
  putRcmdr("nlevlist", tclVar(nlevlistt))
  putRcmdr("faclevlist", tclVar(faclevlistt))
  putRcmdr("faclablist", tclVar(faclablistt))

  facshortListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                               selectmode = single, exportselection = "TRUE", listvariable=varlistshort,
                               width = 6, background="#EBEBDC")
  tkbind(facshortListBox, "<<TraverseIn>>",function() tkfocus(fsel))

  facnameListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=facnamlist,
                              width = 15, background="#EBEBDC")
  nlevListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                           selectmode = single, exportselection = "TRUE", listvariable=nlevlist,
                           width = 6, background="#EBEBDC")
  faclevListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                             selectmode = single, exportselection = "TRUE", listvariable=faclevlist,
                             width = 20, background="#EBEBDC")
  faclabListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                             selectmode = single, exportselection = "TRUE", listvariable=faclablist,
                             width = 20, background="#EBEBDC")

  ## determine current index and reordering for onUp and onDown
  tkbind(facshortListBox, "<<ListboxSelect>>", checkIndexShort)
  tkbind(facnameListBox, "<<ListboxSelect>>", checkIndexNam)
  tkbind(nlevListBox, "<<ListboxSelect>>", checkIndexNlev)
  tkbind(faclevListBox, "<<ListboxSelect>>", checkIndexFlev)
  tkbind(faclabListBox, "<<ListboxSelect>>", checkIndexLab)


  ### funktioniert, ist aber noch nicht schön
  scrollbar <- ttkscrollbar(listFrame, command = function(...) {
    tkyview(facshortListBox, ...)
    tkyview(facnameListBox, ...)
    tkyview(nlevListBox, ...)
    tkyview(faclevListBox, ...)
    tkyview(faclabListBox, ...)
  })

  #    tkgrid(tklabel(enterlistFrame,text="  ", width=5),enterFrame, sticky="w")
  tkgrid(enterFrame, sticky="w", columnspan=5)
  tkgrid.configure(enterFrame, pady=10)
  ## Hoch-/Runterschieben von Einträgen ermöglichen

  downupFrame <- ttkframe(listFrame)
  moveDownButton <- buttonRcmdr(downupFrame, text = gettextRcmdr("Move Down"),
                                foreground = "darkgreen", command = onDown,
                                default = "normal", borderwidth = 3, width=12)
  moveUpButton <- buttonRcmdr(downupFrame, text = gettextRcmdr("Move Up"),
                              foreground = "darkgreen", command = onUp,
                              default = "normal", borderwidth = 3, width=12)
  tkgrid(moveDownButton, sticky="w")
  tkgrid(moveUpButton, sticky="w")

  tkgrid(scrollbar, facshortListBox, facnameListBox, nlevListBox, faclevListBox, faclabListBox, downupFrame, sticky = "nw")
  tkgrid.configure(scrollbar, sticky = "wns")
  tkgrid.configure(facnameListBox, sticky = "new")

  helptab2Button <- buttonRcmdr(tab2, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab2,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab2Button, takefocus=0)

  ## finalize tab2 Factor details
  tkgrid(helptab2Button, sticky="e")
  tkgrid(listFrame, columnspan=6,sticky="w")
  tkgrid(enterlistFrame, columnspan=6,sticky="w")

  ## tab6 for exporting
  helptab6Button <- buttonRcmdr(tab6, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab6,
                                default = "normal", borderwidth = 3)

  exportlabVar <- nameVar
  exportlab <- ttklabel(tab6, textvariable=exportlabVar)
  tkgrid(ttklabel(tab6,text="Current design to be saved:"),exportlab,helptab6Button,sticky="w")
  tkgrid.configure(exportlab, pady=15)
  tkgrid.configure(helptab6Button, sticky="ne")

  ## radio buttons for choosing export type
  etradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("(How to) Export ?"))
  etyperbVariable <- tclVar(.stored.designfac$etyperbVariable)
  noexprb <- tkradiobutton(etradioFrame,text=gettextRcmdr("no export"),variable=etyperbVariable,value="none")
  allrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("all file types"),variable=etyperbVariable,value="all")
  rdarb <- tkradiobutton(etradioFrame,text=gettextRcmdr("rda only"),variable=etyperbVariable,value="rda")
  htmlrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("html and rda"),variable=etyperbVariable,value="html")
  csvrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("csv and rda"),variable=etyperbVariable,value="csv")
  tkgrid(noexprb, sticky="w")
  tkgrid(allrb, sticky="w")
  tkgrid(rdarb, sticky="w")
  tkgrid(htmlrb, sticky="w")
  tkgrid(csvrb, sticky="w")

  ## radio buttons for choosing export decimal separator
  decimalradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("Decimal Separator ?"))
  decimalrbVariable <- tclVar(.stored.designfac$decimalrbVariable)
  defaultrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("default"),variable=decimalrbVariable, value="default")
  pointrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("."),variable=decimalrbVariable, value=".")
  commarb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr(","),variable=decimalrbVariable, value=",")
  tkgrid(defaultrb, sticky="w")  ## in this case, leave default option from options
  tkgrid(pointrb, sticky="w")
  tkgrid(commarb, sticky="w")

  ## export directory
  dirFrame <- ttklabelframe(tab6, text=gettextRcmdr("Storage Directory"))
  putRcmdr("dirVar", tclVar(.stored.designfac$dirVar))
  dirEntry <- tkentry(dirFrame, width="50", textvariable=dirVar)
  dirButton <- buttonRcmdr(dirFrame, text = gettextRcmdr("Change directory"),
                           foreground = "darkgreen", width = "20", command = onChangeDir,
                           default = "normal", borderwidth = 3)
  tkgrid(dirEntry, tklabel(dirFrame, text="   "), dirButton, sticky="w")

  ## export file name
  putRcmdr("fileVar", tclVar(.stored.designfac$fileVar))
  fileEntry <- tkentry(tab6, width="20", textvariable=fileVar)
  efnamelabel <- tklabel(tab6,text=gettextRcmdr("Export file names: name below with appropriate endings (html or csv, and rda)"))
  replacecbVariable <- tclVar(.stored.designfac$cbInitials[8])
  replacecb <- ttkcheckbutton(tab6,text=gettextRcmdr("Replace file(s), if exists"),variable=replacecbVariable)

  ## always grid details, as otherwise default file name does not work
  ## design name info and help button have already been gridded above
  tkgrid(etradioFrame, decimalradioFrame, sticky="nw")
  tkgrid(dirFrame, sticky="w", columnspan=5)
  tkgrid.configure(dirFrame, pady=15)
  tkgrid(efnamelabel, sticky="w", columnspan=5)
  tkgrid(fileEntry, sticky="w", columnspan=5)
  tkgrid(replacecb, sticky="w", columnspan=5)


  ## add buttons outside the notebook
  buttonFrame <- tkframe(topdes2)
  ## die sind aber nicht dunkelgruen ...
  refreshButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Refresh form"),
                               foreground = "darkgreen", width = "12", command = onRefresh,
                               default = "normal", borderwidth = 3)
  storeButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Store form"),
                             foreground = "darkgreen", width = "12", command = onStore,
                             default = "normal", borderwidth = 3)
  loadButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Load form"),
                            foreground = "darkgreen", width = "12", command = onLoad,
                            default = "normal", borderwidth = 3)
  resetButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Reset form"),
                             foreground = "darkgreen", width = "12", command = onReset,
                             default = "normal", borderwidth = 3)
  #        tkgrid(refreshButton,sticky="w")
  #        tkgrid(tklabel(buttonFrame,text="  "),sticky="w")
  tkgrid(storeButton,sticky="w")
  tkgrid(loadButton,sticky="w")
  tkgrid(resetButton,sticky="w")

  tkconfigure(refreshButton, takefocus=0)
  tkconfigure(storeButton, takefocus=0)
  tkconfigure(loadButton, takefocus=0)
  tkconfigure(resetButton, takefocus=0)

  ## storage buttons to the right of the notebook
  tkgrid(tn, buttonFrame, sticky="w", columnspan=2)

  OKCancelHelp(window=topdes2, helpSubject="Menu.fac")
  tkconfigure(OKbutton, takefocus=0)
  tkconfigure(cancelButton, takefocus=0)
  tkconfigure(helpButton, takefocus=0)

  tkgrid(buttonsFrame, sticky="ew")


  ### relations among widgets
  if (!as.logical(as.numeric(tclvalue(randomizeVariable)))){
    tkconfigure(seedEntry, state="disabled")
    tkconfigure(seedlab, state="disabled")
  }else {
    tkconfigure(seedEntry, state="normal")
    tkconfigure(seedlab, state="normal")
  }
  if (exists("activestab.tn", where="RcmdrEnv")){
    tcl(tn, "select", activestab.tn)
    rm(activestab.tn, pos="RcmdrEnv")
  }

  dialogSuffix(window=topdes2, rows=2, columns=2, focus=tn, bindReturn=FALSE)

}

Menu.oa <- function(){
  initializeDialogDoE(title=gettextRcmdr("Create main effects design from orthogonal array ..."))
  ## function initializeDialogDoE assumes topdes2 as windowname
  ## last stored top left corner for window is stored under topleft2xy
  ## onRefresh still makes window walk a little


  if (exists("curindex", where="RcmdrEnv")) rm(curindex, pos="RcmdrEnv")

  if (!exists(".stored.designoa",where="RcmdrEnv"))
    assign(".stored.designoa", .default.designoa,pos="RcmdrEnv")
  ## nameVar, nrunVar, nfacVar, nrepVar
  ## cbInitials containing repeat.onlyVariable, randomizeVariable,
  ##                       facnamesAutoVariable, faclevelsCommonVariable,
  ##                       nrunEntryVariable, usenlevelscbVariable
  ##                       specialcbVariable, replacecbVariable, parentcbVariable,
  ##                       res3cbVariable
  ## level1Var, level2Var, seedVar, specialrbVariable, hardVar, genVar,
  ## catlgVar, designVar, designrbVariable, destyperbVariable
  ## resVar, qualcritrbVariable, facnamlist,nlevlist,faclevlist, faclablist
  ## etyperbVariable, decimalrbVariable, dirVar, fileVar
  ## fromVar, toVar

  putRcmdr("nrunslist",c("NULL",as.character(unique(oacat$nruns))))
  idlist <- as.character(oacat$name)
  idlist <- idlist[-which(oacat$lineage=="" &
                            oacat$nruns==sapply(strsplit(idlist,".", fixed=TRUE), function(obj)
                              prod(apply(matrix(as.numeric(obj[-1]), nrow=2), 2, function(obj2) obj2[1]^obj2[2]))))]
  idlist <- c("NULL",idlist)
  putRcmdr("idlist", idlist)

  ## define called functions
  infoClose <- function(){
    putRcmdr("infotxt",tclVar(""))
  }

  onHelpTab1 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.oaTab1"))
  }
  onHelpTab2 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.FacDetailsGenTab"))
  }
  onHelpTab6 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.exportTab"))
  }

  tabpos <- function(){
    ### get 0-based index of currently selected tab
    activestab.tn <- tclvalue(tcl(tn, "select"))
    activestab.tn <- strsplit(activestab.tn,".",fixed=TRUE)[[1]]
    activestab.tn <- as.numeric(activestab.tn[length(activestab.tn)])-1
    activestab.tn
  }

  onInspect <- function(){
    if (tclvalue(fromVar)=="" & tclvalue(toVar)=="") {
      if (!tclvalue(nrunVar)=="NULL") nrunshow <- paste("c(",tclvalue(nrunVar),", 100000),")
      else nrunshow <- "\"all\", "
    }
    else{
      if (tclvalue(fromVar) == "") nrunshow <- paste("c(0,", tclvalue(toVar),"),")
      else {if (tclvalue(toVar) =="") nrunshow <- paste("c(",tclvalue(fromVar),", 100000),")
      else if (tclvalue(toVar)==tclvalue(fromVar)) nrunshow <- paste(tclvalue(toVar),",", sep="")
      else nrunshow <- paste("c(",tclvalue(fromVar),",",tclvalue(toVar),"),")
      }
    }
    if (as.logical(as.numeric(tclvalue(usenlevelscbVariable))))
      nlevelsshow <- paste("nlevels = c(",paste(as.character(tclObj(nlevlist)),collapse=","),"),")
    else nlevelsshow <- ""
    command <- paste("show.oas(nruns = ", nrunshow, nlevelsshow,
                     " parents.only = ", as.logical(as.numeric(tclvalue(parentcbVariable))),
                     ", show = Inf)")
    hilf <- justDoItDoE(command)
    if (class(hilf)[1]=="try-error") {
      logger(gettextRcmdr("Something went wrong, perhaps invalid values for number of runs?"))
      return()
    }
    ## logger(command)  ## braucht man nur für justDoIt
    tkgrab.release(topdes2)
    doItAndPrint(command)
  }

  storeRcmdr <- function(){
    hilf <- list(nameVar=tclvalue(nameVar),idVar = tclvalue(idVar),
                 nrunVar=tclvalue(nrunVar),nfacVar=tclvalue(nfacVar),nrepVar=tclvalue(nrepVar),
                 minrdfVar=tclvalue(minrdfVar),
                 optimVar = tclvalue(optimVar),
                 cbInitials = c(tclvalue(repeat.onlyVariable), tclvalue(randomizeVariable),
                                tclvalue(colnospecifyVariable),0,
                                1,tclvalue(usenlevelscbVariable),
                                0,tclvalue(replacecbVariable),tclvalue(parentcbVariable),
                                0
                 ),
                 seedVar=tclvalue(seedVar),
                 facnamlist=as.character(tclObj(facnamlist)),
                 nlevlist=as.character(tclObj(nlevlist)),
                 colnolist=as.character(tclObj(colnolist)),
                 faclevlist=as.character(tclObj(faclevlist)),
                 faclablist=as.character(tclObj(faclablist)),
                 etyperbVariable=tclvalue(etyperbVariable),
                 decimalrbVariable=tclvalue(decimalrbVariable),
                 dirVar=tclvalue(dirVar), fileVar=tclvalue(fileVar),
                 fromVar=tclvalue(fromVar), toVar=tclvalue(toVar))
    class(hilf) <- c("menu.designoa","list")
    putRcmdr(".stored.designoa",hilf)
  }

  onOK <- function(){
    onRefreshEnd()
    ## store entries so that users do not have to redo everything
    ## in case of stupid mistakes
    storeRcmdr()
    ## seed is not used from previously stored design
    closeDialog(window=topdes2)
    name <- tclvalue(nameVar)
    if (!is.valid.name(name)){
      errorCondition(window=topdes2,recall=Menu.oa,
                     message=paste('"', name, '" ', gettextRcmdr("is not a valid name."), sep=""))
      return()
    }
    if (is.element(name, listObjects()))
    {
      if ("no" == tclvalue(checkReplace(name, gettextRcmdr("Object"))))
      {
        errorCondition(window=topdes2,recall=Menu.oa,
                       gettextRcmdr("Introduce another name for the new data.frame, or allow replacing."))
        return()
      }
    }
    if (any(getRcmdr(".stored.designoa")$nlevlist=="") )
    {
      errorCondition(window=topdes2,recall=Menu.oa,
                     gettextRcmdr("Factor details are not completely specified."))
      return()
    }
    if (any(getRcmdr(".stored.designoa")$faclevlist=="") )
    {
      errorCondition(window=topdes2,recall=Menu.oa,
                     gettextRcmdr("Factor details are not completely specified."))
      return()
    }
    ###  further error messages with return to menu ?


    textfactornameslist.forcommand <- paste("factor.names=list(",paste(paste(as.character(tclObj(facnamlist)),"=c(",
                                                                             sapply(strsplit(as.character(tclObj(faclevlist))," "),function(obj) paste(dquote(obj),collapse=",")),
                                                                             ")",sep=""),collapse=","),")")


    columns <- dquote("order")
    if (as.logical(as.numeric(as.character(tclvalue(colnospecifyVariable)))))
      columns <- paste("c(",paste(as.character(tclObj(colnolist)), collapse=","),")")

    if (columns == dquote("order") & !tclvalue(optimVar)=="none")
      columns <- dquote(tclvalue(optimVar))

    command <- paste("oa.design(ID=",tclvalue(idVar),",nruns=",tclvalue(nrunVar),
                     ",nfactors=",tclvalue(nfacVar),",replications=",
                     tclvalue(nrepVar),",repeat.only=",as.logical(as.numeric(tclvalue(repeat.onlyVariable))),
                     ",randomize=",as.logical(as.numeric(tclvalue(randomizeVariable))),",seed=",tclvalue(seedVar),
                     ",nlevels=c(", paste(as.character(tclObj(nlevlist)),collapse=","),
                     "),",textfactornameslist.forcommand, ", columns =", columns,
                     ", min.residual.df=", tclvalue(minrdfVar), ")")

    logger(gettextRcmdr("## Trying to create design ... "))
    hilf <- justDoItDoE(command)
    if (class(hilf)[1]=="try-error") {
      Message(paste(gettextRcmdr("Offending command:"), "\n", command), type="error")
      errorCondition(window=topdes2,recall=Menu.oa, message=gettextRcmdr(hilf))
      return()
    }
    logger(paste(name, "<-", command))
    logger("## creator element of design.info will be different, when using the command line command!")
    ## change creator to contain menu settings
    hilfatt <- design.info(hilf)
    hilfatt$creator <- .stored.designoa
    class(hilfatt$creator) <- c("menu.designoa", "list")
    attr(hilf, "design.info") <- hilfatt
    putRcmdr("hilf", hilf)
    ## replace assign by justDoIt; assign(name, hilf, envir=.GlobalEnv)
    justDoIt(paste(name, "<- getRcmdr(\"hilf\")"))
    rm("hilf", pos="RcmdrEnv")
    activeDataSet(name)
    ### exporting
    if (!tclvalue(etyperbVariable)=="none"){
      putRcmdr("path", tclvalue(dirVar))
      putRcmdr("filename", tclvalue(fileVar))
      if (!as.logical(as.numeric(tclvalue(replacecbVariable)))){
        lf <- tolower(list.files(path = path))
        if (tolower(paste(filename, "rda", sep = ".")) %in% lf)
          stop("file ", paste(filename, "rda", "."), " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="html" & tolower(paste(filename, "html", sep = ".")) %in% lf)
          stop("file ", paste(filename, "html", "."), " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="csv" & tolower(paste(filename, "csv", sep = ".")) %in% lf)
          stop("file ", paste(filename, "csv", "."), " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
      }
      if (tclvalue(decimalrbVariable)=="default") command <- paste("export.design(",name,
                                                                   ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                                                                   as.logical(as.numeric(tclvalue(replacecbVariable))),")",sep="")
      else command <- paste("export.design(",name,
                            ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                            as.logical(as.numeric(tclvalue(replacecbVariable))),", OutDec=", dquote(tclvalue(decimalrbVariable)),")",sep="")
      hilf <- justDoItDoE(command)
      if (class(hilf)[1]=="try-error") {
        errorCondition(window=topdes2,recall=Menu.oa, message=gettextRcmdr(hilf))
        return()
      }
      logger(command)
    }
    rm(activestab.tn, pos="RcmdrEnv")
    tkwm.deiconify(CommanderWindow())
    tkfocus(CommanderWindow())
  }

  listDesignoa <- function (envir = .GlobalEnv, ...)
  {
    Vars <- ls(envir = envir, all.names = TRUE)
    Vars[which(sapply(Vars, function(.x){
      aus <- FALSE
      if ("menu.designoa" %in% class(get(.x, envir = envir))) aus <- TRUE
      else if ("design" %in% class(get(.x, envir = envir)))
        if ("menu.designoa" %in% class(design.info(get(.x, envir = envir))$creator))
          aus <- TRUE
        aus
    }))]
  }


  onLoad <- function(){
    ## seems to work now, needs to be tested!
    hilf <- listDesignoa()
    if (length(hilf)==0) {
      tkmessageBox(message=gettextRcmdr("There are no stored design inputs in this session."),
                   icon="error", type="ok", title="no stored design inputs")
      return()
    }
    putRcmdr("deschooseoa",tktoplevel())
    tkwm.title(deschooseoa, gettextRcmdr("Choose stored design form"))
    position <- if (is.SciViews())
      -1
    else position <- "+50+50"
    tkwm.geometry(deschooseoa, position)
    putRcmdr("lb", variableListBox(deschooseoa, variableList=hilf, title="Choose stored design form"))
    tkgrid(lb$frame)
    onOK <- function() {
      putRcmdr(".stored.designoa",get(lb$varlist[as.numeric(tclvalue(tcl(lb$listbox, "curselection")))+1]))
      if ("design" %in% class(getRcmdr(".stored.designoa")))
        putRcmdr(".stored.designoa", design.info(getRcmdr(".stored.designoa"))$creator)
      tkfocus(CommanderWindow())
      tkdestroy(topdes2)
      tkdestroy(deschooseoa)
      Menu.oa()
    }
    OKCancelHelp(window=deschooseoa)
    tkgrid(buttonsFrame, sticky="s")
    dialogSuffix(window=deschooseoa, rows=1, columns=1,
                 focus=lb$listbox)
  }

  onRefreshEnd <- function(){
    nfacchange()
    storeRcmdr()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    putRcmdr("activestab.tn",tabpos())
    ID <- topdes2$ID
    putRcmdr("topleft2xy",as.numeric(c(tclvalue(.Tcl(paste("winfo rootx", ID))),
                                       tclvalue(.Tcl(paste("winfo rooty", ID))))))
    #        assign("activestab.tn",strsplit(activestab.tn,".",fixed=TRUE)[[1]],pos="RcmdrEnv")
    #        assign("activestab.tn",as.numeric(activestab.tn[length(activestab.tn)])-1,pos="RcmdrEnv")
  }

  onRefresh <- function(){
    #print(as.character(tclObj(tcl(tn, "select"))))
    onRefreshEnd()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.oa()
  }

  onStore <- function(){
    ## Speichernamen abfragen und hier ermöglichen (statt stored.designoa)
    textentry() ## creates text string stored in savename.RcmdrPlugin.DoE
    if (!is.null(savename.RcmdrPlugin.DoE)){
      if (!is.valid.name(savename.RcmdrPlugin.DoE)){
        textcorrect(gettextRcmdr("This is not a valid name. Please correct:"))
        return()
      }
      if (is.element(savename.RcmdrPlugin.DoE, listObjects()))
      {
        if ("no" == tclvalue(checkReplace(savename.RcmdrPlugin.DoE, gettextRcmdr("Object"))))
        {
          textcorrect(gettextRcmdr("Please enter a new name:"))
          return()
        }
      }
      storeRcmdr()
      ## replace assign by justDoIt; assign(savename.RcmdrPlugin.DoE, getRcmdr(".stored.designoa"), envir=.GlobalEnv)
      justDoIt(paste(savename.RcmdrPlugin.DoE, "<- getRcmdr(\".stored.designoa\")"))
      message(gettextRcmdr("inputs have been stored"))
    }
  }

  onReset <- function(){
    assign(".stored.designoa",.default.designoa,pos="RcmdrEnv")
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.oa()
  }

  nfacchange <- function(){
    nfacold <- length(as.character(tclObj(varlistshort)))
    nfacnew <- as.numeric(tclvalue(nfacVar))
    if (nfacold==nfacnew) return()
    if (nfacnew < nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[1:nfacnew]))
      putRcmdr("nlevlist", tclVar(as.character(tclObj(nlevlist))[1:nfacnew]))
      putRcmdr("colnolist", tclVar(as.character(tclObj(colnolist))[1:nfacnew]))
      putRcmdr("faclevlist", tclVar(as.character(tclObj(faclevlist))[1:nfacnew]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[1:nfacnew]))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(nlevListBox, listvariable=nlevlist, height=min(10,nfacnew))
      tkconfigure(colnoListBox, listvariable=colnolist, height=min(10,nfacnew))
      tkconfigure(faclevListBox, listvariable=faclevlist, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      if (selpos > nfacnew){
        tcl(fsel, "current", "0")
        factorsel()
      }
    }
    if (nfacnew > nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist", tclVar(c(as.character(tclObj(facnamlist)),
                                      getRcmdr("varlistshortt")[(nfacold+1):nfacnew])) )
      putRcmdr("nlevlist", tclVar(c(as.character(tclObj(nlevlist)),
                                    rep("",nfacnew-nfacold))))
      putRcmdr("colnolist", tclVar(c(as.character(tclObj(colnolist)),
                                     rep("",nfacnew-nfacold))))
      putRcmdr("faclevlist", tclVar(c(as.character(tclObj(faclevlist)),
                                      rep("",nfacnew-nfacold))))
      putRcmdr("faclablist", tclVar(c(as.character(tclObj(faclablist)),
                                      rep("",nfacnew-nfacold))))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      tkconfigure(nlevListBox, listvariable=nlevlist, height=min(10,nfacnew))
      tkconfigure(colnoListBox, listvariable=colnolist, height=min(10,nfacnew))
      tkconfigure(faclevListBox, listvariable=faclevlist, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))
    }
  }
  nameenter <- function(){
    if (identical(tclvalue(getRcmdr("fileVar")),tclvalue(getRcmdr("nameVar"))))
      putRcmdr("name.equal.filename", TRUE)
    else putRcmdr("name.equal.filename", FALSE)
  }
  namechange <- function(){
    if (is.valid.name(tclvalue(nameVar))){
      if (name.equal.filename){
        putRcmdr("fileVar", tclVar(tclvalue(nameVar)))  ## otherwise, variables would be directly tied
        #          putRcmdr("exportlabVar", tclVar(paste("Current design to be saved:", tclvalue(nameVar),"\n   ")))
        tkconfigure(fileEntry, textvariable=getRcmdr("fileVar"))
        #          tkconfigure(exportlab, textvariable=getRcmdr("exportlabVar"))
      }
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid design name")
  }


  factorsel<-function(){
    #### aendert die in der Textbox dargestellte Auswahl
    #### ruiniert aber leider auch wieder die korrekte Ueberschreibung der Werte
    putRcmdr("selpos", as.numeric(tclvalue(tcl(fsel, "current")))+1)
    putRcmdr("curfac", tclVar(as.character(tclObj(varlistshort))[selpos]))
    putRcmdr("curfnam", tclVar(as.character(tclObj(facnamlist))[selpos]))
    putRcmdr("curnlev", tclVar(as.character(tclObj(nlevlist))[selpos]))
    putRcmdr("curcolno", tclVar(as.character(tclObj(colnolist))[selpos]))
    putRcmdr("curflev", tclVar(as.character(tclObj(faclevlist))[selpos]))
    putRcmdr("curflab", tclVar(as.character(tclObj(faclablist))[selpos]))
    tkconfigure(fnam, textvariable=curfnam)
    tkconfigure(nlev, textvariable=curnlev)
    tkconfigure(colno, textvariable=curcolno)
    tkconfigure(flev, textvariable=curflev)
    tkconfigure(flab, textvariable=curflab)
  }

  idsel<-function(){
    #### aendert die in der Textbox dargestellte Auswahl
    #### ruiniert aber leider auch wieder die korrekte Ueberschreibung der Werte
    putRcmdr("idpos", as.numeric(tclvalue(tcl(idEntry, "current"))) + 1)
    if (idpos==1) {
      putRcmdr("colnospecifyVariable", tclVar("0"))
      putRcmdr("colnolist", tclVar(rep("",as.numeric(tclvalue(nfacVar)))))
      tkconfigure(colnoListBox, listvariable=colnolist)
      tkconfigure(colnospecifycb, variable=colnospecifyVariable)
    }
    else {
      hilf <- eval(parse(text=paste("oa.design(",tclvalue(idVar),",randomize=FALSE)")))
      putRcmdr("nrunVar", tclVar(nrow(hilf)))
      if (as.numeric(tclvalue(nfacVar)) > ncol(hilf))
        tkmessageBox(message=gettextRcmdr("The chosen design cannot accomodate the chosen number of factors!"),
                     icon="error", type="ok", title=gettextRcmdr("Too many factors requested for this design"))
    }
    onRefresh()
  }

  nrunsel<-function(){
    #### aendert die in der Textbox dargestellte Auswahl
    #### ruiniert aber leider auch wieder die korrekte Ueberschreibung der Werte
    putRcmdr("nrunpos", as.numeric(tclvalue(tcl(nrunEntry, "current")))+1)
    if (!tclvalue(idVar)=="NULL")
      if (nrunpos>1 & !nrow(eval(parse(text=paste("oa.design(",tclvalue(idVar),",randomize=FALSE)"))))>=tclvalue(nrunVar))
        tkmessageBox(message=gettextRcmdr("mismatch between chosen design and number of runs!"),
                     icon="error", type="ok", title=gettextRcmdr("Number of runs does not match chosen design"))
  }
  fnamchange <- function(){
    ## selpos known from factorsel
    if (is.valid.name(tclvalue(curfnam))){
      hilf <- as.character(tclObj(facnamlist))
      hilf[selpos] <- tclvalue(curfnam)
      ### "facnamlist" is not automatically updated in the listbox
      ### therefore the tkconfigure
      putRcmdr("facnamlist",tclVar(hilf))
      tkconfigure(facnameListBox, listvariable=facnamlist)
    }
    else tkmessageBox(message=gettextRcmdr("invalid name!"),
                      icon="error", type="ok", title=gettextRcmdr("Invalid factor name"))
  }
  colnochange <- function(){
    ## selpos known from factorsel
    if (length(as.character(tclObj(curcolno)))==1){
      hilf <- as.character(tclObj(colnolist))
      hilf[selpos] <- tclvalue(curcolno)
      putRcmdr("colnolist",tclVar(hilf))
      tkconfigure(colnoListBox, listvariable=colnolist)
      nncol <- ncol(eval(parse(text=tclvalue(idVar))))
      if (is.na(as.numeric(tclvalue(curcolno))) | !floor(as.numeric(tclvalue(curcolno)))==as.numeric(tclvalue(curcolno))
          | (as.numeric(tclvalue(curcolno)) > nncol ))
        tkmessageBox(message=gettextRcmdr("an integer number from 1 to ", nncol, " was expected, please correct!"),
                     icon="error", type="ok", title=gettextRcmdr("Invalid column number"))
      else{
        hilf <- as.numeric(as.character(tclObj(curcolno)))
        nnlev <- length(unique(eval(parse(text=tclvalue(idVar)))[,hilf]))
        putRcmdr("curnlev", tclVar(nnlev))
        tkconfigure(nlev, textvariable=curnlev)
        nlevchange()}
    }
    else tkmessageBox(message=gettextRcmdr("Empty entries or entries with blanks are not permitted, please correct!"),
                      icon="error", type="ok", title=gettextRcmdr("Invalid column number"))
  }
  nlevchange <- function(){
    ## selpos known from factorsel
    if (length(as.character(tclObj(curnlev)))==1){
      hilf <- as.character(tclObj(nlevlist))
      hilf[selpos] <- tclvalue(curnlev)
      putRcmdr("nlevlist",tclVar(hilf))
      tkconfigure(nlevListBox, listvariable=nlevlist)
    }
    else tkmessageBox(message="Empty entries or entries with blanks are not permitted, please correct!",
                      icon="error", type="ok", title="Invalid number of levels")
    if (is.na(as.numeric(tclvalue(curnlev))) | !floor(as.numeric(tclvalue(curnlev)))==as.numeric(tclvalue(curnlev)))
      tkmessageBox(message=gettextRcmdr("an integer number was expected, please correct!"),
                   icon="error", type="ok", title=gettextRcmdr("Invalid number of levels"))
    if (length(as.character(tclObj(curflev)))==0){
      flevchange()
      tkconfigure(flev, textvariable=curflev)}
  }

  flevchange <- function(){
    ## selpos known from factorsel
    nlev <- as.numeric(tclvalue(curnlev))
    nlevspec <- length(as.character(tclObj(curflev)))
    if (nlevspec==nlev){
      hilf <- as.character(tclObj(faclevlist))
      hilf[selpos] <- tclvalue(curflev)
      putRcmdr("faclevlist",tclVar(hilf))
      tkconfigure(faclevListBox, listvariable=faclevlist)
    }
    else{
      if (nlevspec==0){
        putRcmdr("curflev", tclVar(paste(1:nlev, sep=" ")))
        hilf <- as.character(tclObj(faclevlist))
        hilf[selpos] <- tclvalue(curflev)
        putRcmdr("faclevlist",tclVar(hilf))
        tkconfigure(faclevListBox, listvariable=faclevlist)
      }
      else tkmessageBox(message=paste(gettextRcmdr(nlev," levels entries needed, \nyou specified ", nlevspec, " levels, please correct!"),collapse=" "),
                        icon="error", type="ok", title="Invalid number of level entries")
    }
  }
  flabchange <- function(){
    ## selpos known from factorsel
    ## for FocusOut event on flab
    ## still problematic, if Focus out occurs with tab
    ## as there is also a tab key event
    hilf <- as.character(tclObj(faclablist))
    hilf[selpos] <- tclvalue(curflab)
    putRcmdr("faclablist",tclVar(hilf))
    tkconfigure(faclabListBox, listvariable=faclablist)
  }

  tabflab <- function(){
    ## for Tab key event on flab
    ## the traversal still jumps to the first traversable control on the sheet
    ## (rather than staying with fnam, if asked by tkfocus to do so)
    ## takefocus has so far been set to 0 for all widgets except the factor detail ones on this tab
    flabchange()  ## otherwise, not carried out!
    hilf <- as.numeric(tclvalue(tcl(fsel,"current")))+1
    if (hilf  >= as.numeric(tclvalue(nfacVar))) return()
    tcl(fsel,"current", hilf)
    factorsel()
    #tkfocus(fnam)
    #tcl(fnam, "selection", "range", 1, "end")
    #tcl("break")
  }


  swap <- function(a,b){
    hilf <- 1:as.numeric(tclvalue(nfacVar))
    hilf[b] <- a
    hilf[a] <- b
    hilf
  }

  indexchange <- function(){
    if (curindex < as.numeric(tclvalue(nfacVar)))
      putRcmdr("orderDown",swap(curindex, curindex+1))
    if (curindex > 1)
      putRcmdr("orderUp",swap(curindex, curindex-1))
    tcl(fsel, "current", curindex-1)
    factorsel()
  }

  checkIndexShort <- function(){
    putRcmdr("curindex", as.numeric(tcl(facshortListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexNam <- function(){
    putRcmdr("curindex", as.numeric(tcl(facnameListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexColno <- function(){
    putRcmdr("curindex", as.numeric(tcl(colnoListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexNlev <- function(){
    putRcmdr("curindex", as.numeric(tcl(nlevListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexFlev <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclevListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLab <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclabListBox,"curselection"))+1)
    indexchange()
  }

  onUp <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex=="1" | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderUp]))
      putRcmdr("nlevlist", tclVar(as.character(tclObj(nlevlist))[orderUp]))
      putRcmdr("colnolist", tclVar(as.character(tclObj(colnolist))[orderUp]))
      putRcmdr("faclevlist", tclVar(as.character(tclObj(faclevlist))[orderUp]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderUp]))
      tkconfigure(nlevListBox, listvariable=nlevlist)
      tkconfigure(colnoListBox, listvariable=colnolist)
      tkconfigure(faclevListBox, listvariable=faclevlist)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex-1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(nlevListBox,"selection","set",curindex-1)
      tcl(colnoListBox,"selection","set",curindex-1)
      tcl(faclevListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  onDown <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex==as.numeric(tclvalue(nfacVar)) | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderDown]))
      putRcmdr("nlevlist", tclVar(as.character(tclObj(nlevlist))[orderDown]))
      putRcmdr("colnolist", tclVar(as.character(tclObj(colnolist))[orderDown]))
      putRcmdr("faclevlist", tclVar(as.character(tclObj(faclevlist))[orderDown]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderDown]))
      tkconfigure(nlevListBox, listvariable=nlevlist)
      tkconfigure(colnoListBox, listvariable=colnolist)
      tkconfigure(faclevListBox, listvariable=faclevlist)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex+1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(nlevListBox,"selection","set",curindex-1)
      tcl(colnoListBox,"selection","set",curindex-1)
      tcl(faclevListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  dquote <- function(obj){
    ## quote vector elements for use as character vector in a command
    aus <- rep("",length(obj))
    wopt <- options("warn")[[1]]
    options(warn=-1)
    for (i in 1:length(obj)) if (is.na(as.numeric(obj[i]))) {
      if (length(grep('"',obj[i])>0))
        aus[i] <- paste("'",obj[i],"'",sep="")
      else
        aus[i] <- paste('"',obj[i],'"',sep="")
    }
    else aus[i] <- obj[i]
    options(warn=wopt)
    aus
  }


  onChangeDir <- function(){
    putRcmdr("direct",tclvalue(tkchooseDirectory()))
    if (!direct=="") {
      putRcmdr("dirVar", tclVar(direct))
      tkconfigure(dirEntry, textvariable = dirVar)
    }
  }

  ######## end define functions


  ##### define userform
  #tn <- ttknotebook(top,height=100, width=500)

  putRcmdr("tn",ttknotebook(topdes2))
  #tn <- ttknotebook(topdes2)

  putRcmdr("tab1",ttkframe(tn))
  putRcmdr("tab2",ttkframe(tn))
  putRcmdr("tab6",ttkframe(tn))## called 6 because of parallel treatment with
  ## fractional factorial menu

  tkadd(tn,tab1,text="Base Settings")   ### tabid=0
  tkadd(tn,tab2,text="Factor Details")  ### tabid=1
  tkadd(tn,tab6,text="Export") ### tabid=5

  tkconfigure(tn, takefocus=0)

  nameFrame <- ttkframe(tab1)
  baseFrame <- ttklabelframe(tab1,text=gettextRcmdr("Size and randomization"))

  ### widgets for tab1 and base frame
  putRcmdr("nameVar", tclVar(.stored.designoa$nameVar))
  nameEntry <- tkentry(nameFrame, width="20", textvariable=nameVar)
  tkbind(nameEntry, "<FocusIn>", nameenter)
  tkbind(nameEntry, "<FocusOut>", namechange)

  nfacVar <- tclVar(.stored.designoa$nfacVar)
  nfacEntry <- tkentry(baseFrame, width="8", textvariable=nfacVar)
  nfacHint <- ttklabel(baseFrame, text="(required!)", foreground="#888888")
  tkbind(nfacEntry,"<FocusOut>",nfacchange)

  idVar <- tclVar(.stored.designoa$idVar)
  putRcmdr("idpos", which(idlist %in% tclvalue(idVar)))
  idEntry <- ttkcombobox(baseFrame, textvariable=idVar, width=20, values=idlist, state="readonly")
  tkbind(idEntry, "<<ComboboxSelected>>", idsel)
  idHint <- ttklabel(baseFrame, text="(select, if desired)", foreground="#888888")

  nrunVar <- tclVar(.stored.designoa$nrunVar)
  putRcmdr("nrunpos", which(nrunslist %in% tclvalue(nrunVar)))
  nrunEntry <- ttkcombobox(baseFrame, textvariable=nrunVar, width=5, values=nrunslist, state="readonly")
  tkbind(nrunEntry, "<<ComboboxSelected>>", nrunsel)
  nrunHint <- ttklabel(baseFrame, text="(select, if desired)", foreground="#888888")

  ### allow for old version stored settings
  if (is.null(.stored.designoa$optimVar)) optimVar <- tclVar("none")
  else optimVar <- tclVar(.stored.designoa$optimVar)

  putRcmdr("optimlist", c("none","min3","min34"))
  putRcmdr("optimpos", which(optimlist %in% tclvalue(optimVar)))
  optimEntry <- ttkcombobox(baseFrame, textvariable=optimVar, width=5, values=optimlist, state="readonly")

  minrdfVar <- tclVar(.stored.designoa$minrdfVar)
  minrdfEntry <- tkentry(baseFrame, width="8", textvariable=minrdfVar)
  nrepVar <- tclVar(.stored.designoa$nrepVar)
  nrepEntry <- tkentry(baseFrame, width="8", textvariable=nrepVar)
  randomizeVariable <-  tclVar(.stored.designoa$cbInitials[2])
  randomizecb <- ttkcheckbutton(baseFrame,text=gettextRcmdr("Randomization"),variable=randomizeVariable)
  tkconfigure(randomizecb, takefocus=0)
  seedVar <- tclVar(sample(31999,1))  ## always new
  seedEntry <- tkentry(baseFrame, width="8", textvariable=seedVar)
  tkconfigure(seedEntry, takefocus=0)
  repeat.onlyVariable <- tclVar(.stored.designoa$cbInitials[1])
  repeat.onlycb <- ttkcheckbutton(baseFrame,text=gettextRcmdr("Repeat only"),variable=repeat.onlyVariable)
  tkconfigure(repeat.onlycb, takefocus=0)

  inspectFrame <- ttklabelframe(tab1,text=gettextRcmdr("Inspect avaialable designs"))
  fromVar <- tclVar(as.character(.stored.designoa$fromVar))  ## as.character should also make it work
  toVar <- tclVar(as.character(.stored.designoa$toVar))      ## with stored designs from previous version
  fromRuns <- tkentry(inspectFrame, width="5", textvariable=fromVar)
  toRuns <- tkentry(inspectFrame, width="5", textvariable=toVar)
  tkgrid(tklabel(inspectFrame, text=gettextRcmdr("Number of of runs")), sticky="w", columnspan=4)
  tkgrid(tklabel(inspectFrame, text="from"), fromRuns, tklabel(inspectFrame, text="to"), toRuns)

  inspectButton <- buttonRcmdr(inspectFrame, text=gettextRcmdr("Show available designs\n     The menu remains open, \n     fetch it back after looking at designs"),
                               foreground = "darkgreen", command = onInspect,
                               default = "normal", borderwidth = 3)
  tkconfigure(inspectButton, takefocus=0)
  parentcbVariable <- tclVar(.stored.designoa$cbInitials[9])
  parentcb <- ttkcheckbutton(inspectFrame,variable=parentcbVariable,text=gettextRcmdr("Restrict to parent arrays"))
  tkconfigure(parentcb, takefocus=0)
  tkgrid(parentcb, columnspan=4, sticky="w")

  usenlevelscbVariable <- tclVar(.stored.designoa$cbInitials[6])
  usenlevelscb <- ttkcheckbutton(inspectFrame,variable=usenlevelscbVariable,text=gettextRcmdr("Use level information from factor details"))
  tkconfigure(usenlevelscb, takefocus=0)
  tkgrid(usenlevelscb, columnspan=4, sticky="w")

  tkgrid(inspectButton, columnspan=4)

  ## preparations for bottom frame
  bottomFrame <- tkframe(topdes2)

  ## grid base frame
  ## omitted nfaccb, on form, nfactors must always be specified
  tkgrid(nfaclab <- tklabel(baseFrame, text=gettextRcmdr("Number of factors")), nfacEntry, nfacHint, sticky="w")
  tkgrid(idlab <- tklabel(baseFrame, text=gettextRcmdr("Specific array")), idEntry, idHint, sticky="w")
  tkgrid(optimlab <- tklabel(baseFrame, text=gettextRcmdr("Automatic optimization")), optimEntry, sticky="w")
  tkgrid(nrunlab <- tklabel(baseFrame, text=gettextRcmdr("Minimum number of runs")), nrunEntry, nrunHint, sticky="w")

  tkgrid(minrdflab <- tklabel(baseFrame, text=gettextRcmdr("Minimum residual df")), minrdfEntry, sticky="w", pady=c(15,0))
  tkgrid(nreplab <- tklabel(baseFrame, text=gettextRcmdr("Replications")), nrepEntry, repeat.onlycb, sticky="w")
  tkgrid.configure(nreplab, pady=15)
  tkgrid(randlab <- tklabel(baseFrame, text="You normally do not need to change randomization settings"),sticky="w",columnspan=3)
  tkgrid(seedlab <- tklabel(baseFrame, text=gettextRcmdr("Seed for randomization")), seedEntry,
         randomizecb, sticky="w")

  helptab1Button <- buttonRcmdr(nameFrame, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab1,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab1Button, takefocus=0)

  ### Finalize tab1
  tkgrid(tklabel(nameFrame, text="Name of new design"), nameEntry, helptab1Button, sticky="w")
  tkgrid(nameFrame, sticky="w", columnspan=4)
  tkgrid.configure(nameFrame, pady=c(5,20))
  tkgrid.configure(helptab1Button, sticky="ne")
  tkgrid(baseFrame, inspectFrame, sticky="nw",columnspan=3)

  ## Factor Details Tab
  ## factor details frame
  ### facnameAutoVariable (not needed any more) and faclevelCommonVariable

  putRcmdr("colnospecifyVariable", tclVar(.stored.designoa$cbInitials[3]))
  colnoFrame <- tkframe(tab2)
  colnospecifycb <- ttkcheckbutton(colnoFrame,text=gettextRcmdr("Manually specify column numbers for array ?"),
                                   variable=colnospecifyVariable, command=onRefresh)

  tkgrid(colnospecifycb, sticky="w")
  tkgrid(colnohint <- tklabel(colnoFrame, text=gettextRcmdr("useful for experts in customizing design properties\n(default column allocations are not always best)")),sticky="w")
  if (idpos > 1) tkgrid(colnoFrame, pady=10,sticky="w")

  ## factor details
  ## values as vectors
  facnamlistt <- .stored.designoa$facnamlist
  nlevlistt <- .stored.designoa$nlevlist
  colnolistt <- .stored.designoa$colnolist
  faclevlistt <- .stored.designoa$faclevlist
  faclablistt <- .stored.designoa$faclablist
  varlistshortt <- if (as.numeric(tclvalue(nfacVar))<=50)
    Letters[1:tclvalue(nfacVar)] else paste("F",1:tclvalue(nfacVar),sep="")

  enterlistFrame <- ttkframe(tab2)
  listFrame <- ttklabelframe(enterlistFrame, text="Factor Details")
  putRcmdr("selpos", 1)
  putRcmdr("curfac", tclVar(varlistshortt[1]))
  putRcmdr("curfnam", tclVar(facnamlistt[1]))
  putRcmdr("curcolno", tclVar(colnolistt[1]))
  putRcmdr("curnlev", tclVar(nlevlistt[1]))
  putRcmdr("curflev", tclVar(faclevlistt[1]))
  putRcmdr("curflab", tclVar(faclablistt[1]))

  ## fsel must select the right factor
  ## this should be highlighted in factor lists
  ##    and all related entries shown for changing in text boxes fnam etc.
  enterFrame <- ttklabelframe(enterlistFrame, text=gettextRcmdr("Modify factor details for selected factor"))
  fsel <- ttkcombobox(enterFrame, textvariable=curfac, width=5, values=varlistshortt, state="readonly")
  tkbind(fsel, "<<ComboboxSelected>>", factorsel)
  #fnam <- ttkentry(listFrame, textvariable=curfnam, width=20,validate="focusout", validatecommand=fnamchange)
  fnam <- ttkentry(enterFrame, textvariable=curfnam, width=15)
  tkbind(fnam, "<FocusOut>", fnamchange)
  colno <- ttkentry(enterFrame, textvariable=curcolno, width=6)
  tkbind(colno, "<FocusOut>", colnochange)
  if (tclvalue(idVar)=="NULL") tkconfigure(colno, state="disabled")
  nlev <- ttkentry(enterFrame, textvariable=curnlev, width=6)
  tkbind(nlev, "<FocusOut>", nlevchange)
  flev <- ttkentry(enterFrame, textvariable=curflev, width=20)
  tkbind(flev, "<FocusOut>", flevchange)
  if (idpos>1 & tclvalue(colnospecifyVariable)=="1"){
    flab <- ttkentry(enterFrame, textvariable=curflab, width=15)
  } else
    flab <- ttkentry(enterFrame, textvariable=curflab, width=20)
  tkbind(flab, "<FocusOut>", flabchange)
  tkbind(flab, "<Key-Tab>", tabflab)
  if (idpos>1 & tclvalue(colnospecifyVariable)=="1"){
    tkgrid(tklabel(enterFrame,text=gettextRcmdr("Select"),width=6),
           tklabel(enterFrame,text=gettextRcmdr("Factor name"), width=15),
           tklabel(enterFrame,text=gettextRcmdr("Column \nnumber"), width=6),
           tklabel(enterFrame,text=gettextRcmdr("no. of \nlevels"), width=6),
           tklabel(enterFrame,text=gettextRcmdr("Levels \n(separate with blanks)"), width=20),
           tklabel(enterFrame,text=gettextRcmdr("Comment/label \nfor html export"), width=15),
           sticky="w")
    tkgrid(fsel,fnam, colno, nlev, flev, flab, sticky="w")} else{
      tkgrid(tklabel(enterFrame,text=gettextRcmdr("Select"),width=6),
             tklabel(enterFrame,text=gettextRcmdr("Factor name"), width=15),
             tklabel(enterFrame,text=gettextRcmdr("no. of \nlevels"), width=6),
             tklabel(enterFrame,text=gettextRcmdr("Levels \n(separate with blanks)"), width=20),
             tklabel(enterFrame,text=gettextRcmdr("Comment or label \n(for html export only)"), width=20),
             sticky="w")
      tkgrid(fsel,fnam, nlev, flev, flab, sticky="w")
    }

  putRcmdr("facnamlist", tclVar(facnamlistt))
  putRcmdr("varlistshort", tclVar(varlistshortt))
  putRcmdr("nlevlist", tclVar(nlevlistt))
  putRcmdr("colnolist", tclVar(colnolistt))
  putRcmdr("faclevlist", tclVar(faclevlistt))
  putRcmdr("faclablist", tclVar(faclablistt))

  facshortListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                               selectmode = single, exportselection = "TRUE", listvariable=varlistshort,
                               width = 6, background="#EBEBDC")
  tkbind(facshortListBox, "<<TraverseIn>>",function() tkfocus(fsel))

  facnameListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=facnamlist,
                              width = 15, background="#EBEBDC")
  colnoListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                            selectmode = single, exportselection = "TRUE", listvariable=colnolist,
                            width = 6, background="#EBEBDC")
  nlevListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                           selectmode = single, exportselection = "TRUE", listvariable=nlevlist,
                           width = 6, background="#EBEBDC")
  faclevListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                             selectmode = single, exportselection = "TRUE", listvariable=faclevlist,
                             width = 20, background="#EBEBDC")
  if (idpos>1 & tclvalue(colnospecifyVariable)=="1")
    faclabListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                               selectmode = single, exportselection = "TRUE", listvariable=faclablist,
                               width = 15, background="#EBEBDC") else
                                 faclabListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                                                            selectmode = single, exportselection = "TRUE", listvariable=faclablist,
                                                            width = 20, background="#EBEBDC")

  ## determine current index and reordering for onUp and onDown
  tkbind(facshortListBox, "<<ListboxSelect>>", checkIndexShort)
  tkbind(facnameListBox, "<<ListboxSelect>>", checkIndexNam)
  tkbind(nlevListBox, "<<ListboxSelect>>", checkIndexNlev)
  tkbind(colnoListBox, "<<ListboxSelect>>", checkIndexColno)
  tkbind(faclevListBox, "<<ListboxSelect>>", checkIndexFlev)
  tkbind(faclabListBox, "<<ListboxSelect>>", checkIndexLab)


  ### funktioniert, ist aber noch nicht schön
  scrollbar <- ttkscrollbar(listFrame, command = function(...) {
    tkyview(facshortListBox, ...)
    tkyview(facnameListBox, ...)
    tkyview(nlevListBox, ...)
    tkyview(colnoListBox, ...)
    tkyview(faclevListBox, ...)
    tkyview(faclabListBox, ...)
  })

  #    tkgrid(tklabel(enterlistFrame,text="  ", width=5),enterFrame, sticky="w")
  tkgrid(enterFrame, sticky="w", columnspan=5)
  tkgrid.configure(enterFrame, pady=10)
  ## Hoch-/Runterschieben von Einträgen ermöglichen

  downupFrame <- ttkframe(listFrame)
  moveDownButton <- buttonRcmdr(downupFrame, text = gettextRcmdr("Move Down"),
                                foreground = "darkgreen", command = onDown,
                                default = "normal", borderwidth = 3, width=12)
  moveUpButton <- buttonRcmdr(downupFrame, text = gettextRcmdr("Move Up"),
                              foreground = "darkgreen", command = onUp,
                              default = "normal", borderwidth = 3, width=12)
  tkgrid(moveDownButton, sticky="w")
  tkgrid(moveUpButton, sticky="w")

  if (idpos>1 & tclvalue(colnospecifyVariable)=="1")
    tkgrid(scrollbar, facshortListBox, facnameListBox, colnoListBox, nlevListBox, faclevListBox, faclabListBox, downupFrame, sticky = "nw") else
      tkgrid(scrollbar, facshortListBox, facnameListBox, nlevListBox, faclevListBox, faclabListBox, downupFrame, sticky = "nw")
  tkgrid.configure(scrollbar, sticky = "wns")
  tkgrid.configure(facnameListBox, sticky = "new")

  helptab2Button <- buttonRcmdr(tab2, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab2,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab2Button, takefocus=0)

  ## finalize tab2 Factor details
  tkgrid(helptab2Button, sticky="e")
  tkgrid(listFrame, columnspan=6,sticky="w")
  tkgrid(enterlistFrame, columnspan=6,sticky="w")

  ## tab6 for exporting
  helptab6Button <- buttonRcmdr(tab6, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab6,
                                default = "normal", borderwidth = 3)

  exportlabVar <- nameVar
  exportlab <- ttklabel(tab6, textvariable=exportlabVar)
  tkgrid(ttklabel(tab6,text="Current design to be saved:"),exportlab,helptab6Button,sticky="w")
  tkgrid.configure(exportlab, pady=15)
  tkgrid.configure(helptab6Button, sticky="ne")

  ## radio buttons for choosing export type
  etradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("(How to) Export ?"))
  etyperbVariable <- tclVar(.stored.designoa$etyperbVariable)
  noexprb <- tkradiobutton(etradioFrame,text=gettextRcmdr("no export"),variable=etyperbVariable,value="none")
  allrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("all file types"),variable=etyperbVariable,value="all")
  rdarb <- tkradiobutton(etradioFrame,text=gettextRcmdr("rda only"),variable=etyperbVariable,value="rda")
  htmlrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("html and rda"),variable=etyperbVariable,value="html")
  csvrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("csv and rda"),variable=etyperbVariable,value="csv")
  tkgrid(noexprb, sticky="w")
  tkgrid(allrb, sticky="w")
  tkgrid(rdarb, sticky="w")
  tkgrid(htmlrb, sticky="w")
  tkgrid(csvrb, sticky="w")

  ## radio buttons for choosing export decimal separator
  decimalradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("Decimal Separator ?"))
  decimalrbVariable <- tclVar(.stored.designoa$decimalrbVariable)
  defaultrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("default"),variable=decimalrbVariable, value="default")
  pointrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("."),variable=decimalrbVariable, value=".")
  commarb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr(","),variable=decimalrbVariable, value=",")
  tkgrid(defaultrb, sticky="w")  ## in this case, leave default option from options
  tkgrid(pointrb, sticky="w")
  tkgrid(commarb, sticky="w")

  ## export directory
  dirFrame <- ttklabelframe(tab6, text=gettextRcmdr("Storage Directory"))
  putRcmdr("dirVar", tclVar(.stored.designoa$dirVar))
  dirEntry <- tkentry(dirFrame, width="50", textvariable=dirVar)
  dirButton <- buttonRcmdr(dirFrame, text = gettextRcmdr("Change directory"),
                           foreground = "darkgreen", width = "20", command = onChangeDir,
                           default = "normal", borderwidth = 3)
  tkgrid(dirEntry, tklabel(dirFrame, text="   "), dirButton, sticky="w")

  ## export file name
  putRcmdr("fileVar", tclVar(.stored.designoa$fileVar))
  fileEntry <- tkentry(tab6, width="20", textvariable=fileVar)
  efnamelabel <- tklabel(tab6,text=gettextRcmdr("Export file names: name below with appropriate endings (html or csv, and rda)"))
  replacecbVariable <- tclVar(.stored.designoa$cbInitials[8])
  replacecb <- ttkcheckbutton(tab6,text=gettextRcmdr("Replace file(s), if exists"),variable=replacecbVariable)

  ## always grid details, as otherwise default file name does not work
  ## design name info and help button have already been gridded above
  tkgrid(etradioFrame, decimalradioFrame, sticky="nw")
  tkgrid(dirFrame, sticky="w", columnspan=5)
  tkgrid.configure(dirFrame, pady=15)
  tkgrid(efnamelabel, sticky="w", columnspan=5)
  tkgrid(fileEntry, sticky="w", columnspan=5)
  tkgrid(replacecb, sticky="w", columnspan=5)


  ## add buttons outside the notebook
  buttonFrame <- tkframe(topdes2)
  ## die sind aber nicht dunkelgruen ...
  refreshButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Refresh form"),
                               foreground = "darkgreen", width = "12", command = onRefresh,
                               default = "normal", borderwidth = 3)
  storeButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Store form"),
                             foreground = "darkgreen", width = "12", command = onStore,
                             default = "normal", borderwidth = 3)
  loadButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Load form"),
                            foreground = "darkgreen", width = "12", command = onLoad,
                            default = "normal", borderwidth = 3)
  resetButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Reset form"),
                             foreground = "darkgreen", width = "12", command = onReset,
                             default = "normal", borderwidth = 3)
  #        tkgrid(refreshButton,sticky="w")
  #        tkgrid(tklabel(buttonFrame,text="  "),sticky="w")
  tkgrid(storeButton,sticky="w")
  tkgrid(loadButton,sticky="w")
  tkgrid(resetButton,sticky="w")

  tkconfigure(refreshButton, takefocus=0)
  tkconfigure(storeButton, takefocus=0)
  tkconfigure(loadButton, takefocus=0)
  tkconfigure(resetButton, takefocus=0)

  ## storage buttons to the right of the notebook
  tkgrid(tn, buttonFrame, sticky="w", columnspan=2)

  OKCancelHelp(window=topdes2, helpSubject="Menu.oa")
  tkconfigure(OKbutton, takefocus=0)
  tkconfigure(cancelButton, takefocus=0)
  tkconfigure(helpButton, takefocus=0)

  tkgrid(buttonsFrame, bottomFrame, sticky="ew")


  ### relations among widgets
  if (!as.logical(as.numeric(tclvalue(randomizeVariable)))){
    tkconfigure(seedEntry, state="disabled")
    tkconfigure(seedlab, state="disabled")
  }else {
    tkconfigure(seedEntry, state="normal")
    tkconfigure(seedlab, state="normal")
  }
  if (exists("activestab.tn", where="RcmdrEnv")){
    tcl(tn, "select", activestab.tn)
    rm(activestab.tn, pos="RcmdrEnv")
  }

  dialogSuffix(window=topdes2, rows=2, columns=2, focus=tn, bindReturn=FALSE)

}

Menu.ccd <- function(){
  putRcmdr("designs", listDesigns2(type="FrF2"))
  ## the next three rows should be omittable
  #    waehl <- unlist(sapply(designs, function(obj) length(grep("FrF2",design.info(get(obj))$type))>0))
  #    if (sum(waehl)) putRcmdr("designs", designs[waehl])
  #       else putRcmdr("designs", "")

  #angefangen
  #FrF2-Design Entry mit Knopf für Erzeugung über FrF2-Menü

  ## FrF2Var für zu verwendendes FrF2 (mit selection box designsBox auszuwählen)
  ## Falls es noch kein FrF2 gibt oder ein anderes verwendet werden soll
  ##         (radio buttons): interim Aufruf des FrF2 Menüs mit Button,
  ##         anschl. ist FrF2Var das neu erzeugte Design


  initializeDialogDoE(title=gettextRcmdr("Central composite design ..."))
  ## function initializeDialogDoE assumes topdes2 as windowname
  ## last stored top left corner for window is stored under topleft2xy
  ## onRefresh still makes window walk a little

  if (exists("curindex", where="RcmdrEnv")) rm(curindex, pos="RcmdrEnv")

  if (!exists(".stored.designccd",where="RcmdrEnv"))
    putRcmdr(".stored.designccd", .default.designccd)
  ## nameVar, nrunVar, nfacVar, nrepVar
  ## cbInitials containing repeat.onlyVariable, randomizeVariable,
  ##                       facnamesAutoVariable, faclevelsCommonVariable,
  ##                       nrunEntryVariable, estcbVariable
  ##                       specialcbVariable, replacecbVariable, MaxC2cbVariable
  ##                       res3cbVariable
  ## level1Var, level2Var, seedVar, specialrbVariable, hardVar, genVar,
  ## catlgVar, designVar, designrbVariable, destyperbVariable
  ## resVar, qualcritrbVariable, facnamlist,faclev1list,faclev2list, faclablist
  ## etyperbVariable, decimalrbVariable, dirVar, fileVar

  ## MaxC2cbVariable is free again (no. 9 of cbInitials)

  ## define called functions
  infoClose <- function(){
    putRcmdr("infotxt",tclVar(""))
  }

  onHelpTab1 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.ccdTab1"))
  }
  onHelpTab6 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.exportTab"))
  }

  tabpos <- function(){
    ### get 0-based index of currently selected tab
    activestab.tn <- tclvalue(tcl(tn, "select"))
    activestab.tn <- strsplit(activestab.tn,".",fixed=TRUE)[[1]]
    activestab.tn <- as.numeric(activestab.tn[length(activestab.tn)])-1
    activestab.tn
  }

  storeRcmdr <- function(){
    hilf <- list(nameVar=tclvalue(nameVar),
                 ncenterVar=tclvalue(ncenterVar),
                 alphaVar=tclvalue(alphaVar),
                 cbInitials = c("0", tclvalue(randomizecbVariable),
                                "0",
                                1,0,
                                0,tclvalue(replacecbVariable),0,
                                0
                 ),
                 seedVar=tclvalue(seedVar),
                 etyperbVariable=tclvalue(etyperbVariable),
                 decimalrbVariable=tclvalue(decimalrbVariable),
                 dirVar=tclvalue(dirVar), fileVar=tclvalue(fileVar))
    class(hilf) <- c("menu.designccd","list")
    putRcmdr(".stored.designccd",hilf)
  }

  onOK <- function(){
    onRefreshEnd()
    ## store entries so that users do not have to redo everything
    ## in case of stupid mistakes
    storeRcmdr()
    putRcmdr("FrF2Var",getSelection(designsBox))
    ## seed is not used from previously stored design
    closeDialog(window=topdes2)
    name <- tclvalue(nameVar)
    if (!is.valid.name(name)){
      errorCondition(window=topdes2,recall=Menu.ccd,
                     message=paste('"', name, '" ', gettextRcmdr("is not a valid name."), sep=""))
      return()
    }
    if (is.element(name, listObjects()))
    {
      if ("no" == tclvalue(checkReplace(name, gettextRcmdr("Object"))))
      {
        errorCondition(window=topdes2,recall=Menu.ccd,
                       gettextRcmdr("Introduce another name for the new data.frame, or allow replacing."))
        return()
      }
    }
    ###  further error messages with return to menu ?

    ### not yet perfect, especially NULL entries are not possible
    ### for didactic reasons distinguish between usage of default.levels and other?

    if (tclvalue(alphaVar) %in% c("orthogonal","rotatable"))
      command <- paste("ccd.augment(",FrF2Var,
                       ", alpha=",dquote(tclvalue(alphaVar)), ", ncenter=c(", tclvalue(ncenterVar),
                       ") ,randomize=",as.logical(as.numeric(tclvalue(randomizecbVariable))),
                       ",seed=",tclvalue(seedVar),")")
    else command <- paste("ccd.augment(",FrF2Var,
                          ", alpha=",dquote(tclvalue(alphaVar)), ", ncenter=c(", tclvalue(ncenterVar),
                          ") ,randomize=",as.logical(as.numeric(tclvalue(randomizecbVariable))),
                          ",seed=",tclvalue(seedVar),")")

    hilf <- justDoItDoE(command)
    if (class(hilf)[1]=="try-error") {
      Message(paste(gettextRcmdr("Offending command:"), "\n", command), type="error")
      errorCondition(window=topdes2,recall=Menu.ccd, message=gettextRcmdr(hilf))
      return()
    }

    logger(paste(name, "<-", command))
    logger("## creator element of design.info will be different, when using the command line command!")
    ## change creator to contain menu settings
    hilfatt <- design.info(hilf)
    hilfatt$creator <- .stored.designccd
    class(hilfatt$creator) <- c("menu.designccd", "list")
    attr(hilf, "design.info") <- hilfatt
    putRcmdr("hilf", hilf)
    ## replace assign by justDoIt; assign(name, hilf, envir=.GlobalEnv)
    justDoIt(paste(name, "<- getRcmdr(\"hilf\")"))
    rm("hilf", pos="RcmdrEnv")
    activeDataSet(name)
    ### exporting
    if (!tclvalue(etyperbVariable)=="none"){
      putRcmdr("path", tclvalue(dirVar))
      putRcmdr("filename", tclvalue(fileVar))
      if (!as.logical(as.numeric(tclvalue(replacecbVariable)))){
        lf <- tolower(list.files(path = path))
        if (tolower(paste(filename, "rda", sep = ".")) %in% lf)
          stop("file ", paste(filename, "rda", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="html" & tolower(paste(filename, "html", sep = ".")) %in% lf)
          stop("file ", paste(filename, "html", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="csv" & tolower(paste(filename, "csv", sep = ".")) %in% lf)
          stop("file ", paste(filename, "csv", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
      }
      if (tclvalue(decimalrbVariable)=="default") command <- paste("export.design(",name,
                                                                   ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                                                                   as.logical(as.numeric(tclvalue(replacecbVariable))),")",sep="")
      else command <- paste("export.design(",name,
                            ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                            as.logical(as.numeric(tclvalue(replacecbVariable))),", OutDec=", dquote(tclvalue(decimalrbVariable)),")",sep="")
      hilf <- justDoItDoE(command)
      if (class(hilf)[1]=="try-error") {
        errorCondition(window=topdes2,recall=Menu.ccd, message=gettextRcmdr(hilf))
        return()
      }
      logger(command)
    }
    rm(activestab.tn, pos="RcmdrEnv")
    tkwm.deiconify(CommanderWindow())
    tkfocus(CommanderWindow())
  }


  onLoad <- function(){
    ## seems to work now, needs to be tested!
    hilf <- listDesignlhs()
    if (length(hilf)==0) {
      tkmessageBox(message=gettextRcmdr("There are no stored design inputs in this session."),
                   icon="error", type="ok", title="no stored design inputs")
      return()
    }
    putRcmdr("deschoose2",tktoplevel())
    tkwm.title(deschoose2, gettextRcmdr("Choose stored design form"))
    position <- if (is.SciViews())
      -1
    else position <- "+50+50"
    tkwm.geometry(deschoose2, position)
    putRcmdr("lb", variableListBox(deschoose2, variableList=hilf, title="Choose stored design form"))
    tkgrid(lb$frame)
    onOK <- function() {
      putRcmdr(".stored.designccd",get(lb$varlist[as.numeric(tclvalue(tcl(lb$listbox, "curselection")))+1]))
      if ("design" %in% class(getRcmdr(".stored.designccd")))
        putRcmdr(".stored.designccd", design.info(getRcmdr(".stored.designccd"))$creator)
      tkfocus(CommanderWindow())
      tkdestroy(topdes2)
      tkdestroy(deschoose2)
      Menu.ccd()
    }
    OKCancelHelp(window=deschoose2)
    tkgrid(buttonsFrame, sticky="s")
    dialogSuffix(window=deschoose2, rows=1, columns=1,
                 focus=lb$listbox)
  }

  onRefreshEnd <- function(){
    storeRcmdr()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    putRcmdr("activestab.tn",tabpos())
    ID <- topdes2$ID
    putRcmdr("topleft2xy",as.numeric(c(tclvalue(.Tcl(paste("winfo rootx", ID))),
                                       tclvalue(.Tcl(paste("winfo rooty", ID))))))
    #        assign("activestab.tn",strsplit(activestab.tn,".",fixed=TRUE)[[1]],pos="RcmdrEnv")
    #        assign("activestab.tn",as.numeric(activestab.tn[length(activestab.tn)])-1,pos="RcmdrEnv")
  }

  onRefresh <- function(){
    #print(as.character(tclObj(tcl(tn, "select"))))
    onRefreshEnd()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.ccd()
  }

  onStore <- function(){
    ## Speichernamen abfragen und hier ermöglichen (statt stored.designccd)
    textentry() ## creates text string stored in savename.RcmdrPlugin.DoE
    if (!is.null(savename.RcmdrPlugin.DoE)){
      if (!is.valid.name(savename.RcmdrPlugin.DoE)){
        textcorrect(gettextRcmdr("This is not a valid name. Please correct:"))
        return()
      }
      if (is.element(savename.RcmdrPlugin.DoE, listObjects()))
      {
        if ("no" == tclvalue(checkReplace(savename.RcmdrPlugin.DoE, gettextRcmdr("Object"))))
        {
          textcorrect(gettextRcmdr("Please enter a new name:"))
          return()
        }
      }
      storeRcmdr()
      ## replace assign by justDoIt; assign(savename.RcmdrPlugin.DoE, getRcmdr(".stored.designccd"), envir=.GlobalEnv)
      justDoIt(paste(savename.RcmdrPlugin.DoE, "<- getRcmdr(\".stored.designccd\")"))
      message(gettextRcmdr("inputs have been stored"))
    }
  }

  onReset <- function(){
    assign(".stored.designccd",.default.designccd,pos="RcmdrEnv")
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.ccd()
  }

  nameenter <- function(){
    if (identical(tclvalue(getRcmdr("fileVar")),tclvalue(getRcmdr("nameVar"))))
      putRcmdr("name.equal.filename", TRUE)
    else putRcmdr("name.equal.filename", FALSE)
  }
  namechange <- function(){
    if (is.valid.name(tclvalue(nameVar))){
      if (name.equal.filename){
        putRcmdr("fileVar", tclVar(tclvalue(nameVar)))  ## otherwise, variables would be directly tied
        #          putRcmdr("exportlabVar", tclVar(paste("Current design to be saved:", tclvalue(nameVar),"\n   ")))
        tkconfigure(fileEntry, textvariable=getRcmdr("fileVar"))
        #          tkconfigure(exportlab, textvariable=getRcmdr("exportlabVar"))
      }
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid design name")
  }


  onChangeDir <- function(){
    putRcmdr("direct",tclvalue(tkchooseDirectory()))
    if (!direct=="") {
      putRcmdr("dirVar", tclVar(direct))
      tkconfigure(dirEntry, textvariable = dirVar)
    }
  }

  newFrF2 <- function(){
    storeRcmdr()

    if (!exists(".stored.design2FrF", where="RcmdrEnv"))
      hilf <- .default.design2
    else hilf <- getRcmdr(".stored.design2FrF")
    hilf$nameVar <- getRcmdr(".stored.designccd")$nameVar
    hilf$cbInitials[5] <- 0
    hilf$cbInitials[6] <- 0
    hilf$cbInitials[7] <- 0
    hilf$resVar <- "V+"
    hilf2 <- options("warn")
    options(warn=-1)
    ## make all levels numeric
    ## nonnum are not coercible to numeric, default levels are not coercible to numeric
    nonnum <- union(which(sapply(hilf$faclev1list, function(obj) is.na(as.numeric(obj)))),
                    which(sapply(hilf$faclev2list, function(obj) is.na(as.numeric(obj)))))
    hilf$faclev1list[nonnum] <- "-1"
    hilf$faclev2list[nonnum] <- "1"
    if (any(is.na(as.numeric(hilf$level1Var)),is.na(as.numeric(hilf$level2Var)))){
      hilf$level1Var <- "-1"
      hilf$level2Var <- "1"
    }
    options(warn=hilf2$warn)
    rm(hilf2)

    assign(".stored.design2FrF", hilf,pos="RcmdrEnv")
    closeDialog(window=topdes2)
    Menu.FrF2level()
    tkmessageBox(message="Continue defining the star portion of the design.",
                 icon="info", type="ok", title="Cube portion was created")
    Menu.ccd()
  }

  dquote <- function(obj){
    ## quote vector elements for use as character vector in a command
    aus <- rep("",length(obj))
    wopt <- options("warn")[[1]]
    options(warn=-1)
    for (i in 1:length(obj)) if (is.na(as.numeric(obj[i]))) {
      if (length(grep('"',obj[i])>0))
        aus[i] <- paste("'",obj[i],"'",sep="")
      else
        aus[i] <- paste('"',obj[i],'"',sep="")
    }
    else aus[i] <- obj[i]
    options(warn=wopt)
    aus
  }

  ######## end define functions


  ##### define userform
  #tn <- ttknotebook(top,height=100, width=500)


  putRcmdr("tn",ttknotebook(topdes2))
  #tn <- ttknotebook(topdes2)

  putRcmdr("tab1",ttkframe(tn))
  putRcmdr("tab6",ttkframe(tn))## called 6 because of parallel treatment with
  ## fractional factorial menu

  tkadd(tn,tab1,text="Base Settings")   ### tabid=0
  tkadd(tn,tab6,text="Export") ### tabid=5

  tkconfigure(tn, takefocus=0)

  nameFrame <- ttkframe(tab1)

  #typeradioFrame <- ttklabelframe(tab1, text=gettextRcmdr("Augment or from scratch ?"))
  #typerbVariable <- tclVar(.stored.designccd$typerbVariable)
  #augmentrb <- tkradiobutton(typeradioFrame,text=gettextRcmdr("add star portion to existing design"),variable=typerbVariable, value="augment")
  #fromscratchrb <- tkradiobutton(typeradioFrame,text=gettextRcmdr("create new central composite design"),variable=typerbVariable, value="fromscratch")
  #tkgrid(augmentrb, sticky="w")  ## in this case, leave default option from options
  #tkgrid(fromscratchrb, sticky="w")

  designFrame <- ttklabelframe(tab1, text=gettextRcmdr("Determine the cube portion"))
  designsBox <- variableListBox(parentWindow=designFrame, designs, title=gettextRcmdr("Pick existing 2-level design"),
                                initialSelection=if (is.null(getRcmdr(".activeDataSet")) || !getRcmdr(".activeDataSet") %in% designs) NULL
                                else which(getRcmdr(".activeDataSet") == designs) - 1)
  designButton <- tkbutton(designFrame, text="Create new 2-level design", command=newFrF2)
  tkgrid(getFrame(designsBox), ttklabel(designFrame, text=" OR "), designButton, sticky="w", padx=10)
  tkconfigure(designButton, takefocus=0)


  baseFrame <- ttklabelframe(tab1,text=gettextRcmdr("Details for star portion"))

  ### widgets for tab1 and base frame
  putRcmdr("nameVar", tclVar(.stored.designccd$nameVar))
  nameEntry <- tkentry(nameFrame, width="20", textvariable=nameVar)
  tkbind(nameEntry, "<FocusIn>", nameenter)
  tkbind(nameEntry, "<FocusOut>", namechange)

  ncenterVar <- tclVar(.stored.designccd$ncenterVar)
  ncenterEntry <- tkentry(baseFrame, width="8", textvariable=ncenterVar)
  ncenterHint <- ttklabel(baseFrame, text="(positive integer number, \npossibly preceded by separate positive integer number for cube portion)", foreground="#888888")

  alphaVar <- tclVar(.stored.designccd$alphaVar)
  alphaEntry <- tkentry(baseFrame, width="12", textvariable=alphaVar)
  alphaHint <- ttklabel(baseFrame, text="(orthogonal, rotatable, or positive number)", foreground="#888888")

  randomizecbVariable <- tclVar(.stored.designccd$cbInitials[2])
  randomizecb <- ttkcheckbutton(baseFrame,text=gettextRcmdr("Randomize design"),variable=randomizecbVariable)
  tkconfigure(randomizecb, takefocus=0)
  seedVar <- tclVar(sample(31999,1))  ## always new
  seedEntry <- tkentry(baseFrame, width="8", textvariable=seedVar)
  tkconfigure(seedEntry, takefocus=0)


  ## preparations for bottom frame
  bottomFrame <- tkframe(topdes2)

  ## grid base frame
  tkgrid(ncenterlab <- tklabel(baseFrame, text=gettextRcmdr("Number of center points for star portion")), ncenterEntry, ncenterHint, sticky="w")
  tkgrid(alphalab <- tklabel(baseFrame, text=gettextRcmdr("Positioning of star points (alpha)")), alphaEntry, alphaHint, sticky="w", pady=15)
  tkgrid(randlab <- tklabel(baseFrame, text="You normally do not need to change randomization settings"),sticky="w",columnspan=3)
  tkgrid(seedlab <- tklabel(baseFrame, text=gettextRcmdr("Seed for randomization")), seedEntry,
         randomizecb, sticky="w")

  helptab1Button <- buttonRcmdr(nameFrame, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab1,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab1Button, takefocus=0)

  ### Finalize tab1
  tkgrid(tklabel(nameFrame, text="Name of new design"), nameEntry, helptab1Button, sticky="w")
  tkgrid(nameFrame, sticky="w", columnspan=4, pady=20)
  tkgrid.configure(helptab1Button, sticky="ne", padx=15)
  #tkgrid(typeradioFrame, sticky="w", columnspan=4)

  tkgrid(designFrame, sticky="e",pady=15, columnspan=4)

  tkgrid(baseFrame, sticky="nw",columnspan=3)

  ## tab6 for exporting
  helptab6Button <- buttonRcmdr(tab6, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab6,
                                default = "normal", borderwidth = 3)

  exportlabVar <- nameVar
  exportlab <- ttklabel(tab6, textvariable=exportlabVar)
  tkgrid(ttklabel(tab6,text="Current design to be saved:"),exportlab,helptab6Button,sticky="w")
  tkgrid.configure(exportlab, pady=15)
  tkgrid.configure(helptab6Button, sticky="ne")

  ## radio buttons for choosing export type
  etradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("(How to) Export ?"))
  etyperbVariable <- tclVar(.stored.designccd$etyperbVariable)
  noexprb <- tkradiobutton(etradioFrame,text=gettextRcmdr("no export"),variable=etyperbVariable,value="none")
  allrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("all file types"),variable=etyperbVariable,value="all")
  rdarb <- tkradiobutton(etradioFrame,text=gettextRcmdr("rda only"),variable=etyperbVariable,value="rda")
  htmlrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("html and rda"),variable=etyperbVariable,value="html")
  csvrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("csv and rda"),variable=etyperbVariable,value="csv")
  tkgrid(noexprb, sticky="w")
  tkgrid(allrb, sticky="w")
  tkgrid(rdarb, sticky="w")
  tkgrid(htmlrb, sticky="w")
  tkgrid(csvrb, sticky="w")

  ## radio buttons for choosing export decimal separator
  decimalradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("Decimal Separator ?"))
  decimalrbVariable <- tclVar(.stored.designccd$decimalrbVariable)
  defaultrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("default"),variable=decimalrbVariable, value="default")
  pointrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("."),variable=decimalrbVariable, value=".")
  commarb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr(","),variable=decimalrbVariable, value=",")
  tkgrid(defaultrb, sticky="w")  ## in this case, leave default option from options
  tkgrid(pointrb, sticky="w")
  tkgrid(commarb, sticky="w")

  ## export directory
  dirFrame <- ttklabelframe(tab6, text=gettextRcmdr("Storage Directory"))
  putRcmdr("dirVar", tclVar(.stored.designccd$dirVar))
  dirEntry <- tkentry(dirFrame, width="50", textvariable=dirVar)
  dirButton <- buttonRcmdr(dirFrame, text = gettextRcmdr("Change directory"),
                           foreground = "darkgreen", width = "20", command = onChangeDir,
                           default = "normal", borderwidth = 3)
  tkgrid(dirEntry, tklabel(dirFrame, text="   "), dirButton, sticky="w")

  ## export file name
  putRcmdr("fileVar", tclVar(.stored.designccd$fileVar))
  fileEntry <- tkentry(tab6, width="20", textvariable=fileVar)
  efnamelabel <- tklabel(tab6,text=gettextRcmdr("Export file names: name below with appropriate endings (html or csv, and rda)"))
  replacecbVariable <- tclVar(.stored.designccd$cbInitials[8])
  replacecb <- ttkcheckbutton(tab6,text=gettextRcmdr("Replace file(s), if exists"),variable=replacecbVariable)

  ## always grid details, as otherwise default file name does not work
  ## design name info and help button have already been gridded above
  tkgrid(etradioFrame, decimalradioFrame, sticky="nw")
  tkgrid(dirFrame, sticky="w", columnspan=5)
  tkgrid.configure(dirFrame, pady=15)
  tkgrid(efnamelabel, sticky="w", columnspan=5)
  tkgrid(fileEntry, sticky="w", columnspan=5)
  tkgrid(replacecb, sticky="w", columnspan=5)


  ## add buttons outside the notebook
  buttonFrame <- tkframe(topdes2)
  ## die sind aber nicht dunkelgruen ...
  refreshButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Refresh form"),
                               foreground = "darkgreen", width = "12", command = onRefresh,
                               default = "normal", borderwidth = 3)
  storeButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Store form"),
                             foreground = "darkgreen", width = "12", command = onStore,
                             default = "normal", borderwidth = 3)
  loadButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Load form"),
                            foreground = "darkgreen", width = "12", command = onLoad,
                            default = "normal", borderwidth = 3)
  resetButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Reset form"),
                             foreground = "darkgreen", width = "12", command = onReset,
                             default = "normal", borderwidth = 3)
  #        tkgrid(refreshButton,sticky="w")
  #        tkgrid(tklabel(buttonFrame,text="  "),sticky="w")
  tkgrid(storeButton,sticky="w")
  tkgrid(loadButton,sticky="w")
  tkgrid(resetButton,sticky="w")

  tkconfigure(refreshButton, takefocus=0)
  tkconfigure(storeButton, takefocus=0)
  tkconfigure(loadButton, takefocus=0)
  tkconfigure(resetButton, takefocus=0)

  ## storage buttons to the right of the notebook
  tkgrid(tn, buttonFrame, sticky="w", columnspan=2)

  OKCancelHelp(window=topdes2, helpSubject="Menu.ccd")
  tkconfigure(OKbutton, takefocus=0)
  tkconfigure(cancelButton, takefocus=0)
  tkconfigure(helpButton, takefocus=0)

  tkgrid(buttonsFrame, bottomFrame, sticky="ew")

  ### relations among widgets
  if (exists("activestab.tn", where="RcmdrEnv")){
    tcl(tn, "select", activestab.tn)
    rm(activestab.tn, pos="RcmdrEnv")
  }

  dialogSuffix(window=topdes2, rows=2, columns=2, focus=tn, bindReturn=FALSE)

}

Menu.bbd <- function(){
  initializeDialogDoE(title=gettextRcmdr("Create Box-Behnken design ..."))
  ## function initializeDialogDoE assumes topdes2 as windowname
  ## last stored top left corner for window is stored under topleft2xy
  ## onRefresh still makes window walk a little

  if (exists("curindex", where="RcmdrEnv")) rm(curindex, pos="RcmdrEnv")

  if (!exists(".stored.designbbd",where="RcmdrEnv"))
    assign(".stored.designbbd", .default.designbbd,pos="RcmdrEnv")
  ## nameVar, nrunVar, nfacVar, nrepVar
  ## cbInitials containing repeat.onlyVariable, randomizeVariable,
  ##                       facnamesAutoVariable, faclevelsCommonVariable,
  ##                       nrunEntryVariable, estcbVariable
  ##                       specialcbVariable, replacecbVariable, MaxC2cbVariable
  ##                       res3cbVariable
  ## level1Var, level2Var, seedVar, specialrbVariable, hardVar, genVar,
  ## catlgVar, designVar, designrbVariable, destyperbVariable
  ## resVar, qualcritrbVariable, facnamlist,faclev1list,faclev2list, faclablist
  ## etyperbVariable, decimalrbVariable, dirVar, fileVar

  ## MaxC2cbVariable is free again (no. 9 of cbInitials)

  ## define called functions
  infoClose <- function(){
    putRcmdr("infotxt",tclVar(""))
  }

  onHelpTab1 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.bbdTab1"))
  }
  onHelpTab2 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.FacDetails2Tab"))
  }
  onHelpTab6 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.exportTab"))
  }

  tabpos <- function(){
    ### get 0-based index of currently selected tab
    activestab.tn <- tclvalue(tcl(tn, "select"))
    activestab.tn <- strsplit(activestab.tn,".",fixed=TRUE)[[1]]
    activestab.tn <- as.numeric(activestab.tn[length(activestab.tn)])-1
    activestab.tn
  }

  storeRcmdr <- function(){
    hilf <- list(nameVar=tclvalue(nameVar),
                 nfacVar=tclvalue(nfacVar),
                 ncenterVar=tclvalue(ncenterVar),blockVar=tclvalue(blockVar),
                 cbInitials = c("0", tclvalue(randomizeVariable),
                                tclvalue(facnameAutoVariable),tclvalue(faclevelCommonVariable),
                                1,0,
                                0,tclvalue(replacecbVariable),0,
                                0
                 ),
                 level1Var=tclvalue(level1Var),level2Var=tclvalue(level2Var),seedVar=tclvalue(seedVar),
                 facnamlist=as.character(tclObj(facnamlist)),
                 faclev1list=as.character(tclObj(faclev1list)),
                 faclev2list=as.character(tclObj(faclev2list)),
                 faclablist=as.character(tclObj(faclablist)),
                 etyperbVariable=tclvalue(etyperbVariable),
                 decimalrbVariable=tclvalue(decimalrbVariable),
                 dirVar=tclvalue(dirVar), fileVar=tclvalue(fileVar))
    class(hilf) <- c("menu.designbbd","list")
    putRcmdr(".stored.designbbd",hilf)
  }

  onOK <- function(){
    onRefreshEnd()
    ## store entries so that users do not have to redo everything
    ## in case of stupid mistakes
    storeRcmdr()
    ## seed is not used from previously stored design
    closeDialog(window=topdes2)
    name <- tclvalue(nameVar)
    if (!is.valid.name(name)){
      errorCondition(window=topdes2,recall=Menu.bbd,
                     message=paste('"', name, '" ', gettextRcmdr("is not a valid name."), sep=""))
      return()
    }
    if (is.element(name, listObjects()))
    {
      if ("no" == tclvalue(checkReplace(name, gettextRcmdr("Object"))))
      {
        errorCondition(window=topdes2,recall=Menu.bbd,
                       gettextRcmdr("Introduce another name for the new data.frame, or allow replacing."))
        return()
      }
    }
    ###  further error messages with return to menu ?

    textfactornameslist.forcommand <- paste("factor.names=list(",paste(paste(as.character(tclObj(facnamlist)),"=c(",
                                                                             dquote(as.character(tclObj(faclev1list))), ",",
                                                                             dquote(as.character(tclObj(faclev2list))), ")",sep=""),
                                                                       collapse=","),")")

    ### not yet perfect, especially NULL entries are not possible
    ### for didactic reasons distinguish between usage of default.levels and other?
    if (tclvalue(blockVar) %in% c("","NULL"))
      command <- paste("bbd.design(nfactors=",tclvalue(nfacVar),", ncenter=", tclvalue(ncenterVar), ", ",
                       ",randomize=",as.logical(as.numeric(tclvalue(randomizeVariable))),",seed=",tclvalue(seedVar),
                       ",",textfactornameslist.forcommand, ")")
    else
      command <- paste("bbd.design(nfactors=",tclvalue(nfacVar),", ncenter=", tclvalue(ncenterVar), ", ",
                       ",randomize=",as.logical(as.numeric(tclvalue(randomizeVariable))),",seed=",tclvalue(seedVar),
                       ",",textfactornameslist.forcommand,", block.name=",dquote(tclvalue(blockVar)),")")

    hilf <- justDoItDoE(command)
    if (class(hilf)[1]=="try-error") {
      Message(paste(gettextRcmdr("Offending command:"), "\n", command), type="error")
      errorCondition(window=topdes2,recall=Menu.bbd, message=gettextRcmdr(hilf))
      return()
    }

    logger(paste(name, "<-", command))
    logger("## creator element of design.info will be different, when using the command line command!")
    ## change creator to contain menu settings
    hilfatt <- design.info(hilf)
    hilfatt$creator <- .stored.designbbd
    class(hilfatt$creator) <- c("menu.designbbd", "list")
    attr(hilf, "design.info") <- hilfatt
    putRcmdr("hilf", hilf)
    ## replace assign by justDoIt; assign(name, hilf, envir=.GlobalEnv)
    justDoIt(paste(name, "<- getRcmdr(\"hilf\")"))
    rm("hilf", pos="RcmdrEnv")
    activeDataSet(name)
    ### exporting
    if (!tclvalue(etyperbVariable)=="none"){
      putRcmdr("path", tclvalue(dirVar))
      putRcmdr("filename", tclvalue(fileVar))
      if (!as.logical(as.numeric(tclvalue(replacecbVariable)))){
        lf <- tolower(list.files(path = path))
        if (tolower(paste(filename, "rda", sep = ".")) %in% lf)
          stop("file ", paste(filename, "rda", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="html" & tolower(paste(filename, "html", sep = ".")) %in% lf)
          stop("file ", paste(filename, "html", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="csv" & tolower(paste(filename, "csv", sep = ".")) %in% lf)
          stop("file ", paste(filename, "csv", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
      }
      if (tclvalue(decimalrbVariable)=="default") command <- paste("export.design(",name,
                                                                   ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                                                                   as.logical(as.numeric(tclvalue(replacecbVariable))),")",sep="")
      else command <- paste("export.design(",name,
                            ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                            as.logical(as.numeric(tclvalue(replacecbVariable))),", OutDec=", dquote(tclvalue(decimalrbVariable)),")",sep="")
      hilf <- justDoItDoE(command)
      if (class(hilf)[1]=="try-error") {
        errorCondition(window=topdes2,recall=Menu.bbd, message=gettextRcmdr(hilf))
        return()
      }
      logger(command)
    }
    rm(activestab.tn, pos="RcmdrEnv")
    tkwm.deiconify(CommanderWindow())
    tkfocus(CommanderWindow())
  }

  listDesign2 <- function (envir = .GlobalEnv, ...)
  {
    Vars <- ls(envir = envir, all.names = TRUE)
    Vars[which(sapply(Vars, function(.x){
      aus <- FALSE
      if ("menu.designbbd" %in% class(get(.x, envir = envir))) aus <- TRUE
      else if ("design" %in% class(get(.x, envir = envir)))
        if ("menu.designbbd" %in% class(design.info(get(.x, envir = envir))$creator))
          aus <- TRUE
        aus
    }))]
  }


  onLoad <- function(){
    ## seems to work now, needs to be tested!
    hilf <- listDesign2()
    if (length(hilf)==0) {
      tkmessageBox(message=gettextRcmdr("There are no stored design inputs in this session."),
                   icon="error", type="ok", title="no stored design inputs")
      return()
    }
    putRcmdr("deschoose2",tktoplevel())
    tkwm.title(deschoose2, gettextRcmdr("Choose stored design form"))
    position <- if (is.SciViews())
      -1
    else position <- "+50+50"
    tkwm.geometry(deschoose2, position)
    putRcmdr("lb", variableListBox(deschoose2, variableList=hilf, title="Choose stored design form"))
    tkgrid(lb$frame)
    onOK <- function() {
      putRcmdr(".stored.designbbd",get(lb$varlist[as.numeric(tclvalue(tcl(lb$listbox, "curselection")))+1]))
      if ("design" %in% class(getRcmdr(".stored.designbbd")))
        putRcmdr(".stored.designbbd", design.info(getRcmdr(".stored.designbbd"))$creator)
      tkfocus(CommanderWindow())
      tkdestroy(topdes2)
      tkdestroy(deschoose2)
      Menu.bbd()
    }
    OKCancelHelp(window=deschoose2)
    tkgrid(buttonsFrame, sticky="s")
    dialogSuffix(window=deschoose2, rows=1, columns=1,
                 focus=lb$listbox)
  }

  onRefreshEnd <- function(){
    nfacchange()
    storeRcmdr()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    putRcmdr("activestab.tn",tabpos())
    ID <- topdes2$ID
    putRcmdr("topleft2xy",as.numeric(c(tclvalue(.Tcl(paste("winfo rootx", ID))),
                                       tclvalue(.Tcl(paste("winfo rooty", ID))))))
    #        assign("activestab.tn",strsplit(activestab.tn,".",fixed=TRUE)[[1]],pos="RcmdrEnv")
    #        assign("activestab.tn",as.numeric(activestab.tn[length(activestab.tn)])-1,pos="RcmdrEnv")
  }

  onRefresh <- function(){
    #print(as.character(tclObj(tcl(tn, "select"))))
    onRefreshEnd()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.bbd()
  }

  onStore <- function(){
    ## Speichernamen abfragen und hier ermöglichen (statt stored.designbbd)
    textentry() ## creates text string stored in savename.RcmdrPlugin.DoE
    if (!is.null(savename.RcmdrPlugin.DoE)){
      if (!is.valid.name(savename.RcmdrPlugin.DoE)){
        textcorrect(gettextRcmdr("This is not a valid name. Please correct:"))
        return()
      }
      if (is.element(savename.RcmdrPlugin.DoE, listObjects()))
      {
        if ("no" == tclvalue(checkReplace(savename.RcmdrPlugin.DoE, gettextRcmdr("Object"))))
        {
          textcorrect(gettextRcmdr("Please enter a new name:"))
          return()
        }
      }
      storeRcmdr()
      ## replace assign by justDoIt; assign(savename.RcmdrPlugin.DoE, getRcmdr(".stored.designbbd"), envir=.GlobalEnv)
      justDoIt(paste(savename.RcmdrPlugin.DoE, "<- getRcmdr(\".stored.designbbd\")"))
      message(gettextRcmdr("inputs have been stored"))
    }
  }

  onReset <- function(){
    assign(".stored.designbbd",.default.designbbd,pos="RcmdrEnv")
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.bbd()
  }

  nfacchange <- function(){
    nfacold <- length(as.character(tclObj(varlistshort)))
    nfacnew <- as.numeric(tclvalue(nfacVar))
    if (nfacnew %in% c(4,5)) tkconfigure(blockEntry, state="normal")
    else (tkconfigure(blockEntry, state="disabled"))
    if (nfacold==nfacnew) return()
    if (nfacnew < nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[1:nfacnew]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[1:nfacnew]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[1:nfacnew]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[1:nfacnew]))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(faclev1ListBox, listvariable=faclev1list, height=min(10,nfacnew))
      tkconfigure(faclev2ListBox, listvariable=faclev2list, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      if (selpos > nfacnew){
        tcl(fsel, "current", "0")
        factorsel()
      }
    }
    if (nfacnew > nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist", tclVar(c(as.character(tclObj(facnamlist)),
                                      getRcmdr("varlistshortt")[(nfacold+1):nfacnew])) )
      putRcmdr("faclev1list", tclVar(c(as.character(tclObj(faclev1list)),
                                       rep(tclvalue(level1Var),nfacnew-nfacold))))
      putRcmdr("faclev2list", tclVar(c(as.character(tclObj(faclev2list)),
                                       rep(tclvalue(level2Var),nfacnew-nfacold))))
      putRcmdr("faclablist", tclVar(c(as.character(tclObj(faclablist)),
                                      rep("",nfacnew-nfacold))))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      tkconfigure(faclev1ListBox, listvariable=faclev1list, height=min(10,nfacnew))
      tkconfigure(faclev2ListBox, listvariable=faclev2list, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))

    }
  }
  nameenter <- function(){
    if (identical(tclvalue(getRcmdr("fileVar")),tclvalue(getRcmdr("nameVar"))))
      putRcmdr("name.equal.filename", TRUE)
    else putRcmdr("name.equal.filename", FALSE)
  }
  namechange <- function(){
    if (is.valid.name(tclvalue(nameVar))){
      if (name.equal.filename){
        putRcmdr("fileVar", tclVar(tclvalue(nameVar)))  ## otherwise, variables would be directly tied
        #          putRcmdr("exportlabVar", tclVar(paste("Current design to be saved:", tclvalue(nameVar),"\n   ")))
        tkconfigure(fileEntry, textvariable=getRcmdr("fileVar"))
        #          tkconfigure(exportlab, textvariable=getRcmdr("exportlabVar"))
      }
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid design name")
  }
  factorsel<-function(){
    #### aendert die in der Textbox dargestellte Auswahl
    #### ruiniert aber leider auch wieder die korrekte Ueberschreibung der Werte
    putRcmdr("selpos", as.numeric(tclvalue(tcl(fsel, "current")))+1)
    putRcmdr("curfac", tclVar(as.character(tclObj(varlistshort))[selpos]))
    putRcmdr("curfnam", tclVar(as.character(tclObj(facnamlist))[selpos]))
    putRcmdr("curflev1", tclVar(as.character(tclObj(faclev1list))[selpos]))
    putRcmdr("curflev2", tclVar(as.character(tclObj(faclev2list))[selpos]))
    putRcmdr("curflab", tclVar(as.character(tclObj(faclablist))[selpos]))
    tkconfigure(fnam, textvariable=curfnam)
    tkconfigure(flev1, textvariable=curflev1)
    tkconfigure(flev2, textvariable=curflev2)
    tkconfigure(flab, textvariable=curflab)
  }
  fnamchange <- function(){
    ## selpos known from factorsel
    if (is.valid.name(tclvalue(curfnam))){
      hilf <- as.character(tclObj(facnamlist))
      hilf[selpos] <- tclvalue(curfnam)
      putRcmdr("facnamlist",tclVar(hilf))
      ### "facnamlist" is not automatically updated in the listbox
      ### therefore the tkconfigure
      tkconfigure(facnameListBox, listvariable=facnamlist)
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid factor name")
  }
  level1enter <- function(){
    putRcmdr("the.common.level1", tclvalue(getRcmdr("level1Var")))
  }
  level1change <- function(){
    if (identical(getRcmdr("the.common.level1"), tclvalue(getRcmdr("level1Var")))) return()
    onRefresh()
  }
  level2enter <- function(){
    putRcmdr("the.common.level2", tclvalue(getRcmdr("level2Var")))
  }
  level2change <- function(){
    if (identical(getRcmdr("the.common.level2"), tclvalue(getRcmdr("level2Var")))) return()
    onRefresh()
  }
  flev1change <- function(){
    ## selpos known from factorsel
    if (length(as.character(tclObj(curflev1)))==1){
      hilf <- as.character(tclObj(faclev1list))
      hilf[selpos] <- tclvalue(curflev1)
      putRcmdr("faclev1list",tclVar(hilf))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
    }
    else tkmessageBox(message="Empty entries or entries with blanks are not permitted, please correct!",
                      icon="error", type="ok", title="Invalid factor level")
  }
  flev2change <- function(){
    ## selpos known from factorsel
    if (length(as.character(tclObj(curflev2)))==1){
      hilf <- as.character(tclObj(faclev2list))
      hilf[selpos] <- tclvalue(curflev2)
      putRcmdr("faclev2list",tclVar(hilf))
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
    }
    else tkmessageBox(message="Empty entries or entries with blanks are not permitted, please correct!",
                      icon="error", type="ok", title="Invalid factor level")
  }
  flabchange <- function(){
    ## selpos known from factorsel
    ## for FocusOut event on flab
    ## still problematic, if Focus out occurs with tab
    ## as there is also a tab key event
    hilf <- as.character(tclObj(faclablist))
    hilf[selpos] <- tclvalue(curflab)
    putRcmdr("faclablist",tclVar(hilf))
    tkconfigure(faclabListBox, listvariable=faclablist)
  }

  tabflab <- function(){
    ## for Tab key event on flab
    ## the traversal still jumps to the first traversable control on the sheet
    ## (rather than staying with fnam, if asked by tkfocus to do so)
    ## takefocus has so far been set to 0 for all widgets except the factor detail ones on this tab
    flabchange()  ## otherwise, not carried out!
    hilf <- as.numeric(tclvalue(tcl(fsel,"current")))+1
    if (hilf  >= as.numeric(tclvalue(nfacVar))) return()
    tcl(fsel,"current", hilf)
    factorsel()
    #tkfocus(fnam)
    #tcl(fnam, "selection", "range", 1, "end")
    #tcl("break")
  }


  swap <- function(a,b){
    hilf <- 1:as.numeric(tclvalue(nfacVar))
    hilf[b] <- a
    hilf[a] <- b
    hilf
  }

  indexchange <- function(){
    if (curindex < as.numeric(tclvalue(nfacVar)))
      putRcmdr("orderDown",swap(curindex, curindex+1))
    if (curindex > 1)
      putRcmdr("orderUp",swap(curindex, curindex-1))
    tcl(fsel, "current", curindex-1)
    factorsel()
  }

  checkIndexShort <- function(){
    putRcmdr("curindex", as.numeric(tcl(facshortListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexNam <- function(){
    putRcmdr("curindex", as.numeric(tcl(facnameListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLev1 <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclev1ListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLev2 <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclev2ListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLab <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclabListBox,"curselection"))+1)
    indexchange()
  }


  onUp <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex=="1" | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderUp]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[orderUp]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[orderUp]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderUp]))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex-1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(faclev1ListBox,"selection","set",curindex-1)
      tcl(faclev2ListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  onDown <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex==as.numeric(tclvalue(nfacVar)) | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderDown]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[orderDown]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[orderDown]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderDown]))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex+1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(faclev1ListBox,"selection","set",curindex-1)
      tcl(faclev2ListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  dquote <- function(obj){
    ## quote vector elements for use as character vector in a command
    aus <- rep("",length(obj))
    wopt <- options("warn")[[1]]
    options(warn=-1)
    for (i in 1:length(obj)) if (is.na(as.numeric(obj[i]))) {
      if (length(grep('"',obj[i])>0))
        aus[i] <- paste("'",obj[i],"'",sep="")
      else
        aus[i] <- paste('"',obj[i],'"',sep="")
    }
    else aus[i] <- obj[i]
    options(warn=wopt)
    aus
  }


  onChangeDir <- function(){
    putRcmdr("direct",tclvalue(tkchooseDirectory()))
    if (!direct=="") {
      putRcmdr("dirVar", tclVar(direct))
      tkconfigure(dirEntry, textvariable = dirVar)
    }
  }

  blockchange <- function(){
    if (!tclvalue(blockVar) %in% c("", "NULL")){
      putRcmdr("blockVar",tclVar(make.names(tclvalue(blockVar))))
      tkconfigure(blockEntry, textvariable = blockVar)}
  }

  ######## end define functions


  ##### define userform
  #tn <- ttknotebook(top,height=100, width=500)

  putRcmdr("tn",ttknotebook(topdes2))
  #tn <- ttknotebook(topdes2)

  putRcmdr("tab1",ttkframe(tn))
  putRcmdr("tab2",ttkframe(tn))
  putRcmdr("tab6",ttkframe(tn))## called 6 because of parallel treatment with
  ## fractional factorial menu

  tkadd(tn,tab1,text="Base Settings")   ### tabid=0
  tkadd(tn,tab2,text="Factor Details")  ### tabid=1
  tkadd(tn,tab6,text="Export") ### tabid=5

  tkconfigure(tn, takefocus=0)

  nameFrame <- ttkframe(tab1)
  baseFrame <- ttklabelframe(tab1,text=gettextRcmdr("Size and randomization"))

  ### widgets for tab1 and base frame
  putRcmdr("nameVar", tclVar(.stored.designbbd$nameVar))
  nameEntry <- tkentry(nameFrame, width="20", textvariable=nameVar)
  tkbind(nameEntry, "<FocusIn>", nameenter)
  tkbind(nameEntry, "<FocusOut>", namechange)

  nfacVar <- tclVar(.stored.designbbd$nfacVar)
  nfacEntry <- tkentry(baseFrame, width="8", textvariable=nfacVar)
  nfacHint <- ttklabel(baseFrame, text="(integer number from 3 to 7)", foreground="#888888")
  tkbind(nfacEntry,"<FocusOut>",nfacchange)
  ncenterVar <- tclVar(.stored.designbbd$ncenterVar)
  ncenterEntry <- tkentry(baseFrame, width="8", textvariable=ncenterVar)
  putRcmdr("blockVar", tclVar(.stored.designbbd$blockVar))
  blockEntry <- tkentry(baseFrame, width="8", textvariable=blockVar)
  tkbind(blockEntry, "<FocusOut>", blockchange)
  randomizeVariable <-  tclVar(.stored.designbbd$cbInitials[2])
  randomizecb <- ttkcheckbutton(baseFrame,text=gettextRcmdr("Randomization"),variable=randomizeVariable)
  tkconfigure(randomizecb, takefocus="0")
  seedVar <- tclVar(sample(31999,1))  ## always new
  seedEntry <- tkentry(baseFrame, width="8", textvariable=seedVar)
  tkconfigure(seedEntry, takefocus="0")

  ## preparations for bottom frame
  bottomFrame <- tkframe(topdes2)

  ## grid base frame
  ## omitted nfaccb, on form, nfactors must always be specified
  tkgrid(nfaclab <- tklabel(baseFrame, text=gettextRcmdr("Number of factors")), nfacEntry, nfacHint, sticky="w")
  tkgrid(ncenterlab <- tklabel(baseFrame, text=gettextRcmdr("Number of center points")), ncenterEntry, sticky="w")
  tkgrid(blocklab <- tklabel(baseFrame, text=gettextRcmdr("Name of block variable")), blockEntry, sticky="w")
  tkgrid.configure(blocklab, pady=15)
  tkgrid(randlab <- tklabel(baseFrame, text="You normally do not need to change randomization settings"),sticky="w",columnspan=3)
  tkgrid(seedlab <- tklabel(baseFrame, text=gettextRcmdr("Seed for randomization")), seedEntry,
         randomizecb, sticky="w")

  helptab1Button <- buttonRcmdr(nameFrame, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab1,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab1Button, takefocus=0)

  ### Finalize tab1
  tkgrid(tklabel(nameFrame, text="Name of new design"), nameEntry, helptab1Button, sticky="w")
  tkgrid(nameFrame, sticky="w", columnspan=4)
  tkgrid.configure(nameFrame, pady=40)
  tkgrid.configure(helptab1Button, sticky="ne")
  tkgrid(baseFrame, sticky="nw",columnspan=3)

  ## Factor Details Tab
  ## factor details frame
  ### facnameAutoVariable (not needed any more) and faclevelCommonVariable

  ## default levels frame
  deflevFrame <- ttklabelframe(tab2,text="Default levels")
  facnameAutoVariable <- tclVar(.stored.designbbd$cbInitials[3])
  faclevelCommonVariable <- tclVar(.stored.designbbd$cbInitials[4])
  faclevelCommonButton <- ttkcheckbutton(deflevFrame,text=gettextRcmdr("Common factor levels"),
                                         variable=faclevelCommonVariable,command=onRefresh)
  tkconfigure(faclevelCommonButton,takefocus=0)
  putRcmdr("level1Var", tclVar(.stored.designbbd$level1Var))
  level1Entry <- ttkentry(deflevFrame, width="20", textvariable=level1Var)
  tkconfigure(level1Entry,takefocus=0)
  tkbind(level1Entry, "<FocusIn>", level1enter)
  tkbind(level1Entry, "<FocusOut>", level1change)
  tkconfigure(level1Entry,takefocus=0)
  putRcmdr("level2Var", tclVar(.stored.designbbd$level2Var))
  level2Entry <- tkentry(deflevFrame, width="20", textvariable=level2Var)
  tkconfigure(level2Entry,takefocus=0)
  tkbind(level2Entry, "<FocusIn>", level2enter)
  tkbind(level2Entry, "<FocusOut>", level2change)
  tkgrid(faclevelCommonButton,sticky="w",columnspan=3)
  faclevCommonLab<-tklabel(deflevFrame,text=gettextRcmdr("CAUTION: Checking this box overwrites all custom factor levels."))
  if (!as.logical(as.numeric(tclvalue(faclevelCommonVariable)))){
    tkgrid(faclevCommonLab,sticky="w", columnspan=3)
    tkgrid.configure(faclevCommonLab,pady=10)
  }
  tkgrid(tklabel(deflevFrame, text=gettextRcmdr("Low Level")),tklabel(deflevFrame,text="  ",width=2),tklabel(deflevFrame, text=gettextRcmdr("High Level")),sticky="e")
  tkgrid(level1Entry, tklabel(deflevFrame,text="  ",width=2),level2Entry, sticky="e")

  ## factor details
  ## values as vectors
  facnamlistt <- .stored.designbbd$facnamlist
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))) {
    faclev1listt <- rep(tclvalue(level1Var),tclvalue(nfacVar))
    faclev2listt <- rep(tclvalue(level2Var),tclvalue(nfacVar))
  } else{
    faclev1listt <- .stored.designbbd$faclev1list
    faclev2listt <- .stored.designbbd$faclev2list
  }
  faclablistt <- .stored.designbbd$faclablist
  varlistshortt <- if (as.numeric(tclvalue(nfacVar))<=50)
    Letters[1:tclvalue(nfacVar)] else paste("F",1:tclvalue(nfacVar),sep="")

  enterlistFrame <- ttkframe(tab2)
  listFrame <- ttklabelframe(enterlistFrame, text="Factor Details")
  putRcmdr("selpos", 1)
  putRcmdr("curfac", tclVar(varlistshortt[1]))
  putRcmdr("curfnam", tclVar(facnamlistt[1]))
  putRcmdr("curflev1", tclVar(faclev1listt[1]))
  putRcmdr("curflev2", tclVar(faclev2listt[1]))
  putRcmdr("curflab", tclVar(faclablistt[1]))

  ## fsel must select the right factor
  ## this should be highlighted in factor lists
  ##    and all related entries shown for changing in text boxes fnam etc.
  enterFrame <- ttklabelframe(enterlistFrame, text=gettextRcmdr("Modify factor details for selected factor"))
  fsel <- ttkcombobox(enterFrame, textvariable=curfac, width=5, values=varlistshortt, state="readonly")
  tkbind(fsel, "<<ComboboxSelected>>", factorsel)
  #fnam <- ttkentry(listFrame, textvariable=curfnam, width=20,validate="focusout", validatecommand=fnamchange)
  fnam <- ttkentry(enterFrame, textvariable=curfnam, width=15)
  tkbind(fnam, "<FocusOut>", fnamchange)
  flev1 <- ttkentry(enterFrame, textvariable=curflev1, width=15)
  tkbind(flev1, "<FocusOut>", flev1change)
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))){
    tkconfigure(flev1,state="disabled")
  }
  flev2 <- ttkentry(enterFrame, textvariable=curflev2, width=15)
  tkbind(flev2, "<FocusOut>", flev2change)
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))){
    tkconfigure(flev2,state="disabled")
  }
  flab <- ttkentry(enterFrame, textvariable=curflab, width=20)
  tkbind(flab, "<FocusOut>", flabchange)
  tkbind(flab, "<Key-Tab>", tabflab)
  tkgrid(tklabel(enterFrame,text=gettextRcmdr("Select"),width=6),
         tklabel(enterFrame,text=gettextRcmdr("Factor name"), width=15),
         tklabel(enterFrame,text=gettextRcmdr("Low level"), width=15),
         tklabel(enterFrame,text=gettextRcmdr("High level"), width=15),
         tklabel(enterFrame,text=gettextRcmdr("Comment or label \n(for html export only)"), width=20),
         sticky="w")
  tkgrid(fsel,fnam, flev1, flev2, flab, sticky="w")

  putRcmdr("facnamlist", tclVar(facnamlistt))
  putRcmdr("varlistshort", tclVar(varlistshortt))
  putRcmdr("faclev1list", tclVar(faclev1listt))
  putRcmdr("faclev2list", tclVar(faclev2listt))
  putRcmdr("faclablist", tclVar(faclablistt))

  facshortListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                               selectmode = single, exportselection = "TRUE", listvariable=varlistshort,
                               width = 6, background="#EBEBDC")
  tkbind(facshortListBox, "<<TraverseIn>>",function() tkfocus(fsel))

  facnameListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=facnamlist,
                              width = 15, background="#EBEBDC")
  faclev1ListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=faclev1list,
                              width = 15, background="#EBEBDC")
  faclev2ListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=faclev2list,
                              width = 15, background="#EBEBDC")
  faclabListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                             selectmode = single, exportselection = "TRUE", listvariable=faclablist,
                             width = 20, background="#EBEBDC")

  ## determine current index and reordering for onUp and onDown
  tkbind(facshortListBox, "<<ListboxSelect>>", checkIndexShort)
  tkbind(facnameListBox, "<<ListboxSelect>>", checkIndexNam)
  tkbind(faclev1ListBox, "<<ListboxSelect>>", checkIndexLev1)
  tkbind(faclev2ListBox, "<<ListboxSelect>>", checkIndexLev2)
  tkbind(faclabListBox, "<<ListboxSelect>>", checkIndexLab)


  ### funktioniert, ist aber noch nicht schön
  scrollbar <- ttkscrollbar(listFrame, command = function(...) {
    tkyview(facshortListBox, ...)
    tkyview(facnameListBox, ...)
    tkyview(faclev1ListBox, ...)
    tkyview(faclev2ListBox, ...)
    tkyview(faclabListBox, ...)
  })

  #    tkgrid(tklabel(enterlistFrame,text="  ", width=5),enterFrame, sticky="w")
  tkgrid(enterFrame, sticky="w", columnspan=5)
  tkgrid.configure(enterFrame, pady=10)
  ## Hoch-/Runterschieben von Einträgen ermöglichen

  downupFrame <- ttkframe(listFrame)
  moveDownButton <- buttonRcmdr(downupFrame, text = gettextRcmdr("Move Down"),
                                foreground = "darkgreen", command = onDown,
                                default = "normal", borderwidth = 3, width=12)
  moveUpButton <- buttonRcmdr(downupFrame, text = gettextRcmdr("Move Up"),
                              foreground = "darkgreen", command = onUp,
                              default = "normal", borderwidth = 3, width=12)
  tkgrid(moveDownButton, sticky="w")
  tkgrid(moveUpButton, sticky="w")

  tkgrid(scrollbar, facshortListBox, facnameListBox, faclev1ListBox, faclev2ListBox, faclabListBox, downupFrame, sticky = "nw")
  tkgrid.configure(scrollbar, sticky = "wns")
  tkgrid.configure(facnameListBox, sticky = "new")

  helptab2Button <- buttonRcmdr(tab2, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab2,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab2Button, takefocus=0)

  ## finalize tab2 Factor details
  tkgrid(helptab2Button, sticky="e")
  tkgrid(deflevFrame, sticky="nw")
  tkgrid.configure(deflevFrame, pady=10)
  tkgrid(listFrame, columnspan=6,sticky="w")
  tkgrid(enterlistFrame, columnspan=6,sticky="w")

  ## tab6 for exporting
  helptab6Button <- buttonRcmdr(tab6, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab6,
                                default = "normal", borderwidth = 3)

  exportlabVar <- nameVar
  exportlab <- ttklabel(tab6, textvariable=exportlabVar)
  tkgrid(ttklabel(tab6,text="Current design to be saved:"),exportlab,helptab6Button,sticky="w")
  tkgrid.configure(exportlab, pady=15)
  tkgrid.configure(helptab6Button, sticky="ne")

  ## radio buttons for choosing export type
  etradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("(How to) Export ?"))
  etyperbVariable <- tclVar(.stored.designbbd$etyperbVariable)
  noexprb <- tkradiobutton(etradioFrame,text=gettextRcmdr("no export"),variable=etyperbVariable,value="none")
  allrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("all file types"),variable=etyperbVariable,value="all")
  rdarb <- tkradiobutton(etradioFrame,text=gettextRcmdr("rda only"),variable=etyperbVariable,value="rda")
  htmlrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("html and rda"),variable=etyperbVariable,value="html")
  csvrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("csv and rda"),variable=etyperbVariable,value="csv")
  tkgrid(noexprb, sticky="w")
  tkgrid(allrb, sticky="w")
  tkgrid(rdarb, sticky="w")
  tkgrid(htmlrb, sticky="w")
  tkgrid(csvrb, sticky="w")

  ## radio buttons for choosing export decimal separator
  decimalradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("Decimal Separator ?"))
  decimalrbVariable <- tclVar(.stored.designbbd$decimalrbVariable)
  defaultrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("default"),variable=decimalrbVariable, value="default")
  pointrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("."),variable=decimalrbVariable, value=".")
  commarb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr(","),variable=decimalrbVariable, value=",")
  tkgrid(defaultrb, sticky="w")  ## in this case, leave default option from options
  tkgrid(pointrb, sticky="w")
  tkgrid(commarb, sticky="w")

  ## export directory
  dirFrame <- ttklabelframe(tab6, text=gettextRcmdr("Storage Directory"))
  putRcmdr("dirVar", tclVar(.stored.designbbd$dirVar))
  dirEntry <- tkentry(dirFrame, width="50", textvariable=dirVar)
  dirButton <- buttonRcmdr(dirFrame, text = gettextRcmdr("Change directory"),
                           foreground = "darkgreen", width = "20", command = onChangeDir,
                           default = "normal", borderwidth = 3)
  tkgrid(dirEntry, tklabel(dirFrame, text="   "), dirButton, sticky="w")

  ## export file name
  putRcmdr("fileVar", tclVar(.stored.designbbd$fileVar))
  fileEntry <- tkentry(tab6, width="20", textvariable=fileVar)
  efnamelabel <- tklabel(tab6,text=gettextRcmdr("Export file names: name below with appropriate endings (html or csv, and rda)"))
  replacecbVariable <- tclVar(.stored.designbbd$cbInitials[8])
  replacecb <- ttkcheckbutton(tab6,text=gettextRcmdr("Replace file(s), if exists"),variable=replacecbVariable)

  ## always grid details, as otherwise default file name does not work
  ## design name info and help button have already been gridded above
  tkgrid(etradioFrame, decimalradioFrame, sticky="nw")
  tkgrid(dirFrame, sticky="w", columnspan=5)
  tkgrid.configure(dirFrame, pady=15)
  tkgrid(efnamelabel, sticky="w", columnspan=5)
  tkgrid(fileEntry, sticky="w", columnspan=5)
  tkgrid(replacecb, sticky="w", columnspan=5)


  ## add buttons outside the notebook
  buttonFrame <- tkframe(topdes2)
  ## die sind aber nicht dunkelgruen ...
  refreshButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Refresh form"),
                               foreground = "darkgreen", width = "12", command = onRefresh,
                               default = "normal", borderwidth = 3)
  storeButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Store form"),
                             foreground = "darkgreen", width = "12", command = onStore,
                             default = "normal", borderwidth = 3)
  loadButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Load form"),
                            foreground = "darkgreen", width = "12", command = onLoad,
                            default = "normal", borderwidth = 3)
  resetButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Reset form"),
                             foreground = "darkgreen", width = "12", command = onReset,
                             default = "normal", borderwidth = 3)
  #        tkgrid(refreshButton,sticky="w")
  #        tkgrid(tklabel(buttonFrame,text="  "),sticky="w")
  tkgrid(storeButton,sticky="w")
  tkgrid(loadButton,sticky="w")
  tkgrid(resetButton,sticky="w")

  tkconfigure(refreshButton, takefocus=0)
  tkconfigure(storeButton, takefocus=0)
  tkconfigure(loadButton, takefocus=0)
  tkconfigure(resetButton, takefocus=0)

  ## storage buttons to the right of the notebook
  tkgrid(tn, buttonFrame, sticky="w", columnspan=2)

  OKCancelHelp(window=topdes2, helpSubject="Menu.bbd")
  tkconfigure(OKbutton, takefocus=0)
  tkconfigure(cancelButton, takefocus=0)
  tkconfigure(helpButton, takefocus=0)

  tkgrid(buttonsFrame, bottomFrame, sticky="ew")


  ### relations among widgets
  if (!as.logical(as.numeric(tclvalue(randomizeVariable)))){
    tkconfigure(seedEntry, state="disabled")
    tkconfigure(seedlab, state="disabled")
  }else {
    tkconfigure(seedEntry, state="normal")
    tkconfigure(seedlab, state="normal")
  }
  if (exists("activestab.tn", where="RcmdrEnv")){
    tcl(tn, "select", activestab.tn)
    rm(activestab.tn, pos="RcmdrEnv")
  }

  dialogSuffix(window=topdes2, rows=2, columns=2, focus=tn, bindReturn=FALSE)

}

Menu.lhs <- function(){
  initializeDialogDoE(title=gettextRcmdr("Create latin hypercube sample ..."))
  ## function initializeDialogDoE assumes topdes2 as windowname
  ## last stored top left corner for window is stored under topleft2xy
  ## onRefresh still makes window walk a little

  if (exists("curindex", where="RcmdrEnv")) rm(curindex, pos="RcmdrEnv")

  if (!exists(".stored.designlhs",where="RcmdrEnv"))
    assign(".stored.designlhs", .default.designlhs,pos="RcmdrEnv")
  ## nameVar, nrunVar, nfacVar, nrepVar
  ## cbInitials containing repeat.onlyVariable, randomizeVariable,
  ##                       facnamesAutoVariable, faclevelsCommonVariable,
  ##                       nrunEntryVariable, estcbVariable
  ##                       specialcbVariable, replacecbVariable, MaxC2cbVariable
  ##                       res3cbVariable
  ## level1Var, level2Var, seedVar, specialrbVariable, hardVar, genVar,
  ## catlgVar, designVar, designrbVariable, destyperbVariable
  ## resVar, qualcritrbVariable, facnamlist,faclev1list,faclev2list, faclablist
  ## etyperbVariable, decimalrbVariable, dirVar, fileVar

  ## MaxC2cbVariable is free again (no. 9 of cbInitials)

  ## define called functions
  infoClose <- function(){
    putRcmdr("infotxt",tclVar(""))
  }

  onHelpTab1 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.lhsTab1"))
  }
  onHelpTab2 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.FacDetails2Tab"))
  }
  onHelpTab6 <- function(){
    if (GrabFocus() && .Platform$OS.type != "windows")
      tkgrab.release(topdes2)
    print(help("Menu.exportTab"))
  }

  tabpos <- function(){
    ### get 0-based index of currently selected tab
    activestab.tn <- tclvalue(tcl(tn, "select"))
    activestab.tn <- strsplit(activestab.tn,".",fixed=TRUE)[[1]]
    activestab.tn <- as.numeric(activestab.tn[length(activestab.tn)])-1
    activestab.tn
  }

  storeRcmdr <- function(){
    hilf <- list(nameVar=tclvalue(nameVar),
                 nrunVar=tclvalue(nrunVar),nfacVar=tclvalue(nfacVar),
                 digitsVar=tclvalue(digitsVar),
                 typerbVariable=tclvalue(typerbVariable),
                 cbInitials = c("0", "1",
                                tclvalue(facnameAutoVariable),tclvalue(faclevelCommonVariable),
                                1,0,
                                0,tclvalue(replacecbVariable),0,
                                0
                 ),
                 level1Var=tclvalue(level1Var),level2Var=tclvalue(level2Var),seedVar=tclvalue(seedVar),
                 facnamlist=as.character(tclObj(facnamlist)),
                 faclev1list=as.character(tclObj(faclev1list)),
                 faclev2list=as.character(tclObj(faclev2list)),
                 faclablist=as.character(tclObj(faclablist)),
                 etyperbVariable=tclvalue(etyperbVariable),
                 decimalrbVariable=tclvalue(decimalrbVariable),
                 dirVar=tclvalue(dirVar), fileVar=tclvalue(fileVar))
    class(hilf) <- c("menu.designlhs","list")
    putRcmdr(".stored.designlhs",hilf)
  }

  onOK <- function(){
    onRefreshEnd()
    ## store entries so that users do not have to redo everything
    ## in case of stupid mistakes
    storeRcmdr()
    ## seed is not used from previously stored design
    name <- tclvalue(nameVar)
    if (!is.valid.name(name)){
      errorCondition(window=topdes2,recall=Menu.lhs,
                     message=paste('"', name, '" ', gettextRcmdr("is not a valid name."), sep=""))
      return()
    }
    if (is.element(name, listObjects()))
    {
      if ("no" == tclvalue(checkReplace(name, gettextRcmdr("Object"))))
      {
        errorCondition(window=topdes2,recall=Menu.lhs,
                       gettextRcmdr("Introduce another name for the new data.frame, or allow replacing."))
        return()
      }
    }
    ###  further error messages with return to menu ?

    textfactornameslist.forcommand <- paste("factor.names=list(",paste(paste(as.character(tclObj(facnamlist)),"=c(",
                                                                             dquote(as.character(tclObj(faclev1list))), ",",
                                                                             dquote(as.character(tclObj(faclev2list))), ")",sep=""),
                                                                       collapse=","),")")

    ### not yet perfect, especially NULL entries are not possible
    ### for didactic reasons distinguish between usage of default.levels and other?
    command <- paste("lhs.design(","type=",dquote(tclvalue(typerbVariable)),
                     ", nruns=",tclvalue(nrunVar),",nfactors=",tclvalue(nfacVar),
                     ",digits=",tclvalue(digitsVar), ",seed=",tclvalue(seedVar),
                     ",",textfactornameslist.forcommand,")")

    hilf <- justDoItDoE(command)
    closeDialog(window=topdes2)
    if (class(hilf)[1]=="try-error") {
      Message(paste(gettextRcmdr("Offending command:"), "\n", command), type="error")
      errorCondition(window=topdes2,recall=Menu.lhs, message=gettextRcmdr(hilf))
      return()
    }

    logger(paste(name, "<-", command))
    logger("## creator element of design.info will be different, when using the command line command!")
    ## change creator to contain menu settings
    hilfatt <- design.info(hilf)
    hilfatt$creator <- .stored.designlhs
    class(hilfatt$creator) <- c("menu.designlhs", "list")
    attr(hilf, "design.info") <- hilfatt
    putRcmdr("hilf", hilf)
    ## replace assign by justDoIt; assign(name, hilf, envir=.GlobalEnv)
    justDoIt(paste(name, "<- getRcmdr(\"hilf\")"))
    rm("hilf", pos="RcmdrEnv")
    activeDataSet(name)
    ### exporting
    if (!tclvalue(etyperbVariable)=="none"){
      putRcmdr("path", tclvalue(dirVar))
      putRcmdr("filename", tclvalue(fileVar))
      if (!as.logical(as.numeric(tclvalue(replacecbVariable)))){
        lf <- tolower(list.files(path = path))
        if (tolower(paste(filename, "rda", sep = ".")) %in% lf)
          stop("file ", paste(filename, "rda", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="html" & tolower(paste(filename, "html", sep = ".")) %in% lf)
          stop("file ", paste(filename, "html", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
        if (tclvalue(etyperbVariable)=="csv" & tolower(paste(filename, "csv", sep = ".")) %in% lf)
          stop("file ", paste(filename, "csv", "."),
               " exists and must not be replaced. Change filename on Export tab or allow replacing of files.")
      }
      if (tclvalue(decimalrbVariable)=="default") command <- paste("export.design(",name,
                                                                   ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                                                                   as.logical(as.numeric(tclvalue(replacecbVariable))),")",sep="")
      else command <- paste("export.design(",name,
                            ", type=",dquote(tclvalue(etyperbVariable)),",path=",dquote(path),", file=",dquote(filename),", replace=",
                            as.logical(as.numeric(tclvalue(replacecbVariable))),", OutDec=", dquote(tclvalue(decimalrbVariable)),")",sep="")
      hilf <- justDoItDoE(command)
      if (class(hilf)[1]=="try-error") {
        errorCondition(window=topdes2,recall=Menu.lhs, message=gettextRcmdr(hilf))
        return()
      }
      logger(command)
    }
    rm(activestab.tn, pos="RcmdrEnv")
    tkwm.deiconify(CommanderWindow())
    tkfocus(CommanderWindow())
  }

  listDesignlhs <- function (envir = .GlobalEnv, ...)
  {
    Vars <- ls(envir = envir, all.names = TRUE)
    Vars[which(sapply(Vars, function(.x){
      aus <- FALSE
      if ("menu.designlhs" %in% class(get(.x, envir = envir))) aus <- TRUE
      else if ("design" %in% class(get(.x, envir = envir)))
        if ("menu.designlhs" %in% class(design.info(get(.x, envir = envir))$creator))
          aus <- TRUE
        aus
    }))]
  }


  onLoad <- function(){
    ## seems to work now, needs to be tested!
    hilf <- listDesignlhs()
    if (length(hilf)==0) {
      tkmessageBox(message=gettextRcmdr("There are no stored design inputs in this session."),
                   icon="error", type="ok", title="no stored design inputs")
      return()
    }
    putRcmdr("deschoose2",tktoplevel())
    tkwm.title(deschoose2, gettextRcmdr("Choose stored design form"))
    position <- if (is.SciViews())
      -1
    else position <- "+50+50"
    tkwm.geometry(deschoose2, position)
    putRcmdr("lb", variableListBox(deschoose2, variableList=hilf, title="Choose stored design form"))
    tkgrid(lb$frame)
    onOK <- function() {
      putRcmdr(".stored.designlhs",get(lb$varlist[as.numeric(tclvalue(tcl(lb$listbox, "curselection")))+1]))
      if ("design" %in% class(getRcmdr(".stored.designlhs")))
        putRcmdr(".stored.designlhs", design.info(getRcmdr(".stored.designlhs"))$creator)
      tkfocus(CommanderWindow())
      tkdestroy(topdes2)
      tkdestroy(deschoose2)
      Menu.lhs()
    }
    OKCancelHelp(window=deschoose2)
    tkgrid(buttonsFrame, sticky="s")
    dialogSuffix(window=deschoose2, rows=1, columns=1,
                 focus=lb$listbox)
  }

  onRefreshEnd <- function(){
    nfacchange()
    storeRcmdr()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    putRcmdr("activestab.tn",tabpos())
    ID <- topdes2$ID
    putRcmdr("topleft2xy",as.numeric(c(tclvalue(.Tcl(paste("winfo rootx", ID))),
                                       tclvalue(.Tcl(paste("winfo rooty", ID))))))
    #        assign("activestab.tn",strsplit(activestab.tn,".",fixed=TRUE)[[1]],pos="RcmdrEnv")
    #        assign("activestab.tn",as.numeric(activestab.tn[length(activestab.tn)])-1,pos="RcmdrEnv")
  }

  onRefresh <- function(){
    #print(as.character(tclObj(tcl(tn, "select"))))
    onRefreshEnd()
    ## letzte Position enthaelt tab index (beginnend bei 1)
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.lhs()
  }

  onStore <- function(){
    ## Speichernamen abfragen und hier ermöglichen (statt stored.designlhs)
    textentry() ## creates text string stored in savename.RcmdrPlugin.DoE
    if (!is.null(savename.RcmdrPlugin.DoE)){
      if (!is.valid.name(savename.RcmdrPlugin.DoE)){
        textcorrect(gettextRcmdr("This is not a valid name. Please correct:"))
        return()
      }
      if (is.element(savename.RcmdrPlugin.DoE, listObjects()))
      {
        if ("no" == tclvalue(checkReplace(savename.RcmdrPlugin.DoE, gettextRcmdr("Object"))))
        {
          textcorrect(gettextRcmdr("Please enter a new name:"))
          return()
        }
      }
      storeRcmdr()
      ## replace assign by justDoIt; assign(savename.RcmdrPlugin.DoE, getRcmdr(".stored.designlhs"), envir=.GlobalEnv)
      justDoIt(paste(savename.RcmdrPlugin.DoE, "<- getRcmdr(\".stored.designlhs\")"))
      message(gettextRcmdr("inputs have been stored"))
    }
  }

  onReset <- function(){
    assign(".stored.designlhs",.default.designlhs,pos="RcmdrEnv")
    tkfocus(CommanderWindow())
    tkdestroy(topdes2)
    Menu.lhs()
  }

  nfacchange <- function(){
    nfacold <- length(as.character(tclObj(varlistshort)))
    nfacnew <- as.numeric(tclvalue(nfacVar))
    if (nfacold==nfacnew) return()
    if (nfacnew < nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[1:nfacnew]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[1:nfacnew]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[1:nfacnew]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[1:nfacnew]))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(faclev1ListBox, listvariable=faclev1list, height=min(10,nfacnew))
      tkconfigure(faclev2ListBox, listvariable=faclev2list, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      if (selpos > nfacnew){
        tcl(fsel, "current", "0")
        factorsel()
      }
    }
    if (nfacnew > nfacold){
      varlistshortt <- if (nfacnew<=50)
        Letters[1:nfacnew] else paste("F",1:nfacnew,sep="")
      putRcmdr("varlistshortt" , varlistshortt)
      putRcmdr("varlistshort", tclVar(getRcmdr("varlistshortt")))
      putRcmdr("facnamlist", tclVar(c(as.character(tclObj(facnamlist)),
                                      getRcmdr("varlistshortt")[(nfacold+1):nfacnew])) )
      putRcmdr("faclev1list", tclVar(c(as.character(tclObj(faclev1list)),
                                       rep(tclvalue(level1Var),nfacnew-nfacold))))
      putRcmdr("faclev2list", tclVar(c(as.character(tclObj(faclev2list)),
                                       rep(tclvalue(level2Var),nfacnew-nfacold))))
      putRcmdr("faclablist", tclVar(c(as.character(tclObj(faclablist)),
                                      rep("",nfacnew-nfacold))))
      tkconfigure(facshortListBox, listvariable=varlistshort, height=min(10,nfacnew))
      tkconfigure(fsel, values=varlistshortt)
      tkconfigure(facnameListBox, listvariable=facnamlist, height=min(10,nfacnew))
      tkconfigure(faclev1ListBox, listvariable=faclev1list, height=min(10,nfacnew))
      tkconfigure(faclev2ListBox, listvariable=faclev2list, height=min(10,nfacnew))
      tkconfigure(faclabListBox, listvariable=faclablist, height=min(10,nfacnew))
    }
  }
  nameenter <- function(){
    if (identical(tclvalue(getRcmdr("fileVar")),tclvalue(getRcmdr("nameVar"))))
      putRcmdr("name.equal.filename", TRUE)
    else putRcmdr("name.equal.filename", FALSE)
  }
  namechange <- function(){
    if (is.valid.name(tclvalue(nameVar))){
      if (name.equal.filename){
        putRcmdr("fileVar", tclVar(tclvalue(nameVar)))  ## otherwise, variables would be directly tied
        #          putRcmdr("exportlabVar", tclVar(paste("Current design to be saved:", tclvalue(nameVar),"\n   ")))
        tkconfigure(fileEntry, textvariable=getRcmdr("fileVar"))
        #          tkconfigure(exportlab, textvariable=getRcmdr("exportlabVar"))
      }
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid design name")
  }
  factorsel<-function(){
    #### aendert die in der Textbox dargestellte Auswahl
    #### ruiniert aber leider auch wieder die korrekte Ueberschreibung der Werte
    putRcmdr("selpos", as.numeric(tclvalue(tcl(fsel, "current")))+1)
    putRcmdr("curfac", tclVar(as.character(tclObj(varlistshort))[selpos]))
    putRcmdr("curfnam", tclVar(as.character(tclObj(facnamlist))[selpos]))
    putRcmdr("curflev1", tclVar(as.character(tclObj(faclev1list))[selpos]))
    putRcmdr("curflev2", tclVar(as.character(tclObj(faclev2list))[selpos]))
    putRcmdr("curflab", tclVar(as.character(tclObj(faclablist))[selpos]))
    tkconfigure(fnam, textvariable=curfnam)
    tkconfigure(flev1, textvariable=curflev1)
    tkconfigure(flev2, textvariable=curflev2)
    tkconfigure(flab, textvariable=curflab)
  }
  fnamchange <- function(){
    ## selpos known from factorsel
    if (is.valid.name(tclvalue(curfnam))){
      hilf <- as.character(tclObj(facnamlist))
      hilf[selpos] <- tclvalue(curfnam)
      putRcmdr("facnamlist",tclVar(hilf))
      ### "facnamlist" is not automatically updated in the listbox
      ### therefore the tkconfigure
      tkconfigure(facnameListBox, listvariable=facnamlist)
    }
    else tkmessageBox(message="invalid name!",icon="error", type="ok", title="Invalid factor name")
  }
  level1enter <- function(){
    putRcmdr("the.common.level1", tclvalue(getRcmdr("level1Var")))
  }
  level1change <- function(){
    if (identical(getRcmdr("the.common.level1"), tclvalue(getRcmdr("level1Var")))) return()
    onRefresh()
  }
  level2enter <- function(){
    putRcmdr("the.common.level2", tclvalue(getRcmdr("level2Var")))
  }
  level2change <- function(){
    if (identical(getRcmdr("the.common.level2"), tclvalue(getRcmdr("level2Var")))) return()
    onRefresh()
  }
  flev1change <- function(){
    ## selpos known from factorsel
    if (length(as.character(tclObj(curflev1)))==1){
      hilf <- as.character(tclObj(faclev1list))
      hilf[selpos] <- tclvalue(curflev1)
      putRcmdr("faclev1list",tclVar(hilf))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
    }
    else tkmessageBox(message="Empty entries or entries with blanks are not permitted, please correct!",
                      icon="error", type="ok", title="Invalid factor level")
  }
  flev2change <- function(){
    ## selpos known from factorsel
    if (length(as.character(tclObj(curflev2)))==1){
      hilf <- as.character(tclObj(faclev2list))
      hilf[selpos] <- tclvalue(curflev2)
      putRcmdr("faclev2list",tclVar(hilf))
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
    }
    else tkmessageBox(message="Empty entries or entries with blanks are not permitted, please correct!",
                      icon="error", type="ok", title="Invalid factor level")
  }
  flabchange <- function(){
    ## selpos known from factorsel
    ## for FocusOut event on flab
    ## still problematic, if Focus out occurs with tab
    ## as there is also a tab key event
    hilf <- as.character(tclObj(faclablist))
    hilf[selpos] <- tclvalue(curflab)
    putRcmdr("faclablist",tclVar(hilf))
    tkconfigure(faclabListBox, listvariable=faclablist)
  }

  tabflab <- function(){
    ## for Tab key event on flab
    ## the traversal still jumps to the first traversable control on the sheet
    ## (rather than staying with fnam, if asked by tkfocus to do so)
    ## takefocus has so far been set to 0 for all widgets except the factor detail ones on this tab
    flabchange()  ## otherwise, not carried out!
    hilf <- as.numeric(tclvalue(tcl(fsel,"current")))+1
    if (hilf  >= as.numeric(tclvalue(nfacVar))) return()
    tcl(fsel,"current", hilf)
    factorsel()
    #tkfocus(fnam)
    #tcl(fnam, "selection", "range", 1, "end")
    #tcl("break")
  }


  swap <- function(a,b){
    hilf <- 1:as.numeric(tclvalue(nfacVar))
    hilf[b] <- a
    hilf[a] <- b
    hilf
  }

  indexchange <- function(){
    if (curindex < as.numeric(tclvalue(nfacVar)))
      putRcmdr("orderDown",swap(curindex, curindex+1))
    if (curindex > 1)
      putRcmdr("orderUp",swap(curindex, curindex-1))
    tcl(fsel, "current", curindex-1)
    factorsel()
  }

  checkIndexShort <- function(){
    putRcmdr("curindex", as.numeric(tcl(facshortListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexNam <- function(){
    putRcmdr("curindex", as.numeric(tcl(facnameListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLev1 <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclev1ListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLev2 <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclev2ListBox,"curselection"))+1)
    indexchange()
  }
  checkIndexLab <- function(){
    putRcmdr("curindex", as.numeric(tcl(faclabListBox,"curselection"))+1)
    indexchange()
  }


  onUp <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex=="1" | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderUp]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[orderUp]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[orderUp]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderUp]))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex-1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(faclev1ListBox,"selection","set",curindex-1)
      tcl(faclev2ListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  onDown <- function(){
    if (!exists("curindex")) return()
    if (length(curindex)==0) return()
    if (curindex==as.numeric(tclvalue(nfacVar)) | is.null(curindex)) return()
    else {
      putRcmdr("facnamlist", tclVar(as.character(tclObj(facnamlist))[orderDown]))
      putRcmdr("faclev1list", tclVar(as.character(tclObj(faclev1list))[orderDown]))
      putRcmdr("faclev2list", tclVar(as.character(tclObj(faclev2list))[orderDown]))
      putRcmdr("faclablist", tclVar(as.character(tclObj(faclablist))[orderDown]))
      tkconfigure(faclev1ListBox, listvariable=faclev1list)
      tkconfigure(faclev2ListBox, listvariable=faclev2list)
      tkconfigure(faclabListBox, listvariable=faclablist)
      tkconfigure(facnameListBox, listvariable=facnamlist)
      putRcmdr("curindex", curindex+1)
      indexchange()
      tcl(facshortListBox,"selection","set",curindex-1)
      tcl(faclev1ListBox,"selection","set",curindex-1)
      tcl(faclev2ListBox,"selection","set",curindex-1)
      tcl(faclabListBox,"selection","set",curindex-1)
      tcl(facnameListBox,"selection","set",curindex-1)
    }
  }

  dquote <- function(obj){
    ## quote vector elements for use as character vector in a command
    aus <- rep("",length(obj))
    wopt <- options("warn")[[1]]
    options(warn=-1)
    for (i in 1:length(obj)) if (is.na(as.numeric(obj[i]))) {
      if (length(grep('"',obj[i])>0))
        aus[i] <- paste("'",obj[i],"'",sep="")
      else
        aus[i] <- paste('"',obj[i],'"',sep="")
    }
    else aus[i] <- obj[i]
    options(warn=wopt)
    aus
  }


  onChangeDir <- function(){
    putRcmdr("direct",tclvalue(tkchooseDirectory()))
    if (!direct=="") {
      putRcmdr("dirVar", tclVar(direct))
      tkconfigure(dirEntry, textvariable = dirVar)
    }
  }

  ######## end define functions


  ##### define userform
  #tn <- ttknotebook(top,height=100, width=500)

  putRcmdr("tn",ttknotebook(topdes2))
  #tn <- ttknotebook(topdes2)

  putRcmdr("tab1",ttkframe(tn))
  putRcmdr("tab2",ttkframe(tn))
  putRcmdr("tab6",ttkframe(tn))## called 6 because of parallel treatment with
  ## fractional factorial menu

  tkadd(tn,tab1,text="Base Settings")   ### tabid=0
  tkadd(tn,tab2,text="Factor Details")  ### tabid=1
  tkadd(tn,tab6,text="Export") ### tabid=5

  tkconfigure(tn, takefocus=0)

  nameFrame <- ttkframe(tab1)
  typeradioFrame <- ttklabelframe(tab1, text=gettextRcmdr("Type of lhs ?"))
  baseFrame <- ttklabelframe(tab1,text=gettextRcmdr("Size and randomization"))

  ### widgets for tab1 and base frame
  putRcmdr("nameVar", tclVar(.stored.designlhs$nameVar))
  nameEntry <- tkentry(nameFrame, width="20", textvariable=nameVar)
  tkbind(nameEntry, "<FocusIn>", nameenter)
  tkbind(nameEntry, "<FocusOut>", namechange)

  typerbVariable <- tclVar(.stored.designlhs$typerbVariable)
  optimumrb <- tkradiobutton(typeradioFrame,text=gettextRcmdr("optimum"),variable=typerbVariable, value="optimum")
  geneticrb <- tkradiobutton(typeradioFrame,text=gettextRcmdr("genetic"),variable=typerbVariable, value="genetic")
  improvedrb <- tkradiobutton(typeradioFrame,text=gettextRcmdr("improved"),variable=typerbVariable, value="improved")
  maximinrb <- tkradiobutton(typeradioFrame,text=gettextRcmdr("maximin"),variable=typerbVariable, value="maximin")
  randomrb <- tkradiobutton(typeradioFrame,text=gettextRcmdr("random"),variable=typerbVariable, value="random")
  tkgrid(optimumrb, sticky="w")
  tkgrid(geneticrb, sticky="w")
  tkgrid(improvedrb, sticky="w")
  tkgrid(maximinrb, sticky="w")
  tkgrid(randomrb, sticky="w")

  nrunVar <- tclVar(.stored.designlhs$nrunVar)
  nrunEntry <- tkentry(baseFrame, width="8", textvariable=nrunVar)
  nrunHint <- ttklabel(baseFrame, text="(positive integer number)", foreground="#888888")
  nfacVar <- tclVar(.stored.designlhs$nfacVar)
  nfacEntry <- tkentry(baseFrame, width="8", textvariable=nfacVar)
  nfacHint <- ttklabel(baseFrame, text="(positive integer number)", foreground="#888888")
  tkbind(nfacEntry,"<FocusOut>",nfacchange)
  digitsVar <- tclVar(.stored.designlhs$digitsVar)
  digitsEntry <- tkentry(baseFrame, width="8", textvariable=digitsVar)
  digitsHint <- ttklabel(baseFrame, text="(integer number)", foreground="#888888")

  ## radio buttons for choosing design type
  seedVar <- tclVar(sample(31999,1))  ## always new
  seedEntry <- tkentry(baseFrame, width="8", textvariable=seedVar)
  seedHint <- tklabel(baseFrame, text="(You normally do not need to change this)", foreground="#888888")
  tkconfigure(seedEntry, takefocus=0)

  ## preparations for bottom frame
  bottomFrame <- tkframe(topdes2)

  ## grid base frame
  tkgrid(nrunlab <- tklabel(baseFrame, text=gettextRcmdr("Number of runs")), nrunEntry, nrunHint, sticky="w")
  ## omitted nfaccb, on form, nfactors must always be specified
  tkgrid(nfaclab <- tklabel(baseFrame, text=gettextRcmdr("Number of factors")), nfacEntry, nfacHint, sticky="w")
  tkgrid(digitslab <- tklabel(baseFrame, text=gettextRcmdr("Number of decimal places")),
         digitsEntry, digitsHint, sticky="w", pady="15")
  tkgrid(seedlab <- tklabel(baseFrame, text=gettextRcmdr("Seed for randomization")), seedEntry, sticky="w")
  tkgrid(seedHint, sticky="w")

  helptab1Button <- buttonRcmdr(nameFrame, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab1,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab1Button, takefocus=0)

  ### Finalize tab1
  tkgrid(tklabel(nameFrame, text="Name of new design"), nameEntry, helptab1Button, sticky="w")
  tkgrid(nameFrame, sticky="w", columnspan=4)
  tkgrid.configure(nameFrame, pady=40)
  tkgrid.configure(helptab1Button, sticky="ne")
  tkgrid(typeradioFrame, baseFrame, sticky="nw",columnspan=3)
  tkgrid.configure(baseFrame, padx=10)

  ## Factor Details Tab
  ## factor details frame
  ### facnameAutoVariable (not needed any more) and faclevelCommonVariable

  ## default levels frame
  deflevFrame <- ttklabelframe(tab2,text="Default levels (lower and upper scale ends)")
  facnameAutoVariable <- tclVar(.stored.designlhs$cbInitials[3])
  faclevelCommonVariable <- tclVar(.stored.designlhs$cbInitials[4])
  faclevelCommonButton <- ttkcheckbutton(deflevFrame,text=gettextRcmdr("Common factor levels"),
                                         variable=faclevelCommonVariable,command=onRefresh)
  tkconfigure(faclevelCommonButton,takefocus=0)
  putRcmdr("level1Var", tclVar(.stored.designlhs$level1Var))
  level1Entry <- ttkentry(deflevFrame, width="20", textvariable=level1Var)
  tkconfigure(level1Entry,takefocus=0)
  tkbind(level1Entry, "<FocusIn>", level1enter)
  tkbind(level1Entry, "<FocusOut>", level1change)
  tkconfigure(level1Entry,takefocus=0)
  putRcmdr("level2Var", tclVar(.stored.designlhs$level2Var))
  level2Entry <- tkentry(deflevFrame, width="20", textvariable=level2Var)
  tkconfigure(level2Entry,takefocus=0)
  tkbind(level2Entry, "<FocusIn>", level2enter)
  tkbind(level2Entry, "<FocusOut>", level2change)
  tkgrid(faclevelCommonButton,sticky="w",columnspan=3)
  faclevCommonLab<-tklabel(deflevFrame,text=gettextRcmdr("CAUTION: Checking this box overwrites all custom factor levels."))
  if (!as.logical(as.numeric(tclvalue(faclevelCommonVariable)))){
    tkgrid(faclevCommonLab,sticky="w", columnspan=3)
    tkgrid.configure(faclevCommonLab,pady=10)
  }
  tkgrid(tklabel(deflevFrame, text=gettextRcmdr("Lower scale end")),tklabel(deflevFrame,text="  ",width=2),tklabel(deflevFrame, text=gettextRcmdr("Upper scale end")),sticky="e")
  tkgrid(level1Entry, tklabel(deflevFrame,text="  ",width=2),level2Entry, sticky="e")

  ## factor details
  ## values as vectors
  facnamlistt <- .stored.designlhs$facnamlist
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))) {
    faclev1listt <- rep(tclvalue(level1Var),tclvalue(nfacVar))
    faclev2listt <- rep(tclvalue(level2Var),tclvalue(nfacVar))
  } else{
    faclev1listt <- .stored.designlhs$faclev1list
    faclev2listt <- .stored.designlhs$faclev2list
  }
  faclablistt <- .stored.designlhs$faclablist
  varlistshortt <- if (as.numeric(tclvalue(nfacVar))<=50)
    Letters[1:tclvalue(nfacVar)] else paste("F",1:tclvalue(nfacVar),sep="")

  enterlistFrame <- ttkframe(tab2)
  listFrame <- ttklabelframe(enterlistFrame, text="Factor Details")
  putRcmdr("selpos", 1)
  putRcmdr("curfac", tclVar(varlistshortt[1]))
  putRcmdr("curfnam", tclVar(facnamlistt[1]))
  putRcmdr("curflev1", tclVar(faclev1listt[1]))
  putRcmdr("curflev2", tclVar(faclev2listt[1]))
  putRcmdr("curflab", tclVar(faclablistt[1]))

  ## fsel must select the right factor
  ## this should be highlighted in factor lists
  ##    and all related entries shown for changing in text boxes fnam etc.
  enterFrame <- ttklabelframe(enterlistFrame, text=gettextRcmdr("Modify factor details for selected factor"))
  fsel <- ttkcombobox(enterFrame, textvariable=curfac, width=5, values=varlistshortt, state="readonly")
  tkbind(fsel, "<<ComboboxSelected>>", factorsel)
  #fnam <- ttkentry(listFrame, textvariable=curfnam, width=20,validate="focusout", validatecommand=fnamchange)
  fnam <- ttkentry(enterFrame, textvariable=curfnam, width=15)
  tkbind(fnam, "<FocusOut>", fnamchange)
  flev1 <- ttkentry(enterFrame, textvariable=curflev1, width=15)
  tkbind(flev1, "<FocusOut>", flev1change)
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))){
    tkconfigure(flev1,state="disabled")
  }
  flev2 <- ttkentry(enterFrame, textvariable=curflev2, width=15)
  tkbind(flev2, "<FocusOut>", flev2change)
  if (as.logical(as.numeric(tclvalue(faclevelCommonVariable)))){
    tkconfigure(flev2,state="disabled")
  }
  flab <- ttkentry(enterFrame, textvariable=curflab, width=20)
  tkbind(flab, "<FocusOut>", flabchange)
  tkbind(flab, "<Key-Tab>", tabflab)
  tkgrid(tklabel(enterFrame,text=gettextRcmdr("Select"),width=6),
         tklabel(enterFrame,text=gettextRcmdr("Factor name"), width=15),
         tklabel(enterFrame,text=gettextRcmdr("First level"), width=15),
         tklabel(enterFrame,text=gettextRcmdr("Second level"), width=15),
         tklabel(enterFrame,text=gettextRcmdr("Comment or label \n(for html export only)"), width=20),
         sticky="w")
  tkgrid(fsel,fnam, flev1, flev2, flab, sticky="w")

  putRcmdr("facnamlist", tclVar(facnamlistt))
  putRcmdr("varlistshort", tclVar(varlistshortt))
  putRcmdr("faclev1list", tclVar(faclev1listt))
  putRcmdr("faclev2list", tclVar(faclev2listt))
  putRcmdr("faclablist", tclVar(faclablistt))

  facshortListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                               selectmode = single, exportselection = "TRUE", listvariable=varlistshort,
                               width = 6, background="#EBEBDC")
  tkbind(facshortListBox, "<<TraverseIn>>",function() tkfocus(fsel))

  facnameListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=facnamlist,
                              width = 15, background="#EBEBDC")
  faclev1ListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=faclev1list,
                              width = 15, background="#EBEBDC")
  faclev2ListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                              selectmode = single, exportselection = "TRUE", listvariable=faclev2list,
                              width = 15, background="#EBEBDC")
  faclabListBox <- tklistbox(listFrame, height = min(10, as.numeric(tclvalue(nfacVar))),
                             selectmode = single, exportselection = "TRUE", listvariable=faclablist,
                             width = 20, background="#EBEBDC")

  ## determine current index and reordering for onUp and onDown
  tkbind(facshortListBox, "<<ListboxSelect>>", checkIndexShort)
  tkbind(facnameListBox, "<<ListboxSelect>>", checkIndexNam)
  tkbind(faclev1ListBox, "<<ListboxSelect>>", checkIndexLev1)
  tkbind(faclev2ListBox, "<<ListboxSelect>>", checkIndexLev2)
  tkbind(faclabListBox, "<<ListboxSelect>>", checkIndexLab)


  ### funktioniert, ist aber noch nicht schön
  scrollbar <- ttkscrollbar(listFrame, command = function(...) {
    tkyview(facshortListBox, ...)
    tkyview(facnameListBox, ...)
    tkyview(faclev1ListBox, ...)
    tkyview(faclev2ListBox, ...)
    tkyview(faclabListBox, ...)
  })

  #    tkgrid(tklabel(enterlistFrame,text="  ", width=5),enterFrame, sticky="w")
  tkgrid(enterFrame, sticky="w", columnspan=5)
  tkgrid.configure(enterFrame, pady=10)
  ## Hoch-/Runterschieben von Einträgen ermöglichen

  downupFrame <- ttkframe(listFrame)
  moveDownButton <- buttonRcmdr(downupFrame, text = gettextRcmdr("Move Down"),
                                foreground = "darkgreen", command = onDown,
                                default = "normal", borderwidth = 3, width=12)
  moveUpButton <- buttonRcmdr(downupFrame, text = gettextRcmdr("Move Up"),
                              foreground = "darkgreen", command = onUp,
                              default = "normal", borderwidth = 3, width=12)
  tkgrid(moveDownButton, sticky="w")
  tkgrid(moveUpButton, sticky="w")

  tkgrid(scrollbar, facshortListBox, facnameListBox, faclev1ListBox, faclev2ListBox, faclabListBox, downupFrame, sticky = "nw")
  tkgrid.configure(scrollbar, sticky = "wns")
  tkgrid.configure(facnameListBox, sticky = "new")

  helptab2Button <- buttonRcmdr(tab2, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab2,
                                default = "normal", borderwidth = 3)
  tkconfigure(helptab2Button, takefocus=0)

  ## finalize tab2 Factor details
  tkgrid(helptab2Button, sticky="e")
  tkgrid(deflevFrame, sticky="nw")
  tkgrid.configure(deflevFrame, pady=10)
  tkgrid(listFrame, columnspan=6,sticky="w")
  tkgrid(enterlistFrame, columnspan=6,sticky="w")

  ## tab6 for exporting
  helptab6Button <- buttonRcmdr(tab6, text = gettextRcmdr("Tab Help"),
                                foreground = "darkgreen", command = onHelpTab6,
                                default = "normal", borderwidth = 3)

  exportlabVar <- nameVar
  exportlab <- ttklabel(tab6, textvariable=exportlabVar)
  tkgrid(ttklabel(tab6,text="Current design to be saved:"),exportlab,helptab6Button,sticky="w")
  tkgrid.configure(exportlab, pady=15)
  tkgrid.configure(helptab6Button, sticky="ne")

  ## radio buttons for choosing export type
  etradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("(How to) Export ?"))
  etyperbVariable <- tclVar(.stored.designlhs$etyperbVariable)
  noexprb <- tkradiobutton(etradioFrame,text=gettextRcmdr("no export"),variable=etyperbVariable,value="none")
  allrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("all file types"),variable=etyperbVariable,value="all")
  rdarb <- tkradiobutton(etradioFrame,text=gettextRcmdr("rda only"),variable=etyperbVariable,value="rda")
  htmlrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("html and rda"),variable=etyperbVariable,value="html")
  csvrb <- tkradiobutton(etradioFrame,text=gettextRcmdr("csv and rda"),variable=etyperbVariable,value="csv")
  tkgrid(noexprb, sticky="w")
  tkgrid(allrb, sticky="w")
  tkgrid(rdarb, sticky="w")
  tkgrid(htmlrb, sticky="w")
  tkgrid(csvrb, sticky="w")

  ## radio buttons for choosing export decimal separator
  decimalradioFrame <- ttklabelframe(tab6, text=gettextRcmdr("Decimal Separator ?"))
  decimalrbVariable <- tclVar(.stored.designlhs$decimalrbVariable)
  defaultrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("default"),variable=decimalrbVariable, value="default")
  pointrb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr("."),variable=decimalrbVariable, value=".")
  commarb <- tkradiobutton(decimalradioFrame,text=gettextRcmdr(","),variable=decimalrbVariable, value=",")
  tkgrid(defaultrb, sticky="w")  ## in this case, leave default option from options
  tkgrid(pointrb, sticky="w")
  tkgrid(commarb, sticky="w")

  ## export directory
  dirFrame <- ttklabelframe(tab6, text=gettextRcmdr("Storage Directory"))
  putRcmdr("dirVar", tclVar(.stored.designlhs$dirVar))
  dirEntry <- tkentry(dirFrame, width="50", textvariable=dirVar)
  dirButton <- buttonRcmdr(dirFrame, text = gettextRcmdr("Change directory"),
                           foreground = "darkgreen", width = "20", command = onChangeDir,
                           default = "normal", borderwidth = 3)
  tkgrid(dirEntry, tklabel(dirFrame, text="   "), dirButton, sticky="w")

  ## export file name
  putRcmdr("fileVar", tclVar(.stored.designlhs$fileVar))
  fileEntry <- tkentry(tab6, width="20", textvariable=fileVar)
  efnamelabel <- tklabel(tab6,text=gettextRcmdr("Export file names: name below with appropriate endings (html or csv, and rda)"))
  replacecbVariable <- tclVar(.stored.designlhs$cbInitials[8])
  replacecb <- ttkcheckbutton(tab6,text=gettextRcmdr("Replace file(s), if exists"),variable=replacecbVariable)

  ## always grid details, as otherwise default file name does not work
  ## design name info and help button have already been gridded above
  tkgrid(etradioFrame, decimalradioFrame, sticky="nw")
  tkgrid(dirFrame, sticky="w", columnspan=5)
  tkgrid.configure(dirFrame, pady=15)
  tkgrid(efnamelabel, sticky="w", columnspan=5)
  tkgrid(fileEntry, sticky="w", columnspan=5)
  tkgrid(replacecb, sticky="w", columnspan=5)


  ## add buttons outside the notebook
  buttonFrame <- tkframe(topdes2)
  ## die sind aber nicht dunkelgruen ...
  refreshButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Refresh form"),
                               foreground = "darkgreen", width = "12", command = onRefresh,
                               default = "normal", borderwidth = 3)
  storeButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Store form"),
                             foreground = "darkgreen", width = "12", command = onStore,
                             default = "normal", borderwidth = 3)
  loadButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Load form"),
                            foreground = "darkgreen", width = "12", command = onLoad,
                            default = "normal", borderwidth = 3)
  resetButton <- buttonRcmdr(buttonFrame, text = gettextRcmdr("Reset form"),
                             foreground = "darkgreen", width = "12", command = onReset,
                             default = "normal", borderwidth = 3)
  #        tkgrid(refreshButton,sticky="w")
  #        tkgrid(tklabel(buttonFrame,text="  "),sticky="w")
  tkgrid(storeButton,sticky="w")
  tkgrid(loadButton,sticky="w")
  tkgrid(resetButton,sticky="w")

  tkconfigure(refreshButton, takefocus=0)
  tkconfigure(storeButton, takefocus=0)
  tkconfigure(loadButton, takefocus=0)
  tkconfigure(resetButton, takefocus=0)

  ## storage buttons to the right of the notebook
  tkgrid(tn, buttonFrame, sticky="w", columnspan=2)

  OKCancelHelp(window=topdes2, helpSubject="Menu.lhs")
  tkconfigure(OKbutton, takefocus=0)
  tkconfigure(cancelButton, takefocus=0)
  tkconfigure(helpButton, takefocus=0)

  tkgrid(buttonsFrame, bottomFrame, sticky="ew")


  ### relations among widgets
  if (exists("activestab.tn", where="RcmdrEnv")){
    tcl(tn, "select", activestab.tn)
    rm(activestab.tn, pos="RcmdrEnv")
  }

  dialogSuffix(window=topdes2, rows=2, columns=2, focus=tn, bindReturn=FALSE)

}
