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
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.confidenceLevel=".95", initial.posthoc="hsd", initial.tab=0, initial.label = NULL)
  dialog.values <- getDialog("multiWayAnova", defaults)
  initializeDialog(title = gettextRcmdr("Multi-Way Analysis of Variance"), use.tabs = TRUE)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  dataFrame <- tkframe(dataTab)
  groupBox <- variableListBox(dataFrame, Factors(), selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))
  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))
    tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
    groups <- getSelection(groupBox)
    if (length(groups) == 0) {
      errorCondition(recall = multiWayAnova, message = gettextRcmdr("You must select at least one factor."))
      return()
    }
    response <- getSelection(responseBox)
    if (length(response) == 0) {
      errorCondition(recall = multiWayAnova, message = gettextRcmdr("You must select a response variable."))
      return()
    }

    level<-tclvalue(confidenceLevel)
    posthoc <- as.character(tclvalue(posthocVariable))

    if (!is.valid.name(modelValue)) {
      UpdateModelNumber(-1)
      errorCondition(recall = multiWayAnova, message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),
                                                               modelValue))
      return()
    }
    if (is.element(modelValue, listAOVModels())) {
      if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        UpdateModelNumber(-1)
        tkdestroy(dataTab)
        multiWayAnova()
        return()
      }
    }
    putDialog ("multiWayAnova", list (initial.group = groups, initial.response = response, initial.confidenceLevel = level, initial.posthoc = posthoc, initial.tab = tab,
                                      initial.label = .groupsLabel))
    closeDialog()

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
    doItAndPrint(paste("mean.diff(", response, " ~ ", paste(groups, collapse = "*"), ", method=", '"',posthoc,'"',")", sep = ""))
    doItAndPrint(paste("xtabs(~ ", paste(groups, collapse=" + "), ", data=", .activeDataSet, ") # counts", sep=""))
    putRcmdr(modelValue)
    putRcmdr("modelWithSubset", FALSE)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "multiWayAnova", apply = "multiWayAnova")
  optionsFrame <- tkframe(optionsTab)
  radioButtons(optionsFrame, name = "posthoc", buttons = c("hsd","lsd","bonferroni","duncan","scheffe","newmankeuls"), values = c("hsd","lsd","bonferroni","duncan","scheffe","newmankeuls"),
               labels = gettextRcmdr(c("Tukey's HSD", "Fisher's LSD", "Bonferroni", "Duncan", "Scheffe", "Newmankeuls")), title = gettextRcmdr("PostHoc Test"),
               initialValue = dialog.values$initial.posthoc)

  hsd <- c("hsd")
  lsd <- c("lsd")
  bonferroni <- c("bonferroni")
  duncan <- c("duncan")
  scheffe <- c("scheffe")
  newmankeuls <- c("newmankeuls")

  confidenceFrame <- tkframe(optionsFrame)
  confidenceLevel <- tclVar(dialog.values$initial.confidenceLevel)
  confidenceField <- ttkentry(confidenceFrame, width = "6", textvariable = confidenceLevel)

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(labelRcmdr(confidenceFrame, text = gettextRcmdr("Confidence Level"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(confidenceField, sticky="w")
  groupsLabel(optionsTab, groupsBox = groupBox, initialText = dialog.values$initial.label)
  tkgrid(posthocFrame, labelRcmdr(optionsFrame, text = " "),
         confidenceFrame, labelRcmdr(optionsFrame, text = " "), sticky = "nw")
  tkgrid(optionsFrame, sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = TRUE, grid.buttons = TRUE)
}

onerandomtable <- function () {
  defaults <- list(initial.factorname = 'c("a","b","c")', initial.repetition = "", initial.seed="0", initial.std="FALSE")
  dialog.values <- getDialog("onerandomtable", defaults)
  initializeDialog(title = gettextRcmdr("Random Number Table of One Way"), use.tabs = FALSE)
  mainFrame <- tkframe(top)

  onOK <- function() {

    fac<-as.character(tclvalue(factorname))
    repet<-tclvalue(repetition)
    sed<-tclvalue(seed)
    sttd<-as.character(tclvalue(std))

    putDialog ("onerandomtable", list (initial.factorname = fac, initial.repetition = repet, initial.seed = sed, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("random.one(", "fac=", fac,",", "r=", repet,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "multiWayAnova", apply = "multiWayAnova")

  factorFrame <- tkframe(mainFrame)
  factorname <- tclVar(dialog.values$initial.factorname)
  factorField <- ttkentry(factorFrame, width = "12", textvariable = factorname)

  repetFrame <- tkframe(mainFrame)
  repetition <- tclVar(dialog.values$initial.repetition)
  repetField <- ttkentry(repetFrame, width = "12", textvariable = repetition)

  seedFrame <- tkframe(mainFrame)
  seed <- tclVar(dialog.values$initial.seed)
  seedField <- ttkentry(seedFrame, width = "12", textvariable = seed)

  stdFrame <- tkframe(mainFrame)
  std <- tclVar(dialog.values$initial.std)
  stdField <- ttkentry(stdFrame, width = "12", textvariable = std)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(factorFrame, text = gettextRcmdr("Factor/input factor's name in quotation"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("Repet"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("Std/TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(factorField, sticky="w")
  tkgrid(repetField, sticky="w")
  tkgrid(seedField, sticky="w")
  tkgrid(stdField, sticky="w")
  tkgrid(factorFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(repetFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(seedFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(stdFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

latinrandomtable <- function () {
  defaults <- list(initial.factornumber = "3", initial.greco = "FALSE", initial.seed="0", initial.std="FALSE")
  dialog.values <- getDialog("latinrandomtable", defaults)
  initializeDialog(title = gettextRcmdr("Random Number Table of Latin Square"), use.tabs = FALSE)
  mainFrame <- tkframe(top)

  onOK <- function() {

    n<-tclvalue(factornumber)
    greco<-as.character(tclvalue(greco))
    sed<-tclvalue(seed)
    sttd<-as.character(tclvalue(std))

    putDialog ("grecorandomtable", list (initial.factornumber = n, initial.greco = greco, initial.seed = sed, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("random.latin(", "n=", n,",", "greco=", greco,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "multiWayAnova", apply = "multiWayAnova")

  factorFrame <- tkframe(mainFrame)
  factornumber <- tclVar(dialog.values$initial.factornumber)
  factorField <- ttkentry(factorFrame, width = "12", textvariable = factornumber)

  grecoFrame <- tkframe(mainFrame)
  greco <- tclVar(dialog.values$initial.greco)
  grecoField <- ttkentry(grecoFrame, width = "12", textvariable = greco)

  seedFrame <- tkframe(mainFrame)
  seed <- tclVar(dialog.values$initial.seed)
  seedField <- ttkentry(seedFrame, width = "12", textvariable = seed)

  stdFrame <- tkframe(mainFrame)
  std <- tclVar(dialog.values$initial.std)
  stdField <- ttkentry(stdFrame, width = "12", textvariable = std)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(factorFrame, text = gettextRcmdr("Number of Factor / N*N latin square"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(grecoFrame, text = gettextRcmdr("Latin or Greco / TRUE = Greco, FALSE = Latin"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("Std/TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(factorField, sticky="w")
  tkgrid(grecoField, sticky="w")
  tkgrid(seedField, sticky="w")
  tkgrid(stdField, sticky="w")
  tkgrid(factorFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(grecoFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(seedFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(stdFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

multirandomtable <- function () {
  defaults <- list(initial.factornumber = "c(2,3)", initial.repetition = "", initial.seed="0", initial.std="FALSE")
  dialog.values <- getDialog("multirandomtable", defaults)
  initializeDialog(title = gettextRcmdr("Random Number Table of Multi Way"), use.tabs = FALSE)
  mainFrame <- tkframe(top)

  onOK <- function() {

    fac<-as.character(tclvalue(factornumber))
    repet<-tclvalue(repetition)
    sed<-tclvalue(seed)
    sttd<-as.character(tclvalue(std))

    putDialog ("onerandomtable", list (initial.factornumber = fac, initial.repetition = repet, initial.seed = sed, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("random.multi(", "fac=", fac,",", "r=", repet,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "multiWayAnova", apply = "multiWayAnova")

  factorFrame <- tkframe(mainFrame)
  factornumber <- tclVar(dialog.values$initial.factornumber)
  factorField <- ttkentry(factorFrame, width = "12", textvariable = factornumber)

  repetFrame <- tkframe(mainFrame)
  repetition <- tclVar(dialog.values$initial.repetition)
  repetField <- ttkentry(repetFrame, width = "12", textvariable = repetition)

  seedFrame <- tkframe(mainFrame)
  seed <- tclVar(dialog.values$initial.seed)
  seedField <- ttkentry(seedFrame, width = "12", textvariable = seed)

  stdFrame <- tkframe(mainFrame)
  std <- tclVar(dialog.values$initial.std)
  stdField <- ttkentry(stdFrame, width = "12", textvariable = std)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(factorFrame, text = gettextRcmdr("Factor/input Each factor's Level"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("Repet"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("Std/TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(factorField, sticky="w")
  tkgrid(repetField, sticky="w")
  tkgrid(seedField, sticky="w")
  tkgrid(stdField, sticky="w")
  tkgrid(factorFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(repetFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(seedFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(stdFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

mainplot <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL)
  dialog.values <- getDialog("mainplot", defaults)
  initializeDialog(title = gettextRcmdr("Main Effects Plot"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), selectmode = "single",
                              title = gettextRcmdr("Factors (pick one)"),
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    if (length(groups) == 0) {
      errorCondition(recall = mainplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("mainplot", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste("main.effect(", .activeDataSet,',',groups,',',response, ")"))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "mainplot", apply = "mainplot")

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

interactionplot <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL)
  dialog.values <- getDialog("interactionplot", defaults)
  initializeDialog(title = gettextRcmdr("Interaction Plot"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick two)"),
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    if (length(groups) == 0) {
      errorCondition(recall = mainplot, message = gettextRcmdr("You must select two factor."))
      return()}
    if (length(groups) == 1) {
      errorCondition(recall = mainplot, message = gettextRcmdr("You must select two factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = mainplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("interactionplot", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste("interaction.effect(", .activeDataSet,',',groups.list,',',response, ")"))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "interactionplot", apply = "interactionplot")

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

scatterplot <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL)
  dialog.values <- getDialog("scatterplot", defaults)
  initializeDialog(title = gettextRcmdr("Scatter Plot"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)

    if (length(groups) == 0) {
      errorCondition(recall = mainplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = mainplot, message = gettextRcmdr("You must select at least one factor."))
      return()}

    putDialog ("scatterplot", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste("scatter.plot(", .activeDataSet,",",groups.list,",",response, ")"))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "scatterplot", apply = "scatterplot")

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

