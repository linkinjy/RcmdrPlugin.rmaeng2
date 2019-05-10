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
      errorCondition(recall = interactionplot, message = gettextRcmdr("You must select two factor."))
      return()}
    if (length(groups) == 1) {
      errorCondition(recall = interactionplot, message = gettextRcmdr("You must select two factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = interactionlot, message = gettextRcmdr("You must select at least one factor."))
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

sscatterplot <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL)
  dialog.values <- getDialog("sscatterplot", defaults)
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
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("sscatterplot", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste("scatter.plot(", .activeDataSet,",",groups.list,",",response, ")"))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "sscatterplot", apply = "sscatterplot")

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}
