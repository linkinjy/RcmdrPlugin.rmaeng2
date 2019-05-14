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
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("Repeat"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
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
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("Repeat"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
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

oneerror <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL,initial.alpha="0.05")
  dialog.values <- getDialog("oneerror", defaults)
  initializeDialog(title = gettextRcmdr("Estimate of One Way's Error Variance"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), selectmode = "single",
                              title = gettextRcmdr("Factors (pick one)"),
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))

  onOK <- function() {

    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    al <- tclvalue(alpha)

    if (length(groups) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}

    putDialog ("oneerror", list (initial.group = groups, initial.response = response, initial.alpha = al))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste("error.var(", response,"~",groups.list,",",.activeDataSet,",",al, ")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "oneerror", apply = "oneerror")

  alphaFrame <- tkframe(dataFrame)
  alpha <- tclVar(dialog.values$initial.alpha)
  alphaField <- ttkentry(alphaFrame, width = "12", textvariable = alpha)

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(labelRcmdr(alphaFrame, text = gettextRcmdr("Alpha"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(alphaField, sticky="w")
  tkgrid(alphaFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

usermeandiff <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL,initial.method="hsd")
  dialog.values <- getDialog("usermeandiff", defaults)
  initializeDialog(title = gettextRcmdr("Population Mean Difference : Factor and Interaction (User's Setting)"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    methodd <- as.character(tclvalue(method))

    if (length(groups) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}

    putDialog ("usermeandiff", list (initial.group = groups, initial.response = response, initial.method = methodd))
    closeDialog()

    groups.list <- paste(paste(groups, sep = ""), collapse = "+")
    doItAndPrint(paste("mean.diff(", response,"~",groups.list,",","method=",'"',methodd,'"',")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "usermeandiff", apply = "usermeandiff")

  methodFrame <- tkframe(dataFrame)
  method <- tclVar(dialog.values$initial.method)
  methodField <- ttkentry(methodFrame, width = "12", textvariable = method)

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(labelRcmdr(methodFrame, text = gettextRcmdr("Method
(input : hsd / lsd / bonferroni / scheffe / newmankeuls / duncan)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(methodField, sticky="w")
  tkgrid(methodFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

meandiff <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL,initial.method="hsd")
  dialog.values <- getDialog("meandiff", defaults)
  initializeDialog(title = gettextRcmdr("Population Mean Difference : All Factor and Interaction"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    methodd <- as.character(tclvalue(method))

    if (length(groups) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}

    putDialog ("meandiff", list (initial.group = groups, initial.response = response, initial.method = methodd))
    closeDialog()

    groups.list <- paste(paste(groups, sep = ""), collapse = "*")
    doItAndPrint(paste("mean.diff(", response,"~",groups.list,",","method=",'"',methodd,'"',")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "meandiff", apply = "meandiff")

  methodFrame <- tkframe(dataFrame)
  method <- tclVar(dialog.values$initial.method)
  methodField <- ttkentry(methodFrame, width = "12", textvariable = method)

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(labelRcmdr(methodFrame, text = gettextRcmdr("Method
(input : hsd / lsd / bonferroni / scheffe / newmankeuls / duncan)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(methodField, sticky="w")
  tkgrid(methodFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

facrandomtable <- function () {
  defaults <- list(initial.level = "c(3,3,2)", initial.nvars = "3", initial.repet="2", initial.std="FALSE")
  dialog.values <- getDialog("facrandomtable", defaults)
  initializeDialog(title = gettextRcmdr("Random Number Table of Factorial Design"), use.tabs = FALSE)
  mainFrame <- tkframe(top)

  onOK <- function() {

    levell<-as.character(tclvalue(level))
    nvar<-tclvalue(nvars)
    repe<-tclvalue(repet)
    sttd<-as.character(tclvalue(std))

    putDialog ("facrandomtable", list (initial.level = levell, initial.nvars = nvar, initial.repet = repe, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("fac.design(", "levels=", levell,",", "nVars=", nvar,",", "r=", repe,",","std=", sttd, ")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "facrandomtable", apply = "facrandomtable")

  levelFrame <- tkframe(mainFrame)
  level <- tclVar(dialog.values$initial.level)
  levelField <- ttkentry(levelFrame, width = "12", textvariable = level)

  nvarsFrame <- tkframe(mainFrame)
  nvars <- tclVar(dialog.values$initial.nvars)
  nvarsField <- ttkentry(nvarsFrame, width = "12", textvariable = nvars)

  repetFrame <- tkframe(mainFrame)
  repet <- tclVar(dialog.values$initial.repet)
  repetField <- ttkentry(repetFrame, width = "12", textvariable = repet)

  stdFrame <- tkframe(mainFrame)
  std <- tclVar(dialog.values$initial.std)
  stdField <- ttkentry(stdFrame, width = "12", textvariable = std)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(levelFrame, text = gettextRcmdr("Each Factor's level / c(level1,level2,...)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(nvarsFrame, text = gettextRcmdr("Number of Factors"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("Repeat"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("Std / TRUE : standard order or FALSE : run order"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(levelField, sticky="w")
  tkgrid(nvarsField, sticky="w")
  tkgrid(repetField, sticky="w")
  tkgrid(stdField, sticky="w")
  tkgrid(levelFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(nvarsFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(repetFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(stdFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

boxbenken <- function () {
  defaults <- list(initial.kfactor = "3", initial.center = 4, initial.random="TRUE")
  dialog.values <- getDialog("boxbenken", defaults)
  initializeDialog(title = gettextRcmdr("Design of Box Benken"), use.tabs = FALSE)
  mainFrame <- tkframe(top)

  onOK <- function() {

    kfactors<-tclvalue(kfactor)
    centers<-tclvalue(center)
    randoms<-as.character(tclvalue(random))

    putDialog ("boxbenken", list (initial.kfactor = kfactors, initial.center = centers, initial.random = randoms))
    closeDialog()

    doItAndPrint(paste("kbbd(", "k=", kfactors,",", "n0=", centers,",","randomize=", randoms, ")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "boxbenken", apply = "boxbenken")

  kfactorFrame <- tkframe(mainFrame)
  kfactor <- tclVar(dialog.values$initial.kfactor)
  kfactorField <- ttkentry(kfactorFrame, width = "12", textvariable = kfactor)

  centerFrame <- tkframe(mainFrame)
  center <- tclVar(dialog.values$initial.center)
  centerField <- ttkentry(centerFrame, width = "12", textvariable = center)

  randomFrame <- tkframe(mainFrame)
  random <- tclVar(dialog.values$initial.random)
  randomField <- ttkentry(randomFrame, width = "12", textvariable = random)

  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(kfactorFrame, text = gettextRcmdr("Number of Factors"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(centerFrame, text = gettextRcmdr("Number of center point"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomFrame, text = gettextRcmdr("Randomize / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(kfactorField, sticky="w")
  tkgrid(centerField, sticky="w")
  tkgrid(randomField, sticky="w")
  tkgrid(kfactorFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(centerFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(randomFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

central <- function () {
  defaults <- list(initial.basis = "3", initial.center = "4", initial.wbreps="1", initial.bbreps="1", initial.random="TRUE", initial.oneblock="FALSE")
  dialog.values <- getDialog("central", defaults)
  initializeDialog(title = gettextRcmdr("Design of Central Composite"), use.tabs = FALSE)
  mainFrame <- tkframe(top)

  onOK <- function() {

    basiss<-tclvalue(basis)
    centers<-tclvalue(center)
    wbrep<-tclvalue(wbreps)
    bbrep<-tclvalue(bbreps)
    randoms<-as.character(tclvalue(random))
    oneblocks<-as.character(tclvalue(oneblock))

    putDialog ("central", list (initial.basis = basiss, initial.center = centers, initial.wbreps = wbrep, initial.bbreps = bbrep,initial.random = randoms, initial.oneblock = oneblocks))
    closeDialog()

    doItAndPrint(paste("kccd(", "basis=", basiss,",", "n0=", centers,",","wbreps=",wbrep,",","bbreps=",bbrep,",","randomize=", randoms,"," ,"oneblock=",oneblocks,")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "central", apply = "central")

  basisFrame <- tkframe(mainFrame)
  basis <- tclVar(dialog.values$initial.basis)
  basisField <- ttkentry(basisFrame, width = "12", textvariable = basis)

  centerFrame <- tkframe(mainFrame)
  center <- tclVar(dialog.values$initial.center)
  centerField <- ttkentry(centerFrame, width = "12", textvariable = center)

  wbrepsFrame <- tkframe(mainFrame)
  wbreps <- tclVar(dialog.values$initial.wbreps)
  wbrepsField <- ttkentry(wbrepsFrame, width = "12", textvariable = wbreps)

  bbrepsFrame <- tkframe(mainFrame)
  bbreps <- tclVar(dialog.values$initial.bbreps)
  bbrepsField <- ttkentry(bbrepsFrame, width = "12", textvariable = bbreps)

  randomFrame <- tkframe(mainFrame)
  random <- tclVar(dialog.values$initial.random)
  randomField <- ttkentry(randomFrame, width = "12", textvariable = random)

  oneblockFrame <- tkframe(mainFrame)
  oneblock <- tclVar(dialog.values$initial.oneblock)
  oneblockField <- ttkentry(oneblockFrame, width = "12", textvariable = oneblock)


  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(basisFrame, text = gettextRcmdr("Number of Factors"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(centerFrame, text = gettextRcmdr("Number of Center Point"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(wbrepsFrame, text = gettextRcmdr("Number of Within Block Replication"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(bbrepsFrame, text = gettextRcmdr("Number of Between Block Replication"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomFrame, text = gettextRcmdr("Randomize / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(oneblockFrame, text = gettextRcmdr("Oneblock / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(basisField, sticky="w")
  tkgrid(centerField, sticky="w")
  tkgrid(wbrepsField, sticky="w")
  tkgrid(bbrepsField, sticky="w")
  tkgrid(randomField, sticky="w")
  tkgrid(oneblockField, sticky="w")
  tkgrid(basisFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(centerFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(wbrepsFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(bbrepsFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(randomFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(oneblockFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

designsplit <- function () {
  defaults <- list(initial.factor1 = "c(1,2,3)", initial.factor2 = "c(1,2,3)", initial.repet="1", initial.seed="0", initial.random="TRUE", initial.std="FALSE")
  dialog.values <- getDialog("designsplit", defaults)
  initializeDialog(title = gettextRcmdr("Random Number Table of Split Method"), use.tabs = FALSE)
  mainFrame <- tkframe(top)

  onOK <- function() {

    factor11<-as.character(tclvalue(factor1))
    factor22<-as.character(tclvalue(factor2))
    repe<-tclvalue(repet)
    sed<-tclvalue(seed)
    randoms<-as.character(tclvalue(random))
    sttd<-as.character(tclvalue(std))

    putDialog ("designsplit", list (initial.factor1 = factor11, initial.factor2 = factor22, initial.repet = repe, initial.seed = sed,initial.random = randoms, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("design.split(", factor11,",", factor22,",","r=",repe,",","seed=",sed,",","randomization=", randoms,"," ,"std=",sttd,")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "designsplit", apply = "designsplit")

  factor1Frame <- tkframe(mainFrame)
  factor1 <- tclVar(dialog.values$initial.factor1)
  factor1Field <- ttkentry(factor1Frame, width = "12", textvariable = factor1)

  factor2Frame <- tkframe(mainFrame)
  factor2 <- tclVar(dialog.values$initial.factor2)
  factor2Field <- ttkentry(factor2Frame, width = "12", textvariable = factor2)

  repetFrame <- tkframe(mainFrame)
  repet <- tclVar(dialog.values$initial.repet)
  repetField <- ttkentry(repetFrame, width = "12", textvariable = repet)

  seedFrame <- tkframe(mainFrame)
  seed <- tclVar(dialog.values$initial.seed)
  seedField <- ttkentry(seedFrame, width = "12", textvariable = seed)

  randomFrame <- tkframe(mainFrame)
  random <- tclVar(dialog.values$initial.random)
  randomField <- ttkentry(randomFrame, width = "12", textvariable = random)

  stdFrame <- tkframe(mainFrame)
  std <- tclVar(dialog.values$initial.std)
  stdField <- ttkentry(stdFrame, width = "12", textvariable = std)


  tkgrid(labelRcmdr(mainFrame, text="  "), sticky = "nw")
  tkgrid(mainFrame, sticky="nw")
  tkgrid(labelRcmdr(factor1Frame, text = gettextRcmdr("Level of Factor1"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(factor2Frame, text = gettextRcmdr("Level of Factor2"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("Repeat"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomFrame, text = gettextRcmdr("Randomize / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("Standard Order / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(factor1Field, sticky="w")
  tkgrid(factor2Field, sticky="w")
  tkgrid(repetField, sticky="w")
  tkgrid(seedField, sticky="w")
  tkgrid(randomField, sticky="w")
  tkgrid(stdField, sticky="w")
  tkgrid(factor1Frame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(factor2Frame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(repetFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(seedFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(randomFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(stdFrame, labelRcmdr(mainFrame, text = " "), sticky = "nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

qualaov <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL,initial.repet="1",initial.row = "1", initial.column="1", initial.fac="1")
  dialog.values <- getDialog("qualaov", defaults)
  initializeDialog(title = gettextRcmdr("ANOVA of Qualitative Data"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group,"all"))
  responseBox <- variableListBox(dataFrame, selectmode = "single"
                                 ,title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response,"all"))

  onOK <- function(){
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    repe <- tclvalue(repet)
    rows <- tclvalue(row)
    columns <- tclvalue(column)
    facc <- tclvalue(fac)

    if (length(groups) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = sscatterplot, message = gettextRcmdr("You must select at least one factor."))
      return()}

    putDialog ("qualaov", list (initial.group = groups, initial.response = response, initial.repet = repe, initial.row = rows, initial.column = columns, initial.fac = facc))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = "+")
    doItAndPrint(paste("qual.aov(", response,"~",groups.list,",",.activeDataSet,",","r=",repe,",", "i=",rows,",","j=",columns,",","fac=",facc,")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "qualaov", apply = "qualaov")

  repetFrame <- tkframe(dataFrame)
  repet <- tclVar(dialog.values$initial.repet)
  repetField <- ttkentry(repetFrame, width = "12", textvariable = repet)

  rowFrame <- tkframe(dataFrame)
  row <- tclVar(dialog.values$initial.row)
  rowField <- ttkentry(rowFrame, width = "12", textvariable = row)

  columnFrame <- tkframe(dataFrame)
  column <- tclVar(dialog.values$initial.column)
  columnField <- ttkentry(columnFrame, width = "12", textvariable = column)

  facFrame <- tkframe(dataFrame)
  fac <- tclVar(dialog.values$initial.fac)
  facField <- ttkentry(facFrame, width = "12", textvariable = fac)

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("Repeat"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(rowFrame, text = gettextRcmdr("Number of Row"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(columnFrame, text = gettextRcmdr("Number of Column"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(facFrame, text = gettextRcmdr("1 : One way / 2 : Two way"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(repetField, sticky="w")
  tkgrid(rowField, sticky="w")
  tkgrid(columnField, sticky="w")
  tkgrid(facField, sticky="w")
  tkgrid(repetFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(rowFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(columnFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(facFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}


