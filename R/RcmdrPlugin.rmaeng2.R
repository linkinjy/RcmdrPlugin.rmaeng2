aanova <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.formul = "")
  dialog.values <- getDialog("aanova", defaults)
  initializeDialog(title = gettextRcmdr("분산분석"))
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    formull <- as.character(tclvalue(formul))

    if (length(groups) == 0) {
      errorCondition(recall = aanova, message = gettextRcmdr("You must select at least one factor."))
      return()}
    if (length(response) == 0) {
      errorCondition(recall = aanova, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("aanova", list (initial.group = groups, initial.response = response, initial.formul = formull))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste("aov(", response,"~", formull,",data=",.activeDataSet,")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "aanova", apply = "aanova")

  formulFrame <- tkframe(dataFrame)
  formul <- tclVar(dialog.values$initial.formul)
  formulField <- ttkentry(formulFrame, width = "20", textvariable = formul)

  tkgrid(labelRcmdr(formulFrame, text = gettextRcmdr("Formula / ex) a+b+c+a*b+b*c+c*a+a*b*c"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(formulField, sticky="w")
  tkgrid(formulFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

onerandomtable <- function () {
  defaults <- list(initial.factorname = 'c("a","b","c")', initial.repetition = "", initial.seed="0", initial.std="FALSE")
  dialog.values <- getDialog("onerandomtable", defaults)
  initializeDialog(title = gettextRcmdr("일원배치법 난수표"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    fac<-as.character(tclvalue(factorname))
    repet<-tclvalue(repetition)
    sed<-tclvalue(seed)
    sttd<-as.character(tclvalue(std))

    putDialog ("onerandomtable", list (initial.factorname = fac, initial.repetition = repet, initial.seed = sed, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("random.one(", "fac=", fac,",", "r=", repet,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    justDoIt(paste(modelValue, "<-random.one(","fac=", fac,",", "r=", repet,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    activeModel(modelValue)

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
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(factorFrame, text = gettextRcmdr("요인"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("반복수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("표준순서 / TRUE : 표준순서 or FALSE : 실행 순서"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
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
  initializeDialog(title = gettextRcmdr("라틴 및 그레코라틴 난수표"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    n<-tclvalue(factornumber)
    greco<-as.character(tclvalue(greco))
    sed<-tclvalue(seed)
    sttd<-as.character(tclvalue(std))

    putDialog ("grecorandomtable", list (initial.factornumber = n, initial.greco = greco, initial.seed = sed, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("random.latin(", "n=", n,",", "greco=", greco,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    justDoIt(paste(modelValue, "<-random.latin(", "n=", n,",", "greco=", greco,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    activeModel(modelValue)

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
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(factorFrame, text = gettextRcmdr("요인의 수 / N*N 라틴방격"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(grecoFrame, text = gettextRcmdr("라틴, 그레코 / TRUE = 그레코, FALSE = 라틴"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("표준순서 / TRUE : 표준순서 or FALSE : 실행 순서"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
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
  initializeDialog(title = gettextRcmdr("다원배치법 난수표"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    fac<-as.character(tclvalue(factornumber))
    repet<-tclvalue(repetition)
    sed<-tclvalue(seed)
    sttd<-as.character(tclvalue(std))

    putDialog ("onerandomtable", list (initial.factornumber = fac, initial.repetition = repet, initial.seed = sed, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("random.multi(", "fac=", fac,",", "r=", repet,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    justDoIt(paste(modelValue, "<-random.multi(", "fac=", fac,",", "r=", repet,",", "seed=", sed,",", "std=", sttd, ")", sep = ""))
    activeModel(modelValue)
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
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(factorFrame, text = gettextRcmdr("요인 / c(A인자 수준수, B인자 수준수)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("반복수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("표준순서 / TRUE : 표준순서 or FALSE : 실행 순서"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
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
  initializeDialog(title = gettextRcmdr("주효과도"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "single",
                              title = gettextRcmdr("요인 (하나 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

  onOK <- function() {
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    if (length(groups) == 0) {
      errorCondition(recall = mainplot, message = gettextRcmdr("You must select at least one factor."))
      return()}
    putDialog ("mainplot", list (initial.group = groups, initial.response = response))
    closeDialog()

    .activeDataSet <- ActiveDataSet()
    doItAndPrint(paste("main.effect(", .activeDataSet,',',.activeDataSet,"$",groups,',',.activeDataSet,"$",response, ")"))
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
  initializeDialog(title = gettextRcmdr("교호작용도"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 (두개 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

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
    groups.list <- paste(paste(rep(ActiveDataSet()),rep('$'),groups, sep = ""), collapse = ",")
    doItAndPrint(paste("interaction.effect(", .activeDataSet,',',groups.list,',',.activeDataSet,"$",response, ")"))
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
  initializeDialog(title = gettextRcmdr("산점도"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 (하나 이상 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

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
    groups.list <- paste(paste(rep(ActiveDataSet()),rep('$'),groups, sep = ""), collapse = ",")
    doItAndPrint(paste("scatter.plot(", .activeDataSet,",",groups.list,",",.activeDataSet,"$",response, ")"))
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
  initializeDialog(title = gettextRcmdr("일원배치법 오차분산의 추정"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "single",
                              title = gettextRcmdr("요인 (하나 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("반응변수 (하나 선택)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

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
    doItAndPrint(paste("error.var(", .activeDataSet,"$",response,"~",.activeDataSet,"$",groups,",",.activeDataSet,",",al, ")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "oneerror", apply = "oneerror")

  alphaFrame <- tkframe(dataFrame)
  alpha <- tclVar(dialog.values$initial.alpha)
  alphaField <- ttkentry(alphaFrame, width = "12", textvariable = alpha)

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(labelRcmdr(alphaFrame, text = gettextRcmdr("알파"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
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
  initializeDialog(title = gettextRcmdr("모평균차 : 요인 및 교호작용 (사용자 설정)"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("요인 (하나 이상 선택)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("반응변수 (하나 선택)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

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

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(rep("factor"),rep("("),rep(ActiveDataSet()),rep('$'),groups, rep(")"),sep = ""), collapse = "+")
    doItAndPrint(paste("mean.diff(", .activeDataSet,"$",response,"~",groups.list,",","method=",'"',methodd,'"',")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "usermeandiff", apply = "usermeandiff")

  methodFrame <- tkframe(dataFrame)
  method <- tclVar(dialog.values$initial.method)
  methodField <- ttkentry(methodFrame, width = "12", textvariable = method)

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(labelRcmdr(methodFrame, text = gettextRcmdr("방법
(입력 : hsd / lsd / bonferroni / scheffe / newmankeuls / duncan)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(methodField, sticky="w")
  tkgrid(methodFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

meandiff <- function () {
  defaults <- list(initial.group = NULL,initial.response = NULL,initial.method="hsd")
  dialog.values <- getDialog("meandiff", defaults)
  initializeDialog(title = gettextRcmdr("모평균차 : 모든 요인 및 교호작용"), use.tabs = FALSE)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, selectmode = "multiple",
                              title = gettextRcmdr("Factors (pick one or more)"),
                              initialSelection = varPosn(dialog.values$initial.group, "all"))
  responseBox <- variableListBox(dataFrame, title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "all"))

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

    .activeDataSet <- ActiveDataSet()
    groups.list <- paste(paste(rep("factor"),rep("("),rep(ActiveDataSet()),rep('$'),groups, rep(")"),sep = ""), collapse = "*")
    doItAndPrint(paste("mean.diff(", .activeDataSet,"$",response,"~",groups.list,",","method=",'"',methodd,'"',")", sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "meandiff", apply = "meandiff")

  methodFrame <- tkframe(dataFrame)
  method <- tclVar(dialog.values$initial.method)
  methodField <- ttkentry(methodFrame, width = "12", textvariable = method)

  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), sticky = "nw")
  tkgrid(getFrame(responseBox), labelRcmdr(dataFrame, text=" "), sticky = "nw")
  tkgrid(labelRcmdr(methodFrame, text = gettextRcmdr("방법
(입력 : hsd / lsd / bonferroni / scheffe / newmankeuls / duncan)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(methodField, sticky="w")
  tkgrid(methodFrame, labelRcmdr(dataFrame, text = " "), sticky = "nw")
  tkgrid(dataFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(use.tabs = FALSE, grid.buttons = TRUE)
}

facrandomtable <- function () {
  defaults <- list(initial.level = "c(3,3,2)", initial.nvars = "3", initial.repet="2", initial.std="FALSE")
  dialog.values <- getDialog("facrandomtable", defaults)
  initializeDialog(title = gettextRcmdr("요인배치법 난수표"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))


    levell<-as.character(tclvalue(level))
    nvar<-tclvalue(nvars)
    repe<-tclvalue(repet)
    sttd<-as.character(tclvalue(std))

    putDialog ("facrandomtable", list (initial.level = levell, initial.nvars = nvar, initial.repet = repe, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("fac.design(", "levels=", levell,",", "nVars=", nvar,",", "r=", repe,",","std=", sttd, ")", sep = ""))
    justDoIt(paste(modelValue, "<-fac.design(", "levels=", levell,",", "nVars=", nvar,",", "r=", repe,",","std=", sttd, ")", sep = ""))
    activeModel(modelValue)
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
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(levelFrame, text = gettextRcmdr("각 요인의 수준 / c(A인자 수준,B인자 수준,...)"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(nvarsFrame, text = gettextRcmdr("요인의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("반복수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("표준순서 / TRUE : 표준순서 or FALSE : 실행 순서"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
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
  initializeDialog(title = gettextRcmdr("Box-Benken 설계"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    kfactors<-tclvalue(kfactor)
    centers<-tclvalue(center)
    randoms<-as.character(tclvalue(random))

    putDialog ("boxbenken", list (initial.kfactor = kfactors, initial.center = centers, initial.random = randoms))
    closeDialog()

    doItAndPrint(paste("kbbd(", "k=", kfactors,",", "n0=", centers,",","randomize=", randoms, ")", sep = ""))
    justDoIt(paste(modelValue, "<-kbbd(", "k=", kfactors,",", "n0=", centers,",","randomize=", randoms, ")", sep = ""))
    activeModel(modelValue)
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
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(kfactorFrame, text = gettextRcmdr("요인의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(centerFrame, text = gettextRcmdr("중심점의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomFrame, text = gettextRcmdr("랜덤화 / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
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
  initializeDialog(title = gettextRcmdr("중심합성계획"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    basiss<-tclvalue(basis)
    centers<-tclvalue(center)
    wbrep<-tclvalue(wbreps)
    bbrep<-tclvalue(bbreps)
    randoms<-as.character(tclvalue(random))
    oneblocks<-as.character(tclvalue(oneblock))

    putDialog ("central", list (initial.basis = basiss, initial.center = centers, initial.wbreps = wbrep, initial.bbreps = bbrep,initial.random = randoms, initial.oneblock = oneblocks))
    closeDialog()

    doItAndPrint(paste("kccd(", "basis=", basiss,",", "n0=", centers,",","wbreps=",wbrep,",","bbreps=",bbrep,",","randomize=", randoms,"," ,"oneblock=",oneblocks,")", sep = ""))
    justDoIt(paste(modelValue, "<-kccd(", "basis=", basiss,",", "n0=", centers,",","wbreps=",wbrep,",","bbreps=",bbrep,",","randomize=", randoms,"," ,"oneblock=",oneblocks,")", sep = ""))
    activeModel(modelValue)
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
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(basisFrame, text = gettextRcmdr("요인의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(centerFrame, text = gettextRcmdr("중심점의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(wbrepsFrame, text = gettextRcmdr("블록 내 반복의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(bbrepsFrame, text = gettextRcmdr("블록 간 반복의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomFrame, text = gettextRcmdr("랜덤화 / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(oneblockFrame, text = gettextRcmdr("블록이 하나인가?/ TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
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
  initializeDialog(title = gettextRcmdr("분할법 난수표"), use.tabs = FALSE)
  mainFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))

    factor11<-as.character(tclvalue(factor1))
    factor22<-as.character(tclvalue(factor2))
    repe<-tclvalue(repet)
    sed<-tclvalue(seed)
    randoms<-as.character(tclvalue(random))
    sttd<-as.character(tclvalue(std))

    putDialog ("designsplit", list (initial.factor1 = factor11, initial.factor2 = factor22, initial.repet = repe, initial.seed = sed,initial.random = randoms, initial.std = sttd))
    closeDialog()

    doItAndPrint(paste("design.split(", factor11,",", factor22,",","r=",repe,",","seed=",sed,",","randomization=", randoms,"," ,"std=",sttd,")", sep = ""))
    justDoIt(paste(modelValue, "<-design.split(", factor11,",", factor22,",","r=",repe,",","seed=",sed,",","randomization=", randoms,"," ,"std=",sttd,")", sep = ""))
    activeModel(modelValue)
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
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(factor1Frame, text = gettextRcmdr("요인1의 수준"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(factor2Frame, text = gettextRcmdr("요인2의 수준"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("반복수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(seedFrame, text = gettextRcmdr("Seed"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(randomFrame, text = gettextRcmdr("랜덤화 / TRUE or FALSE"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(stdFrame, text = gettextRcmdr("표준순서 / TRUE : 표준순서 or FALSE : 실행 순서"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
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
  initializeDialog(title = gettextRcmdr("계수치 분산분석"), use.tabs = FALSE)
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
  tkgrid(labelRcmdr(repetFrame, text = gettextRcmdr("반복수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(rowFrame, text = gettextRcmdr("행의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(columnFrame, text = gettextRcmdr("열의 수"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(facFrame, text = gettextRcmdr("1 : 일원배치 / 2 : 이원배치"), fg = getRcmdr("title.color"), font = "RcmdrTitleFont"), sticky = "w")
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


