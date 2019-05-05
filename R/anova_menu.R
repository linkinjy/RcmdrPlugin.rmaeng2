.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  putRcmdr("slider.env", new.env())
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }
  }
}

anova_window <- function () {
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
