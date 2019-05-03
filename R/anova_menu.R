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

anova_window <- function(){

  dataSets <- listDataSets()
  defaults <- list (initial.x = NULL, initial.y = NULL, initial.method = "" )
  dialog.values <- getDialog ("anova_window", defaults)
  initializeDialog(title = gettextRcmdr("ANOVA"))
  xBox <- variableListBox(top, dataSets, title = gettextRcmdr("Independent Variables") )
  yBox <- variableListBox(top, dataSets, title = gettextRcmdr("Dependent Variables") )

  onOK <- function() {
    x <- getSelection(xBox)
    y <- getSelection(yBox)

    if (length(x) == 0) {
      errorCondition(recall = anova_window, message = gettextRcmdr("You must select a variable for x."))
      return()
    }
    if (length(y) == 0) {
      errorCondition(recall = anova_window, message = gettextRcmdr("You must select a variable for y."))
      return()
    }

    x <- as.matrix(x)
    y <- as.matrix(y)

    method <- as.character(tclvalue(methodVariable))
    putDialog ("anova_window", list (initial.x = x, initial.method = method) )
    closeDialog()
    doItAndPrint(paste("ANOVA_output <- decomp(x = ", x, ", y = ", y, ", method='", method, "')", sep = ""))
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "ANOVA", reset = "anova_window")
  optionsFrame <- tkframe(top)
  checkBoxes(optionsFrame, frame = "methodFrame", boxes = c("tukey",
                                                          "fisher", "bonferroni", "scheffe", "duncan", "newmankeuls", "CI", "maineffect", "interaction"),
               labels = gettextRcmdr(c("Tukey's HSD", "Fisher's LSD",
                                       "Bonferroni", "Scheffe", "Duncan", "Newmankeuls", "Confidence Level", "Main Effect", "Interaction")), title = gettextRcmdr("Option"),
               initialValue = NULL)
  rightFrame <- tkframe(optionsFrame)
  tkgrid(getFrame(xBox), sticky = "nw")
  tkgrid(getFrame(yBox), sticky = "nw")
  tkgrid(labelRcmdr(rightFrame, text = ""), sticky = "w")
  tkgrid(methodFrame, rightFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()


}
