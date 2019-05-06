.onAttach <- function(libname, pkgname) {
  if (!interactive()) {
    return()
  }

  # Current options
  Rcmdr_opts <- options()$Rcmdr

  # If empty, convert to named list
  if (is.null(Rcmdr_opts)) {
    Rcmdr_opts <- list(plugins = NULL)
  }

  # TODO: getRcmdr("messages.height")  <----------- [???]

  # Plugins to add
  add_plugins <- "RcmdrPlugin.rmaeng2"

  # Add plugins in certain order
  plugins <- c(
    setdiff(Rcmdr_opts$plugins, add_plugins),
    rev(sort(add_plugins))
  )

  # Open 3-window Rcmdr, if options is not defined
  if (is.null(Rcmdr_opts$console.output)) {
    console.output <- FALSE

  } else {
    console.output <- Rcmdr_opts$console.output
  }

  updated_opts <-
    utils::modifyList(
      Rcmdr_opts,
      list(plugins = plugins, console.output = console.output)
    )

  if (!identical(Rcmdr_opts, updated_opts)) {
    # Set new options and restart R Commander
    options(Rcmdr = updated_opts)

    # if (!"package:Rcmdr" %in% search()) {
    #     Rcmdr::Commander()
    #
    # } else {
    #     if (!isTRUE(Rcmdr::getRcmdr("autoRestart", fail = FALSE))) {
    #         Rcmdr::closeCommander(ask = FALSE, ask.save = TRUE)
    #         Rcmdr::Commander()
    #     }
    # }
  }}

gvc_decomp <- function(){
  require(decompr)

  dataSets <- listDataSets()
  defaults <- list (initial.x = NULL, initial.y = NULL, initial.method = "leontief" )
  dialog.values <- getDialog ("gvc_decomp", defaults)
  initializeDialog(title = gettextRcmdr("GVC Decomposition"))
  xBox <- variableListBox(top, dataSets, title = gettextRcmdr("Intermediate Demand") )
  yBox <- variableListBox(top, dataSets, title = gettextRcmdr("Final Demand") )
  kBox <- variableListBox(top, dataSets, title = gettextRcmdr("Countries / Regions") )
  iBox <- variableListBox(top, dataSets, title = gettextRcmdr("Industries") )
  oBox <- variableListBox(top, dataSets, title = gettextRcmdr("Final Output") )
  onOK <- function() {
    x <- getSelection(xBox)
    y <- getSelection(yBox)
    k <- getSelection(kBox)
    i <- getSelection(iBox)
    o <- getSelection(oBox)
    if (length(x) == 0) {
      errorCondition(recall = gvc_decomp, message = gettextRcmdr("You must select a variable for x."))
      return()
    }
    if (length(y) == 0) {
      errorCondition(recall = gvc_decomp, message = gettextRcmdr("You must select a variable for y."))
      return()
    }
    if (length(k) == 0) {
      errorCondition(recall = gvc_decomp, message = gettextRcmdr("You must select a variable for k."))
      return()
    }
    if (length(i) == 0) {
      errorCondition(recall = gvc_decomp, message = gettextRcmdr("You must select a variable for i."))
      return()
    }
    if (length(o) == 0) {
      errorCondition(recall = gvc_decomp, message = gettextRcmdr("You must select a variable for o."))
      return()
    }
    x <- as.matrix(x)
    y <- as.matrix(y)
    k <- as.vector(unlist(lapply(k, as.character)))
    i <- as.vector(unlist(lapply(i, as.character)))
    o <- as.vector(unlist(o))
    method <- as.character(tclvalue(methodVariable))
    putDialog ("gvc_decomp", list (initial.x = x, initial.method = method) )
    closeDialog()
    doItAndPrint(paste("decomposition_output <- decomp(x = ", x, ", y = ", y, ", k = ", k, ", i = ", i, ", o = ", o, ", method='", method, "')", sep = ""))
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "decomp", reset = "gvc_decomp")
  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "method", buttons = c("leontief",
                                                          "wwz", "vertical_specialisation"), values = c("leontief", "wwz", "vertical_specialisation"),
               labels = gettextRcmdr(c("Leontief", "Wang-Wei-Zhu",
                                       "Vertical Specialisation")), title = gettextRcmdr("Decomposition method"),
               initialValue = dialog.values$initial.method)
  rightFrame <- tkframe(optionsFrame)
  tkgrid(getFrame(xBox), sticky = "nw")
  tkgrid(getFrame(yBox), sticky = "nw")
  tkgrid(getFrame(kBox), sticky = "nw")
  tkgrid(getFrame(iBox), sticky = "nw")
  tkgrid(getFrame(oBox), sticky = "nw")
  tkgrid(labelRcmdr(rightFrame, text = ""), sticky = "w")
  tkgrid(methodFrame, rightFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()


}
