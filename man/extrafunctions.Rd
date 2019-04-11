\name{Extra Functions}
\alias{SaveGUI}
\alias{LoadGUI}
\alias{GetWindowsENVIR}
\alias{ChangeWindow}
\alias{CancelWindow}

\title{Extra Functions}

\description{List of extra functions which can be used. }

\usage{
  SaveGUI(object.names,init.name="result")

  LoadGUI()

  ChangeWindow(dialogtitle,tab=1,framename,argument,new.value)

  CancelWindow(dialogtitle)

  GetWindowsENVIR()

}
\arguments{
  \item{object.names}{Vector of names of the to be saved objects}
  \item{init.name}{Initial name for the RData file}
  \item{dialogtitle}{Title of to be changed window}
  \item{tab}{Number of to be changed tab}
  \item{framename}{Name of to be changed frame}
  \item{argument}{Name of to be changed argument (not label!)}
  \item{new.value}{Character containing the value it should be changed in}
}


\details{More information of the use of these functions can be found in the vignette located in the \code{doc} folder of the package.}


\author{IMERMAENG2 \email{sf01363@naver.com}}
