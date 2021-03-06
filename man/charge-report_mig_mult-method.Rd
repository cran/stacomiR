% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_mig_mult.R
\docType{methods}
\name{charge,report_mig_mult-method}
\alias{charge,report_mig_mult-method}
\alias{charge.report_mig_mult}
\title{charge method for report_mig_mult}
\usage{
\S4method{charge}{report_mig_mult}(object, silent = FALSE)
}
\arguments{
\item{object}{An object of class \link{report_mig_mult-class}}

\item{silent}{Default FALSE, if TRUE the program should no display messages}
}
\value{
report_mig_mult with slots filled by user choice
}
\description{
Unique the other report classes where the charge method is only used by the graphical interface 
to collect and test objects in the environment envir_stacomi, and see if the right choices have
been made in the graphical interface, this method is used to load data on migration control operations
fishway operations, and counting devices operations as data from those are displayed in the main plots.
}
\author{
Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}
