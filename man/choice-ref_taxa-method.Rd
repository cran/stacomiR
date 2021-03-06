% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ref_taxa.R
\docType{methods}
\name{choice,ref_taxa-method}
\alias{choice,ref_taxa-method}
\title{Choice method for reftaxa referential objects with only one taxa selected}
\usage{
\S4method{choice}{ref_taxa}(object, objectreport = NULL,
  is.enabled = TRUE)
}
\arguments{
\item{object}{An object of class \link{ref_taxa-class}}

\item{objectreport}{An object report which includes the \link{ref_taxa-class}, default NULL}

\item{is.enabled}{Sets if the frame is enabled at launch, defaut TRUE}
}
\description{
Choice method for reftaxa referential objects with only one taxa selected
}
\examples{
 \dontrun{
 object=new("ref_taxa")
win=gwindow()
group<-ggroup(container=win,horizontal=FALSE)
object<-charge(object)
report_mig<-new(report_mig)
choice(object,objectreport=report_mig)
}
}
\author{
Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}
\keyword{internal}
