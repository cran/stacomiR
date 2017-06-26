\name{stacomiR-package}
\alias{stacomiR-package}
\docType{package}
\title{package Fish Migration Monitoring (stacomiR)- Stations de Controle des Migrations}
\description{
'Stacomi' enables to read informations in a database about fish migrations to build overviews of fish migrations and creating charts, plots, and summaries.
The 'stacomi' project comprises an open source 'Postgres' database, and a 'java' interface to fill in data. Please contact the author if you want to try it out.
}
\details{
\tabular{ll}{
Package: \tab stacomiR\cr
Type: \tab Package\cr
Version: \tab 0.5.3\cr
Date: \tab 2017-06-24\cr
License: \tab  GPL (>= 2)\cr
LazyLoad: \tab yes\cr
Depends: \tab methods\cr
}
To use the stacomiR package, simply load the library using \code{library(stacomiR)}. Once done, type \code{stacomi()} in R terminal.
The use of stacomi(gr_interface=FALSE) allows to launch stacomi without graphical interface from the command line. The program is currently
translated to French, to switch to English or French type \code{Sys.setenv(LANG = "en")} \code{Sys.setenv(LANG = "fr")}
}
\author{
Author and Maintainer : \email{cedric.briand"at"eptb-vilaine.fr}
}
\keyword{package}
\keyword{dynamic}
\keyword{database}
\seealso{
\pkg{stacomirtools}
}

\examples{
  #stacomi()
}
