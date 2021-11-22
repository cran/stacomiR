# stacomiR 0.6.0

stacomir based on gwidget was dropped from CRAN in May 2020, it has been completely reprogrammed to remove all dependence to
gwidget and the graphical interface will be a new shiny package (under developpement). Stacomirtools has been rewritten and submitted to CRAN 
to connect via pool and DBI.

* added import to package  `rlang` 
* Change to adapt to new dplyr format `dplyr:n()` instead of `n()` see dplyr breaking change in version 1.0.0
* adapted to stacomirtools new connection and connection options (using pool and DBI instead of RODC)
* removed all dependency to gwidgetRGTK2 so in practise there is no longer a graphical interface, the shiny package is due soon.
* added code of conduct
* rewrote unit tests increased coverage from 50 to 78 %

# stacomiR 0.5.4.4 (not released on CRAN)


* added import to package  `rlang` 
* Change to adapt to new dplyr format `dplyr:n()` instead of `n()` see dplyr breaking change in version 1.0.0


# stacomiR 0.5.4.3


* fixed problem when report with just one line in one year where total number is zero
* fixed bug in monthly report when there is just a single month
* fixed new bug in report_interannual where no value returned from the database created a character which 
was crashing the program
* fixed bug in bilan interannual when quantiles were calculated on a single line, now reports exactly the value of that line for all quantiles and avoid crashes.
* fixed problem in plot,report_sample_char,missing-method, a ... additional was documented but not present (sent from CRAN)
* The vignette crashes as the program finds difference between rounded numbers, while I don't understand why, 
    round(sum(datasub$value, na.rm = TRUE), 2) and round(sum(datasub2$value, na.rm = TRUE), 2) are not equal: and provide a difference that is not rounded (e.g. 0.000996741) I have changed the test.

# stacomiR 0.5.4.2

* fixed new issues with rounding error when building vignette

# stacomiR 0.5.4.1

* fixed issue with prototype generating check warning on CRAN

# stacomiR 0.5.4.0

* removed reference to test database, and use of dplyr for internal calculations. 
Trying to launch as test database with postgres failed on distant computers for tests.... (@marion #484-502)

* Updated testhat tests (@marion  @cedric #502-506)

* developped vignette for package (@marion #480-484)

# stacomiR 0.5.3.2

* changed stacomiR-package.md according to Kurt Hornik (CRAN) demand to use macro in share (@cedric #481)

* bug in interface report_dc corrected (@cedric #489)

* Problem with ceilPOSIXt now named only ceil in package Hmisc. (@cedric #489)


# stacomiR 0.5.3.1

* added and edited readme.Rmd file (@cedric and @marion #447-#452)

* compilation of messages, fixes bug for for some year in report\_mig\_mult (@cedric #444)

* Editing translation messages (@cedric #446)

* fix bug for setasqualitative method in report\_sea__age (@cedric #443)

* updated documentation (@cedric #440)

* Modification in glass eel graph (@cedric #441)

* changes to the summary-report\_mig\_interannual-method (@cedric # 439)

* change to the presentation page in R-forge (@timothée #425 #436-#438)

# stacomiR 0.5.3

* Language with poedit (@cedric and @timothee).

* suppress all assignments to .globalEnv.

* adapt documentation and package to pass with zero warning and zero note.

# stacomiR 0.5.2 (R-Forge)

* Development of the command line interface (@cedric)

* Adaptation to the database change to adapt French SANDRE directives (@cedric)

* added 4 new report (silver eel, sea age, report\_mig\_mult, report_annual) and adapted all old script to use in both command line and graphical interface. (@ cedric and @marion)

# stacomiR 0.5.1 (R-Forge)

* tests with testthat (@cedric)

# stacomiR 0.5.0 (R-Forge)

* Roxygen documentation (@cedric)

# stacomiR 0.4 (R-Forge)

* classes adapted to the new database format

# stacomiR 0.3 (R-Forge)

* Adapting to one schema per user in the postgres database 

* initial development of all classes following version 0.3 of the database
 