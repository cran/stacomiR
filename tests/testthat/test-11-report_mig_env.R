context("report_mig_env")


test_that("test creating an instance of report_mig_env",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-rlang::env_get(envir_stacomi, "sch") # "iav."
	  assign("sch","iav.",envir_stacomi)
	  r_mig_env<-new("report_mig_env")
	  r_mig_env<-choice_c(r_mig_env,
		  dc=c(5,6,12),
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGJ","AGG","CIV"),
		  stationMesure=c("temp_gabion","coef_maree","phases_lune"),
		  datedebut="2008-01-01",
		  datefin="2008-12-31",
		  silent=TRUE)	
	  r_mig_env<-charge(r_mig_env,silent=TRUE)
	  r_mig_env<-connect(r_mig_env,silent=TRUE)
	  expect_true(nrow(r_mig_env@report_env@data)>0,"Data not loaded in the report_env part of the object" )
	  expect_true(nrow(r_mig_env@report_mig_mult@data)>0,"Data not loaded in the report_mig_mult part of the object" )
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("test plot method",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-rlang::env_get(envir_stacomi, "sch") # "iav."
	  assign("sch","iav.",envir_stacomi)
	  r_mig_env<-new("report_mig_env")
	  r_mig_env<-choice_c(r_mig_env,
		  dc=c(5,6,12),
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGJ","AGG","CIV"),
		  stationMesure=c("temp_gabion","coef_maree","phases_lune"),
		  datedebut="2008-01-01",
		  datefin="2008-12-31",
		  silent=TRUE)	
	  r_mig_env<-charge(r_mig_env,silent=TRUE)
	  r_mig_env<-connect(r_mig_env,silent=TRUE)
	  r_mig_env<-calcule(r_mig_env,silent=TRUE)			
	  suppressWarnings(plot(r_mig_env,silent=TRUE))
	  suppressWarnings(plot(r_mig_env,
			  color_station=c("temp_gabion"="red","coef_maree"="blue","phases_lune"="pink"),
			  color_dc=c("5"="yellow","6"="orange","12"="purple"),silent=TRUE))
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})






