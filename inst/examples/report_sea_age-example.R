require(stacomiR)
stacomi(gr_interface=FALSE,
	login_window=FALSE,
	database_expected=FALSE)
\dontrun{
  #create an instance of the class
  r_seaa<-new("report_sea_age")
  baseODBC<-get("baseODBC",envir=envir_stacomi)
  baseODBC[c(2,3)]<-rep("logrami",2)
  assign("baseODBC",baseODBC,envir_stacomi)
  sch<-rlang::env_get(envir_stacomi, "sch")
  assign("sch","logrami.",envir_stacomi)
  r_seaa<-choice_c(r_seaa,
	  dc=c(107,108,101),			
	  horodatedebut="2012-01-01",
	  horodatefin="2012-12-31",
	  limit1hm=675,
	  limit2hm=875,
	  silent=FALSE
  )
  r_seaa<-connect(r_seaa)
  r_seaa<-calcule(r_seaa)
  
}	
# load the dataset generated by previous lines
# Salmons from the loire on two dams
data("r_seaa")
# the calculation will fill the slot calcdata

# stages are in r_seaa@calcdata[["6"]][,"stage"] 
#look at data structure using str(r_seaa@calcdata[["6"]])

# plot data to confirm the split by limits is correct
plot(r_seaa, plot.type=1)

# if there are several dc, data it split by dc
plot(r_seaa, plot.type=2)
\dontrun{
# print a summary statistic, and save the output in a list for later use
  stats<-summary(r_seaa)
  
  
  write_database(r_seaa)
}
