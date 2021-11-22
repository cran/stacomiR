#'
#' 
#' Internal function, tests the connection and if it works loads the stacomi interface
#' @note \code{base} is copied by stacomi into envir_stacomi. Same for \code{database_expected}
#' 
#' @param ... Other arguments
#' @return Nothing
#' @author Cedric Briand \email{cedric.briand'at'eptb-vilaine.fr}
#' @keywords internal
load_stacomi <- function(...) {
	
	# assigned when passing through stacomi
	database_expected <- get("database_expected", envir_stacomi)  # logical true or false
	
	if (database_expected) {
		sch <- get_schema()
		dbname  <- options("stacomiR.dbname")[[1]]
		host <- options("stacomiR.host")[[1]]
		port <- options("stacomiR.port")[[1]]
		user <- options("stacomiR.user")[[1]]
		if (user=="") {
			# this is the default options at start
			# if interactive will try to set the options upon loading
			if (interactive()){
				user <- readline(prompt="Enter user: ")
				options("stacomiR.user"=user)
				password <- readline(prompt="Enter password: ")	
				options("stacomiR.password"=password)
			}
		}
		password <- options("stacomiR.password")[[1]]
		
		con = new("ConnectionDB")
		e = expression(con <- connect(con))
		con = tryCatch(eval(e), error = function(e) e)
		if ("Rcpp::exception"%in%class(con)){
			cat(con$message)
			test <- FALSE
		} else {
			test <- TRUE
			pool::poolClose(con@connection)		
		}
		
		
		
		# second test to check that the database is working OK		
		
		if (test) {
			requete = new("RequeteDB")
			requete@sql = paste0("select count(*) from ", sch, "t_lot_lot")
			requete <- stacomirtools::query(requete)
			if (nrow(requete@query) == 0) {
				# the database link is not working or the 
				# schema
				funout(paste(gettext("Problem during the test, connection to the database is established but failed to connect to the right schema argument passed to stacomi",
										domain = "R-stacomiR"), "\n", 
								gettext("dbname", domain = "R-stacomiR")," :", dbname, "\n", 
								gettext("User", domain = "R-stacomiR"),	" :", user, "\n", 
								gettext("Port", domain = "R-stacomiR"),	" :", port, "\n", 
								gettext("Host", domain = "R-stacomiR"),	" :", host, "\n", 								
								gettext("Password", domain = "R-stacomiR"),	" :", password),
						gettext("schema", domain = "R-stacomiR"), " :", sch)
			} 
			
		} else {
			# the test has failed and the user will be prompted to another
			funout(paste(gettext("Problem when testing the DB connection", domain = "R-stacomiR"),
							gettext("dbname", domain = "R-stacomiR")," :", dbname, "\n", 
							gettext("User", domain = "R-stacomiR"),	" :", user, "\n", 
							gettext("Port", domain = "R-stacomiR"),	" :", port, "\n", 
							gettext("Host", domain = "R-stacomiR"),	" :", host, "\n", 								
							gettext("Password", domain = "R-stacomiR"),	" :", password))
		}  # end else test (else == the test didn't pass, we have to change the name and password
	} else {
		# here : database_expected=FALSE we don't want to check the connection
		# at all...
	}
}






#' stacomi Main launcher for program stacomi
#' 
#' When \code{database_expected=FALSE} a connection to the database is not expected. Therefore test are run by calling examples object stored in Rdata.
#' To change the language use Sys.setenv(LANG = 'fr') or Sys.setenv(LANG = 'en')
#' @param database_expected Boolean, if \code{TRUE} pre launch tests will be run to test the connection validity
#' @param datawd The data working directory
#' @param sch The schema in the stacomi database default 'iav.'
#' @return Nothing, called for its side effect of loading
#' @usage stacomi(database_expected=TRUE, datawd = "~", sch = "iav")
#' @author Cedric Briand \email{cedric.briand'at'eptb-vilaine.fr}
#' @examples
#' 
#'  require(stacomiR)
#' #launch stacomi 
#'  \dontrun{ 
#' stacomi(database_expected=TRUE, datawd='~',sch= "iav.")
#' }
#' #launch stacomi without connection to the database
#' stacomi(database_expected=FALSE)
#' 
#' @export
stacomi = function(database_expected = TRUE,  datawd = "~", sch = "iav") {	
	assign("database_expected", database_expected, envir = envir_stacomi)
	# values assigned in the envir_stacomi
	assign("datawd", datawd, envir = envir_stacomi)
	assign("sch", paste(sch, ".", sep = ""), envir = envir_stacomi)
	load_stacomi()
	invisible(NULL)
}






#' Working environment for stacomiR created when launching stacomi()
#' 
#' This is where the graphical interface stores its objects
#' try \code{ls(envir=envir_stacomi)}
#' @keywords environment
#' @export
envir_stacomi <- new.env(parent = asNamespace("stacomiR"))
# calcmig<-data.frame()
