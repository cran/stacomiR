# Global variables for test

# this file is called before testhat so funcion will be available in all test
# https://testthat.r-lib.org/articles/test-fixtures.html#withr-defer-
# could have used with_envvar and local_envvar but had to set them each time

env_set_test_stacomi <- function(env = parent.frame()) {
	o <- options()
	withr::defer(options(					
			stacomiR.dbname = "bd_contmig_nat",
			stacomiR.host ="localhost",
			stacomiR.port = "5432",
			stacomiR.user = "postgres",
			stacomiR.password = "postgres"					
	),	env)
assign("user","postgres",envir=env)
assign("password","postgres",envir=env)
assign("host", "localhost", envir=env)
assign("schema", "iav", envir=env)
# test for foreign keys in the database ? set TRUE to test FALSE to avoid tests
# if set to TRUE be sure that user and password correspond to superuser
assign("test_foreign_keys", TRUE, envir=env)

}
