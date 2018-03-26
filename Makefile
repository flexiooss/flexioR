all:
	R -e 'roxygen2::roxygenise()' --silent
	R CMD build .
