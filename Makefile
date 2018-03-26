all: man site targz

man :
	R -e 'roxygen2::roxygenise()' --silent

site :
	R -e 'pkgdown::build_site()' --silent

targz :
	R CMD build .
