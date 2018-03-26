all: targz

man :
	R -e 'roxygen2::roxygenise()' --silent

site :
	R -e 'pkgdown::build_site()' --silent

targz : man site
	R CMD build .
