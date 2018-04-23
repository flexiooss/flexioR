all: targz

man : clean
	R -e 'roxygen2::roxygenise()' --silent

site : man
	R -e 'pkgdown::build_site()' --silent

targz : man site
	R CMD build .

clean :
	rm -rf man/ docs/ flexioR_*.tar.gz
