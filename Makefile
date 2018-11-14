# Makefile
# Time-stamp: <2018-11-14 13:02:39 (slane)>
.PHONY: all build-docker install-packages clean-manuscripts clobber

all: install-packages data/data.rds manuscripts/manuscript.pdf

################################################################################
# Build docker container for reproducible environment
.PHONY: build-docker
build-docker: .build.docker
.build.docker: docker/Dockerfile
	docker build --rm -t hierarch-path docker/; \
	touch .build.docker

################################################################################
# Rules to make data
# data/data.rds: R/clean-data.R data-raw/data.csv
# 	cd $(<D); \
# 	Rscript --no-save --no-restore $(<F)

################################################################################
# Rules to make manuscripts
# manuscripts/manuscript.tex: manuscripts/manuscript.Rnw data/data.rds
# 	cd $(<D); \
# 	Rscript --no-save --no-restore -e "knitr::knit('$(<F)')"

# manuscripts/manuscript.pdf: manuscripts/manuscript.tex
# 	cd $(<D); \
# 	latexmk -pdf $(<F)

# manuscripts/manuscript.html: manuscripts/manuscript.Rmd
# 	cd $(<D); \
# 	Rscript --no-save --no-restore -e "rmarkdown::render('$(<F)')"

################################################################################
# Cleaning up
clean-manuscripts:
	cd manuscripts/; \
	rm -f *.aux *.bbl *.bcf *.blg *.fdb_latexmk *.fls *.lof *.log *.lot \
		*.code *.loe *.toc *.rec *.out *.run.xml *~ *.tex

clobber: clean-manuscripts
	cd manuscripts/; \
	rm -rf auto/ cache/ figure/

################################################################################
# Rule to install packages (from script extraction).
# This has been provided for the case where library/require calls are made in
# separate scripts. If the packages are not installed, make should return an
# error, saying that the packages are not available. This is politic; don't
# assume the user wants to install stuff without asking.
install-packages: scripts/installs.txt
scripts/installs.txt: scripts/strip-libs.sh R/ipak.R
	cd scripts; \
	chmod u+x strip-libs.sh; \
	./strip-libs.sh ../R/ installs.txt; \
	./strip-libs.sh ../Rmd/ installs.txt; \
	./strip-libs.sh ../manuscripts/ installs.txt; \
	Rscript --no-save --no-restore ../R/ipak.R insts=installs.txt
