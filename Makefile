# Makefile
# Time-stamp: <2018-12-04 09:23:44 (slane)>
.PHONY: all build-docker install-packages clean-manuscripts clobber

all: install-packages data/data.rds manuscripts/manuscript.pdf

# Set the directory of the Makefile.
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

################################################################################
# Build docker container for reproducible environment
.PHONY: build-docker
build-docker: .build.docker
.build.docker: docker/Dockerfile
	docker build --rm -t hierarch-path docker/; \
	touch .build.docker

################################################################################
# Test the JAGS models
#' jags-test: Runs scripts to test the JAGS models with simulated data.
.PHONY: jags-test
jags-test: figs/jags-testing/ca-cd-tests.pdf \
	figs/jags-testing/ca-ld1-tests.pdf \
	figs/jags-testing/ca-ld2-tests.pdf
figs/jags-testing/ca-cd-tests.pdf: R/jags-testing-ca-cd.R \
	scripts/constant-arrival-constant-detection.jag
	mkdir -p figs/jags-testing \
	&& cd $(<D) \
	&& Rscript --no-save --no-restore $(<F)

figs/jags-testing/ca-ld1-tests.pdf: R/jags-testing-ca-ld1.R \
	scripts/constant-arrival-linear-detection.jag
	mkdir -p figs/jags-testing \
	&& cd $(<D) \
	&& Rscript --no-save --no-restore $(<F)

figs/jags-testing/ca-ld2-tests.pdf: R/jags-testing-ca-ld2.R \
	scripts/constant-arrival-linear-detection-site-specific.jag
	mkdir -p figs/jags-testing \
	&& cd $(<D) \
	&& Rscript --no-save --no-restore $(<F)

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

################################################################################
# Show help on important targets.
.PHONY: help
help: Makefile
	@echo '\nHelp on Hierarchical Pathway Simulation Targets\n----';
	@sed -n "s/^#' //p" $(<F);
	@echo '----';

################################################################################
# Rule to create dependency graph.
deps-png:
	docker run -v $$(pwd):/tmp/graph stevelane/makedeps-graph; \
	mv deps.* figs/
