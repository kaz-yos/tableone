## Makefile for generating R packages.
## original by 2011 Andrew Redd
## https://gist.github.com/halpo/1374344

## Modified by kaz-yos for a different directory structure.
## To be put in the package directory.
## The build and check output files are put in the package directory.
## These files as well as Makefile have to be .Rbuildignore.
## writing rules
## https://www.gnu.org/software/make/manual/html_node/Rules.html#Rules

## Extract the package name and version from the DESCRIPTION file.
PKG_NAME=$(shell    grep -i ^package: DESCRIPTION | cut -d : -d \  -f 2)
PKG_VERSION=$(shell grep -i ^version: DESCRIPTION | cut -d : -d \  -f 2)

## Define files
R_FILES   := $(wildcard R/*.R)
SRC_FILES := $(wildcard src/*) $(addprefix src/, $(COPY_SRC))
PKG_FILES := DESCRIPTION NAMESPACE NEWS $(R_FILES) $(SRC_FILES)


## .PHONY to allow non-file targets (file targets should not be here)
## https://www.gnu.org/software/make/manual/html_node/Phony-Targets.html
.PHONY: build check install clean


### Define targets

## build depends on the *.tar.gz file, i.e., its own product.
## *.tar.gz file is defined seprately to prevent build execution on every invocation.
build: $(PKG_NAME)_$(PKG_VERSION).tar.gz

## (file target) The *.tar.gz file depends on package files including NAMESPACE,
## and build *.tar.gz file from these.
$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	R CMD build ../${PKG_NAME}

## (file target) NAMESPACE depends on *.R files, and excecute roxygen2 on these.
## methods::is() is not automatically loaded by roxygen2 version 4
NAMESPACE: $(R_FILES)
	Rscript -e "library(methods); library(roxygen2); roxygenize('.')"

## check requires the *.tar.gz file, and execute strict tests on it.
check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check --as-cran ./$(PKG_NAME)_$(PKG_VERSION).tar.gz

## install requires the *.tar.gz file, and execute installation using it.
install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD install ./$(PKG_NAME)_$(PKG_VERSION).tar.gz


## clean has no dependency, and execute removal of make output files.
clean:
	-rm    -f $(PKG_NAME)_*.tar.gz
	-rm -r -f $(PKG_NAME).Rcheck
	-rm -r -f man/*.Rd
	-rm -r -f NAMESPACE


## Define a target "list" that just prints the names of files.
.PHONY: list
list:
	@echo "R files:"
	@echo $(R_FILES)
	@echo
	@echo "Source files:"
	@echo $(SRC_FILES)
