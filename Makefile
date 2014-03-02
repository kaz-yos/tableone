# Makefile for generating R packages.
# 2011 Andrew Redd
#
# Assumes Makefile is in a folder where package contents are in a subfolder pkg.
# Roxygen uses the roxygen2 package, and will run automatically on check and all.

PKG_VERSION=$(shell grep -i ^version pkg/DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package pkg/DESCRIPTION | cut -d : -d \  -f 2)

R_FILES := $(wildcard pkg/R/*.R)
SRC_FILES := $(wildcard pkg/src/*) $(addprefix pkg/src/, $(COPY_SRC))
PKG_FILES := pkg/DESCRIPTION pkg/NAMESPACE $(R_FILES) $(SRC_FILES)

.PHONY: tarball install check clean build

tarball: $(PKG_NAME)_$(PKG_VERSION).tar.gz
$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	R CMD build pkg


check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

build: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL --build $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

pkg/NAMESPACE: $(R_FILES)
	Rscript -e "library(roxygen2);roxygenize('pkg')"

clean:
	-rm -f $(PKG_NAME)_*.tar.gz
	-rm -r -f $(PKG_NAME).Rcheck
	-rm -r -f pkg/man/*
	-rm -r -f pkg/NAMESPACE
.PHONY: list
list:
	@echo "R files:"
	@echo $(R_FILES)
	@echo "Source files:"
	@echo $(SRC_FILES)
