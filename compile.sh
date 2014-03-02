#!/bin/sh

## Set package name
pkgName="tableone"

## build documents (load methods and utils explicitly)
echo "################################################################################"
echo "creating .rd files"
# echo "running ./man_create.R"
echo ""
# ./man_create.R
Rscript -e 'library(methods); library(utils); devtools::document("../tableone/"); devtools::check_doc("../tableone/")'


## build a package
echo ""
echo "################################################################################"
echo "building"
echo ""
cd ../
R CMD build ./tableone

## run tests on the latest archive file
echo ""
echo "################################################################################"
archiveName=`find tableone_*.tar.gz | tail -n 1`
echo "checking ${archiveName}"
echo ""
R CMD check --as-cran ${archiveName}

## open the test
echo ""
echo "################################################################################"
echo "opening test results"
echo ""
open ./tableone.Rcheck

## get back to the same directory
cd ./tableone
