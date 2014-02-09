#!/bin/sh

## Invoke in the target folder
for file in *.Rd

do
    ## Remove comment lines
    cat ${file} | grep -v "^ *%" > ${file}2
    ## Save the old files with comments
    mv ${file} ${file}Old
    ## Rename the new files to the original names
    mv ${file}2 ${file}
done

