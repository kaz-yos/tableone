## Custom expect functions to enable testing against a reference csv
expect_known_csv <- function(object, file,
                             update = TRUE,
                             ...,
                             info = NULL,
                             label = NULL,
                             version = 2) {
    act <- quasi_label(enquo(object), label, arg = "object")

    if (!file.exists(file)) {
        warning("Creating reference value", call. = FALSE)
        write.csv(object, file)
        succeed()
    } else {
        ref_val <- read.csv(file)
        comp <- compare(act$val, ref_val, ...)
        if (update && !comp$equal) {
            write.csv(act$val, file)
        }


        expect(
            comp$equal,
            sprintf(
                "%s has changed from known value recorded in %s.\n%s",
                act$lab, encodeString(file, quote = "'"), comp$message
            ),
            info = info
        )
    }

    invisible(act$value)
}

## Overwrite expect_equal_to_reference to test against both RData and csv
expect_equal_to_reference <- function(mat, name, ..., update = FALSE) {

    ## Test against RData
    expect_known_value(..., update = update)

    ## Test against csv
    expect_known_csv(object = mat,
                     file = paste0(name, "csv"),
                     update = update
                     ...)
}
