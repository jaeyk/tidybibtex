#' Make exceptions for capitalizing the first letter of a string vector.
#'
#' @param pattern The target word to avoid capitalization.
#' @param replacement The target string object that contains the target word.
#' @return The target string object in which the target word becomes lower cased.
#' @importFrom stringr str_replace_all
#' @importFrom glue glue
#' @export

make_exceptions <- function(word, replacement) {

    str_replace_all(replacement, glue(" {str_to_title(word)}"), glue(" {word}"))

}

#' Capitalizing the first letter of a selected field entry
#'
#' @param vec A Bibtex object
#' @param field A selected field (e.g., "title")
#' @param exceptions A character vector that contains the words to avoid capitalization.
#' @return A Bibtex object where the selected filed is modified.
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_to_title
#' @importFrom stringr str_replace_all
#' @importFrom glue glue
#' @export

capitalize_field <- function(vec, field, exceptions = NULL) {

    target <- vec[str_detect(vec, field)]

    replacement <- str_extract_all(target, "\\{[^()]+\\}") %>% str_to_title()

    # exceptions <- c("for", "the", "in", "and", "to", "of", "tidyr", "dplyr")

    if (!is.null(exceptions)) {

        for (word in exceptions) {

            replacement <- make_exceptions(word, replacement)

        }

    }

    vec[str_detect(vec, field)] <- str_replace_all(target, "\\{[^()]+\\}", replacement)

    return(vec)
}

#' Capitalizing the first letter of a selected field entry in a Bibtex file
#'
#' @param file_path A file_path where a Bibtex file is located.r
#' @param field A selected field (e.g., "title")
#' @param exceptions A character vector that contains the words to avoid capitalization.
#' @return A Bibtex object where the selected filed is modified.
#' @export

cap_bib_field <- function(file_path, field, exceptions = NULL) {

    vec <- readLines(file_path)

    vec <- capitalize_field(vec, field, exceptions = NULL)

    return(vec)

}
