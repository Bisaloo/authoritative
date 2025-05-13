#' Parse the `Authors@R` field from a DESCRIPTION file
#'
#' Parse the `Authors@R` field from a DESCRIPTION file into a `person` object
#'
#' @param authors_r_string A character containing the `Authors@R` field from a
#'   `DESCRIPTION` file
#'
#' @return A `person` object, or a `list` of `person` objects of length equals
#' to the length of `authors_r_string`
#'
#' @examples
#' # Read from a DESCRIPTION file directly
#' pkg_description <- system.file("DESCRIPTION", package = "authoritative")
#' authors_r_pkg <- read.dcf(pkg_description, "Authors@R")
#'
#' parse_authors_r(authors_r_pkg)
#'
#' # Read from a database of CRAN metadata
#' cran_epidemiology_packages |>
#'   subset(!is.na(`Authors@R`), `Authors@R`, drop = TRUE) |>
#'   parse_authors_r() |>
#'   head()
#'
#' @export
parse_authors_r <- function(authors_r_string) {
  # Sanitize input from pkgsearch / crandb
  authors_r_string <- stringi::stri_replace_all_fixed(
    authors_r_string,
    "<U+000a>",
    " "
  )

  authors_persons <- lapply(str2expression(authors_r_string), eval)

  # Malformed Authors@R field
  is_person <- vapply(authors_persons, \(x) inherits(x, "person"), logical(1))
  authors_persons[!is_person] <- NA

  # Drop extra comments
  authors_persons[!is.na(authors_persons)] <- lapply(
    authors_persons[!is.na(authors_persons)],
    function(x) {
      # We cannot use *apply() directly because it doesn't recreate a nice
      # person object (as a list of person objects). Hence why we "manually"
      # recreate it via c(person, person)
      no_comments <- do.call(
        c,
        # person setter methods lose the comment field names so we unclass() and
        # reclass instead
        lapply(x, function(y) {
          w <- unclass(y)
          w[[1]]$comment <- y$comment[names(y$comment) %in% c("ORCID", "ROR")]
          if (length(w[[1]]$comment) == 0) {
            w[[1]]$comment <- NULL
          }
          class(w) <- class(y)
          return(w)
        })
      )
      return(no_comments)
    }
  )

  if (length(authors_persons) == 1) {
    authors_persons <- authors_persons[[1]]
  }

  return(authors_persons)
}
