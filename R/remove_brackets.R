#' Remove content between brackets
#'
#' @param string A character vector to clean
#' @param bracket Type of bracket containing the content to remove.
#'   Must be one of "(" (default), "<", "["
#'
#' @returns A character vector of the same size of `string`, where
#'   elements has been stripped of their `bracket`ted content.
#'
#' @dev
#'
#' @examples
#' remove_brackets(
#'   "R source code is versioned with svn (and mirrored on GitHub)"
#' )
#'
#' remove_brackets(
#'   c("R <https://r-project.org>", "Python <https://python.org>"),
#'   bracket = "<"
#' )
#'
#' remove_brackets(
#'   "Content in square bracket [is removed] in the middle of a string",
#'   bracket = "["
#' )
remove_brackets <- function(string, bracket = c("(", "<", "[")) {
  bracket <- match.arg(bracket)

  closing_bracket <- switch(
    bracket,
    "<" = ">",
    "(" = ")",
    "[" = "]"
  )

  unbracketed <- stringi::stri_replace_all_regex(
    string,
    paste0("\\", bracket, "[^", closing_bracket, "]*\\", closing_bracket),
    ""
  )

  return(unbracketed)
}
