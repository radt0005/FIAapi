# ---- Parser functions for each grouping variable ----

#' Parse COUNTYCD group strings into state/county identifiers
#'
#' @param x Character vector of raw \code{GRP1}/\code{GRP2} strings from
#'   the EVALIDator API, e.g. \code{"`51001 51001 VA Accomack"}.
#' @return A data frame with columns \code{STATE_ABBREV}, \code{STATECD},
#'   \code{COUNTYCD}, \code{COUNTY_NAME}.
#' @keywords internal
#' 
parse_COUNTYCD <- function(x) {
  splits <- strsplit(x, " ", fixed = TRUE)
  mat <- t(vapply(splits, function(s) s[1:3], character(3)))
  fips <- as.numeric(mat[, 2])
  data.frame(
    STATE_ABBREV = mat[, 3],
    STATECD      = fips %/% 1000,
    COUNTYCD     = fips %% 1000,
    COUNTY_NAME  = sub("^\\S+\\s+\\S+\\s+\\S+\\s+", "", x),
    stringsAsFactors = FALSE
  )
}

#' Parse FORTYPCD group strings into code and name
#'
#' @param x Character vector of raw group strings, e.g.
#'   \code{"`0161 Loblolly pine"}.
#' @return A data frame with columns \code{FORTYPCD}, \code{FORTYPCD_NAME}.
#' @keywords internal
#' 
parse_FORTYPCD <- function(x) {
  data.frame(
    FORTYPCD      = as.numeric(sub("^`(\\d+).*", "\\1", x)),
    FORTYPCD_NAME = trimws(sub("^`\\d+\\s*", "", x)),
    stringsAsFactors = FALSE
  )
}

#' Parse STATECD group strings into code and name
#'
#' @param x Character vector of raw group strings, e.g.
#'   \code{"`0001 1 Alabama"}.
#' @return A data frame with columns \code{STATECD}, \code{STATE_NAME}.
#' @keywords internal
#' 
parse_STATECD <- function(x) {
  data.frame(
    STATECD    = as.numeric(sub("^`\\d+\\s+(\\d+)\\s+.*", "\\1", x)),
    STATE_NAME = sub("^`\\d+\\s+\\d+\\s+", "", x),
    stringsAsFactors = FALSE
  )
}

#' Parse OWNCD group strings into code and name
#'
#' @param x Character vector of raw group strings, e.g.
#'   \code{"`0001 National Forest"}.
#' @return A data frame with columns \code{OWNCD}, \code{OWNCD_NAME}.
#' @keywords internal
#' 
parse_OWNCD <- function(x) {
  data.frame(
    OWNCD      = as.numeric(sub("^`(\\d+).*", "\\1", x)),
    OWNCD_NAME = trimws(sub("^`\\d+\\s*", "", x)),
    stringsAsFactors = FALSE
  )
}

#' Parse OWNGRPCD group strings into code and name
#'
#' @param x Character vector of raw group strings, e.g.
#'   \code{"`0001 National Forest"}.
#' @return A data frame with columns \code{OWNGRPCD}, \code{OWNGRPCD_NAME}.
#' @keywords internal
#' 
parse_OWNGRPCD <- function(x) {
  data.frame(
    OWNGRPCD      = as.numeric(sub("^`(\\d+).*", "\\1", x)),
    OWNGRPCD_NAME = trimws(sub("^`\\d+\\s*", "", x)),
    stringsAsFactors = FALSE
  )
}

#' Parse SPCD group strings into code, common name, and scientific name
#'
#' @param x Character vector of raw group strings, e.g.
#'   \code{"`0068 SPCD 0068 - eastern redcedar (Juniperus virginiana)"}.
#' @return A data frame with columns \code{SPCD}, \code{COMMON_NAME},
#'   \code{SCIENTIFIC_NAME}.
#' @keywords internal
#' 
parse_SPCD <- function(x) {
  code <- as.numeric(sub("^`(\\d+).*", "\\1", x))
  
  # Strip the leading `CODE SPCD CODE - ` prefix
  rest <- sub("^`\\d+\\s+SPCD\\s+\\d+\\s*-\\s*", "", x)
  
  # Scientific name is inside the trailing parentheses
  scientific <- sub(".*\\(([^()]*)\\)\\s*$", "\\1", rest)
  
  # Common name is everything before the trailing parentheses
  common <- trimws(sub("\\s*\\([^()]*\\)\\s*$", "", rest))
  
  data.frame(
    SPCD            = code,
    COMMON_NAME     = common,
    SCIENTIFIC_NAME = scientific,
    stringsAsFactors = FALSE
  )
}

#' Parse SPGRPCD group strings into code and name
#'
#' @param x Character vector of raw group strings, e.g.
#'   \code{"`00001 Longleaf and slash pines"}.
#' @return A data frame with columns \code{SPGRPCD}, \code{SPGRPCD_NAME}.
#' @keywords internal
#' 
parse_SPGRPCD <- function(x) {
  data.frame(
    SPGRPCD      = as.numeric(sub("^`(\\d+).*", "\\1", x)),
    SPGRPCD_NAME = trimws(sub("^`\\d+\\s*", "", x)),
    stringsAsFactors = FALSE
  )
}

#' Parse EVALID group strings into EVALID code and description
#'
#' @param x Character vector of raw group strings, e.g.
#'   \code{"`0001 012019 Alabama 2019"}.
#' @return A data frame with columns \code{EVALID} (character, to
#'   preserve leading zeros) and \code{EVALID_DESC}.
#' @keywords internal
#' 
parse_EVALID <- function(x) {
  data.frame(
    EVALID      = sub("^`\\d+\\s+(\\S+)\\s+.*", "\\1", x),
    EVALID_DESC = sub("^`\\d+\\s+\\S+\\s+", "", x),
    stringsAsFactors = FALSE
  )
}

#' Fallback parser for grouping variables without a custom parser
#'
#' @param x Character or numeric vector of raw group values.
#' @param var_name Character. Name to assign to the resulting column.
#' @return A single-column data frame named \code{var_name}.
#' @keywords internal
#' 
# Fallback for grouping vars without a custom parser yet
parse_default <- function(x, var_name) {
  df <- data.frame(x, stringsAsFactors = FALSE)
  names(df) <- var_name
  df
}

parser_lookup <- list(
  COUNTYCD = parse_COUNTYCD,
  FORTYPCD = parse_FORTYPCD,
  STATECD  = parse_STATECD,
  OWNCD    = parse_OWNCD,
  OWNGRPCD = parse_OWNGRPCD,
  SPCD     = parse_SPCD,
  SPGRPCD  = parse_SPGRPCD,
  EVALID   = parse_EVALID
)

# ---- Dispatch table ----

#' Dispatch a group variable's raw values to its parser function
#'
#' Looks up \code{var_name} in \code{parser_lookup} and applies the
#' corresponding parser, falling back to \code{parse_default()} if no
#' custom parser is registered for that grouping variable.
#'
#' @param var_name Character. The grouping variable name (e.g.
#'   \code{"COUNTYCD"}), or \code{NULL} if no grouping was requested.
#' @param values Character vector of raw group values to parse.
#' @return A data frame of parsed columns for that grouping variable.
#' @keywords internal
#' 
parse_grp <- function(var_name, values) {
  if (!is.null(var_name) && var_name %in% names(parser_lookup)) {
    parser_lookup[[var_name]](values)
  } else {
    parse_default(values, var_name)
  }
}

