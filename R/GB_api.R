usethis::use_package("httr")
# library(httr)
# library(jsonlite)
# library(xml2)

# ---- Main function ----
#' Query the FIA EVALIDator API and return results as a data frame
#'
#' Queries the USDA Forest Inventory and Analysis (FIA) EVALIDator API's
#' \code{fullreport} endpoint and parses the JSON response into a tidy
#' data frame. If one or two grouping variables are requested via
#' \code{GRP_BY_ATTRIB}, the corresponding group columns returned by the
#' API (\code{GRP1} and/or \code{GRP2}) are parsed into meaningful,
#' separate columns (e.g. splitting county FIPS codes and names, or
#' species codes and common/scientific names).
#'
#' @param EVAL_GRP Numeric or character. The FIA evaluation group ID
#'   (e.g. \code{12019} for Alabama 2019). Passed to the API's \code{wc}
#'   parameter.
#' @param ATTRIBUTE_NBR Numeric or character. The EVALIDator attribute/
#'   statistic number to query (e.g. \code{10} for aboveground biomass).
#'   Passed to the API's \code{snum} parameter.
#' @param GRP_BY_ATTRIB Character vector of length 0, 1, or 2 (default
#'   \code{"STATECD"}). Specifies which variable(s) to group results by.
#'   Must be one (or two) of: \code{"COUNTYCD"}, \code{"STATECD"},
#'   \code{"OWNCD"}, \code{"OWNGRPCD"}, \code{"FORTYPCD"}, \code{"SPCD"},
#'   \code{"SPGRPCD"}, \code{"EVALID"}. Case-insensitive. If length 2,
#'   the first element is used as the row grouping variable and the
#'   second as the column grouping variable. Set to \code{NULL} to
#'   request no grouping (results grouped only by \code{EVALID} on the
#'   API side).
#'
#' @return A data frame with one row per group combination (or one row
#'   total if no grouping is requested), including parsed identifier
#'   column(s) for each requested grouping variable followed by the
#'   API's estimate columns (typically \code{ESTIMATE}, \code{PLOT_COUNT},
#'   \code{SE}, \code{SE_PERCENT}, \code{VARIANCE}). Returns \code{NULL}
#'   if the API response contains no \code{estimates} element.
#'
#' @details
#' Column parsing by grouping variable:
#' \itemize{
#'   \item \code{COUNTYCD}: \code{STATE_ABBREV}, \code{STATECD},
#'     \code{COUNTYCD}, \code{COUNTY_NAME}
#'   \item \code{STATECD}: \code{STATECD}, \code{STATE_NAME}
#'   \item \code{OWNCD}: \code{OWNCD}, \code{OWNCD_NAME}
#'   \item \code{OWNGRPCD}: \code{OWNGRPCD}, \code{OWNGRPCD_NAME}
#'   \item \code{FORTYPCD}: \code{FORTYPCD}, \code{FORTYPCD_NAME}
#'   \item \code{SPCD}: \code{SPCD}, \code{COMMON_NAME},
#'     \code{SCIENTIFIC_NAME}
#'   \item \code{SPGRPCD}: \code{SPGRPCD}, \code{SPGRPCD_NAME}
#'   \item \code{EVALID}: \code{EVALID}, \code{EVALID_DESC}
#' }
#'
#' If the API does not return valid JSON (e.g. because \code{EVAL_GRP} is
#' not a valid evaluation ID), the function stops with an informative
#' error rather than failing on JSON/XML parsing.
#'
#' @examples
#' \dontrun{
#' # Aboveground biomass by county, Virginia 2019
#' agb_county <- GB_api(EVAL_GRP = 512019,
#'                          ATTRIBUTE_NBR = 10,
#'                          GRP_BY_ATTRIB = "COUNTYCD")
#'
#' # Aboveground biomass by species, Alabama 2019
#' agb_species <- GB_api(EVAL_GRP = 12019,
#'                           ATTRIBUTE_NBR = 10,
#'                           GRP_BY_ATTRIB = "SPCD")
#'
#' # Grouped by state and EVALID (two grouping variables)
#' agb_state_eval <- GB_api(EVAL_GRP = 242019,
#'                              ATTRIBUTE_NBR = 10,
#'                              GRP_BY_ATTRIB = c("STATECD", "EVALID"))
#' }
#'
#' @export
GB_api <- function(EVAL_GRP,
                      ATTRIBUTE_NBR,
                      GRP_BY_ATTRIB = "STATECD"){
  grp_lookup <- c(
    COUNTYCD  = "County code and name",
    STATECD   = "State code",
    OWNCD     = "Ownership class",
    OWNGRPCD  = "Ownership group",
    FORTYPCD  = "Forest type",
    SPCD      = "Species",
    SPGRPCD   = "Species group",
    EVALID    = "EVALID"
  )
  
  if(!is.null(GRP_BY_ATTRIB)){
    
    # Validate GRP_BY_ATTRIB values before doing anything else
    bad <- toupper(GRP_BY_ATTRIB)[!toupper(GRP_BY_ATTRIB) %in% names(grp_lookup)]
    if(length(bad) > 0){
      stop("Invalid GRP_BY_ATTRIB value(s): ", paste(bad, collapse = ", "),
           ". Must be one of: ", paste(names(grp_lookup), collapse = ", "))
    }
    
    if(length(GRP_BY_ATTRIB) != 1){
      if(length(GRP_BY_ATTRIB) == 2){
        rowVar <- grp_lookup[toupper(GRP_BY_ATTRIB)][1]
        colVar <- grp_lookup[toupper(GRP_BY_ATTRIB)][2]
      }else stop("GRP_BY_ATTRIB must have length 1 (rowVar) or 2 (rowVar, colVar).")
    }else {
      rowVar <- grp_lookup[toupper(GRP_BY_ATTRIB)]
      colVar <- ""
    }
  }else {colVar = ""; rowVar = ""}  
  
  url <- paste0(
    "https://apps.fs.usda.gov/fiadb-api/fullreport?",
    "snum=", ATTRIBUTE_NBR,
    "&wc=", EVAL_GRP,
    "&rselected=", utils::URLencode(rowVar),
    "&cselected=", utils::URLencode(colVar),
    "&outputFormat=NJSON"
  )
  
  message("Querying EVALIDator API.")
  # resp <- httr::GET(url)
  # 
  # x <- httr::content(resp,
  #                    as = "parsed",
  #                    encoding = "UTF-8")
  resp <- httr::GET(url)
  
  if (httr::http_error(resp)) {
    stop("EVALIDator API request failed with status ", httr::status_code(resp),
         ". Check that EVAL_GRP = ", EVAL_GRP, " is valid.")
  }
  
  content_type <- httr::http_type(resp)
  
  if (!grepl("json", content_type, ignore.case = TRUE)) {
    stop("EVALIDator API did not return JSON (got '", content_type, "'). ",
         "This usually means EVAL_GRP = ", EVAL_GRP, " is not a valid evaluation ID.")
  }
  
  x <- httr::content(resp, as = "parsed", encoding = "UTF-8")  
  if(is.null(x$estimates))
    return(NULL)
  
  # estimates <-
  #   as.data.frame(
  #     do.call(rbind, x$estimates)
  #   )
  # 
  # estimates <- data.frame(lapply(estimates, unlist),
  #                         stringsAsFactors = FALSE)
  
  estimates <- do.call(
    rbind,
    lapply(x$estimates, function(row) as.data.frame(row, stringsAsFactors = FALSE))
  )
  rownames(estimates) <- NULL
  
  # Split GRP1 and GRP2 strings as needed
  GRP1_VAR = names(rowVar)
  GRP2_VAR = names(colVar)
  
  g1_res <- NULL
  g2_res <- NULL
  
  if (!is.null(GRP1_VAR)) {
    g1_res <- parse_grp(GRP1_VAR, estimates$GRP1)
    estimates$GRP1 <- NULL
  }
  
  if (!is.null(GRP2_VAR)) {
    g2_res <- parse_grp(GRP2_VAR, estimates$GRP2)
    estimates$GRP2 <- NULL
  }
  
  parts <- Filter(Negate(is.null), list(g1_res, g2_res, estimates))
  return(do.call(cbind, parts))
}
