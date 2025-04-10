#' Read-in data from the Shared Socioeconomic Pathways (SSP)
#'
#' Read-in SSP data and store as magclass object. Filter for subtype and subset in the convert function to use common
#' read cache (speeds up the computations).
#'
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()]
#' @examples \dontrun{
#' readSource("SSP", subtype = "gdp")
#' }
#' @order 1
readSSP <- function() {
  myColTypes <- c(rep.int("text", 5), rep.int("numeric", 31))
  x <- readxl::read_xlsx("ssp_basic_drivers_release_3.0.1_full.xlsx",
                         sheet = "data",
                         col_types = myColTypes,
                         progress = FALSE) %>%
    tidyr::unite("Model.Scenario.Variable.Unit", c("Model", "Scenario", "Variable", "Unit"), sep = ".") %>%
    tidyr::pivot_longer(cols = tidyselect::where(is.numeric), names_to = "year")

  # The above SSP release 3.0.1 does not have any data on urban population share. So we get that from an older release.
  myColTypes <- paste(c(rep.int("c", 5), rep.int("d", 41)), collapse = "")
  xUrb <- readr::read_csv("SspDb_country_data_2013-06-12.csv.zip", col_types = myColTypes, progress = FALSE) %>%
    dplyr::filter(.data$MODEL == "NCAR", .data$VARIABLE == "Population|Urban|Share") %>%
    tidyr::unite("Model.Scenario.Variable.Unit",  c("MODEL", "SCENARIO", "VARIABLE", "UNIT"), sep = ".") %>%
    # Drop columns (years) with only NAs
    dplyr::select(tidyselect::vars_select_helpers$where(~ !all(is.na(.x)))) %>%
    tidyr::pivot_longer(cols = tidyselect::where(is.numeric), names_to = "year") %>%
    # Convert Region codes now (normally only done in convert) to harmonize with region codes in x. Use custom match
    # with the country.names in x to make sure that when the country.names are converted to iso3c in convert,
    # no duplicates appear.
    dplyr::mutate("Region" = countrycode::countrycode(.data$REGION,
                                                      "iso3c",
                                                      "country.name",
                                                      custom_match = c("BIH" = "Bosnia and Herzegovina",
                                                                       "COG" = "Congo",
                                                                       "COD" = "Democratic Republic of the Congo",
                                                                       "CIV" = "C\u00f4te d'Ivoire",
                                                                       "MMR" = "Myanmar",
                                                                       "PSE" = "Palestine",
                                                                       "RUS" = "Russian Federation",
                                                                       "TTO" = "Trinidad and Tobago",
                                                                       "VNM" = "Viet Nam")),
                  .keep = "unused")

  dplyr::bind_rows(x, xUrb) %>%
    as.magpie(spatial = "Region", temporal = "year", tidy = TRUE, filter = FALSE)
}

#' @rdname readSSP
#' @order 2
#' @param x MAgPIE object returned from readSSP
#' @param subtype A string, either "all", "gdp", "pop", "lab", "urb"
#' @param subset A vector of strings designating the scenarios. Defaults to c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5").
#'   "Historical Reference" is also available as a scenario.
convertSSP <- function(x, subtype = "all", subset = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) {
  if (!subtype %in% c("all", "gdp", "pop", "lab", "urb")) {
    stop(glue("Bad input for readSSP. Invalid 'subtype' argument. Available subtypes are 'all', 'gdp', 'pop', 'lab' \\
              and 'urb'."))
  }

  if (subtype == "gdp") {
    x <- mselect(x,
                 Model = "OECD ENV-Growth 2023",
                 Scenario = subset,
                 Variable = "GDP|PPP",
                 Unit = "billion USD_2017/yr")
    # Convert from billions to millions
    x <- x * 1e3
  }
  if (subtype == "pop") {
    x <- mselect(x, Model = "IIASA-WiC POP 2023", Scenario = subset, Variable = "Population", Unit = "million")
  }
  if (subtype == "lab") {
    # Choose only age groups between 15 and 64
    agegrps <- getNames(x, dim = "Variable")[grepl("^Population(.*)(19|24|29|34|39|44|49|54|59|64)$",
                                                   getNames(x, dim = "Variable"))]
    x <- mselect(x, Model = "IIASA-WiC POP 2023", Scenario = subset, Variable = agegrps, Unit = "million")
  }
  if (subtype == "urb") {
    x <- mselect(x, Model = "NCAR", Variable = "Population|Urban|Share", Unit = "%")
    # Clean up Scenario names before filtering with subset
    getNames(x, dim = "Scenario") <- sub("_.*", "", getNames(x, dim = "Scenario"))
    x <- mselect(x, Scenario = subset)
    # Convert from percentage to share
    x <- x / 100
  }

  # Reduce dimension by summation when possible
  if (subtype != "all") x <- dimSums(x, dim = c("Model", "Variable", "Unit"))

  # Drop regions (anything with brackets, or called "World") and convert to iso3c. Use custom match for the Federal
  # States of Micronesia (= FSM)
  x <- x[!grepl("\\(|World", getCells(x)), , ]
  getCells(x) <- countrycode::countrycode(getCells(x), "country.name", "iso3c", custom_match = c("Micronesia" = "FSM"))


  toolGeneralConvert(x, useDefaultSetNames = subtype != "all")
}

#' @rdname readSSP
#' @order 3
downloadSSP <- function() {
  stop("Manual download of SSP data required!")
  # Compose meta data
  list(url           = "https://data.ece.iiasa.ac.at/ssp/#/downloads",
       doi           = "-",
       title         = "SSP projections",
       description   = "SSP projections Release 3.0.1, March2024",
       unit          = "-",
       author        = "IIASA, OECD, Wittgenstein Center",
       release_date  = "2024",
       license       = "-",
       comment       = "Manual download required! Accessed on the 15.04.2024.")
}
