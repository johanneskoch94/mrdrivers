#' Get all drivers
#'
#' Return all drivers and write them to file if version not set to "current".
#'
#' @inheritParams calcGDP
#' @inheritParams madrat::calcOutput
#' @param version version tag to add to the file name.
#'
#' @returns A list of magpie objects
#' @keywords internal
toolGetAllDrivers <- function(scenario = "SSPs", aggregate = TRUE, average2020 = FALSE, version = "current") {
  x <- list(
    "pop" = calcOutput("Population", scenario = scenario, aggregate = aggregate),
    "gdp" = calcOutput("GDP", scenario = scenario, aggregate = aggregate, average2020 = average2020),
    "gdppc" = calcOutput("GDPpc", scenario = scenario, aggregate = aggregate, average2020 = average2020),
    "lab" = calcOutput("Labour", scenario = scenario, aggregate = aggregate),
    "urb" = calcOutput("Urban", scenario = scenario, aggregate = aggregate)
  )
  if (version != "current") {
    readr::write_rds(x, glue::glue("compare_versions/scenarios/{tolower(scenario)}_v{version}.Rds"))
  }
  x
}
