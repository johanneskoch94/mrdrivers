#' @rdname calcPopulationPast
#' @param LabourPast A string designating the source for the historical working-age population data.
#' Available sources are:
#' \itemize{
#'   \item "WDI": World development indicators from the World Bank
#' }
calcLabourPast <- function(LabourPast = "WDI") { # nolint
  # Check user input
  toolCheckUserInput("LabourPast", as.list(environment()))
  # Call calcInternalPopulationFuture function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(list("LabourPast" = unlist(strsplit(LabourPast, "-"))),
              ~calcOutput("InternalLabourPast", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce(mbindOrFillWith = "fillWith")
}

calcInternalLabourPast <- function(LabourPast) { # nolint
  x <- switch(
    LabourPast,
    "WDI"  = readSource("WDI", "SP.POP.1564.TO"),
    "SSPs" = calcOutput("InternalLabourPastSSPs", aggregate = FALSE),
    stop("Bad input for calcLabour. Invalid 'LabourPast' argument.")
  )

  # Apply finishing touches to combined time-series
  x <- toolFinishingTouches(x)

  # Hopefully temporary: rename lab scenarios pop. Necessary for REMIND to work.
  getNames(x) <- sub("lab_", "pop_", getNames(x))

  list(x = x, weight = NULL, unit = "million", description = glue("{LabourPast} data."))
}

calcInternalLabourPastSSPs <- function() {
  x <- readSource("SSP", "lab")[, , "Historical Reference"]
  # Remove years which only contain 0
  x <- x[, !apply(x, 2, function(y) all(y == 0)), ]
  getNames(x) <- paste0("lab_", getNames(x))
  list(x = x, weight = NULL, unit = "million", description = "Labor from SSPs")
}
