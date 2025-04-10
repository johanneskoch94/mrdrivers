#' @rdname calcGDPPast
calcPopulationPast <- function(pastData = toolGetScenarioDefinition("Population", "SSPs")$pastData) {
  toolCheckUserInput("PopulationPast", as.list(environment()))

  # Map over components of pastData.
  data <- purrr::map(unlist(strsplit(pastData, "-")),
                     ~calcOutput("InternalPopulationPast", pastData = .x, aggregate = FALSE, supplementary = TRUE)) %>%
    toolListFillWith()

  # Fill in trailing zeros with closest value
  data$x <- toolInterpolateAndExtrapolate(data$x)
  data
}

calcInternalPopulationPast <- function(pastData) {
  data <- switch(
    pastData,
    "WDI"       = readSource("WDI", "pop"),
    "UN_PopDiv" = readSource("UN_PopDiv", "pop", "estimates"),
    "MI"        = readSource("MissingIslands", "pop"),
    stop("Bad input for calcPopulationPast. Invalid 'pastData' argument.")
  )

  getNames(data) <- pastData

  list(x = data, weight = NULL, unit = "million", description = glue("{pastData} data"))
}



#' @rdname calcGDPPast
calcLabourPast <- function(pastData = toolGetScenarioDefinition("Labour", "SSPs")$pastData) {
  toolCheckUserInput("LabourPast", as.list(environment()))
  # Map over components of pastData.
  purrr::map(unlist(strsplit(pastData, "-")),
             ~calcOutput("InternalLabourPast", pastData = .x, aggregate = FALSE, supplementary = TRUE)) %>%
    toolListFillWith()
}

calcInternalLabourPast <- function(pastData) {
  data <- switch(
    pastData,
    "WDI"       = readSource("WDI", "lab"),
    "UN_PopDiv" = readSource("UN_PopDiv", "lab", "estimates"),
    "SSPs"      = readSource("SSP", "lab", "Historical Reference"),
    stop("Bad input for calcLabourPast. Invalid 'pastData' argument.")
  )

  getNames(data) <- pastData

  list(x = data, weight = NULL, unit = "million", description = glue("{pastData} data"))
}


#' @rdname calcGDPPast
calcUrbanPast <- function(pastData = toolGetScenarioDefinition("Urban", "SSPs")$pastData) {
  data <- switch(
    pastData,
    "WDI" = readSource("WDI", "urb") / 100,
    stop("Bad input for calcUrbanPast. Invalid 'pastData' argument.")
  )

  # TWN is missing an urban share. Give it that of HKG.
  if (all(data["TWN", , ] == 0)) {
    data["TWN", , ] <- data["HKG", , ]
  }

  getNames(data) <- pastData

  weight <- calcOutput("PopulationPast", pastData = pastData, aggregate = FALSE)
  getNames(weight) <- getNames(data)

  list(x = data,
       weight = weight,
       unit = "share of population",
       description = glue("{pastData} data (with missing TWN values set to that of HKG)"))
}
