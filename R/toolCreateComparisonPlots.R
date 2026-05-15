#' Plot differences to previous version
#'
#' Create line plots comparing the new scenarios with the old
#'
#' @param new description
#' @param old description
#' @param tCutOff description
#'
#' @returns An invisible list of plots
#' @keywords internal
toolCreateComparisonPlots <- function(new = "current", old = "7.2.1", tCutOff = 2050) {
  rlang::check_installed("ggplot2")

  if (new == "current") {
    new_list <- toolGetAllDrivers()
    new_list_countries <- toolGetAllDrivers(aggregate = FALSE)
  } else {
    new_list <- readr::read_rds(glue::glue("compare_versions/scenarios/ssps_v{new}.Rds"))
    new_list_countries <- readr::read_rds(glue::glue("compare_versions/scenarios/ssps_v{new}_country.Rds"))
  }

  old_list <- readr::read_rds(glue::glue("compare_versions/scenarios/ssps_v{old}.Rds"))
  old_list_countries <- readr::read_rds(glue::glue("compare_versions/scenarios/ssps_v{old}_country.Rds"))


  my_plot <- function(new_list, old_list, i) {
    new_drivers_i <- purrr::map(new_list, ~ tibble::as_tibble(.x[i, , ])) %>%
      purrr::list_rbind(names_to = "driver") %>%
      dplyr::mutate(v = "new")

    old_drivers_i <-  purrr::map(old_list, ~ tibble::as_tibble(.x[i, , ])) %>%
      purrr::list_rbind(names_to = "driver") %>%
      dplyr::mutate(v = "old")

    old_drivers_i %>%
      dplyr::bind_rows(new_drivers_i) %>%
      dplyr::rename("scen" = "variable") %>%
      dplyr::mutate(driver = factor(.data$driver, levels = c("pop", "gdp", "gdppc", "lab", "urb"))) %>%
      dplyr::filter(dplyr::between(.data$year, 2015, tCutOff)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(.data$year, .data$value, col = .data$scen, linetype = .data$v)) +
      ggplot2::facet_wrap(~.data$driver, ncol = 3, scales = "free_y")
  }

  plots <- NULL
  for (i in getItems(new_list$pop, 1)) plots[[i]] <- my_plot(new_list, old_list, i)
  filename <- glue::glue("compare_versions/plots/comparisonPlots_{old}_vs_{new}_reg{tCutOff}.Rds")
  readr::write_rds(plots, file = filename)

  plots2 <- NULL
  for (i in getItems(new_list_countries$pop, 1)) plots2[[i]] <- my_plot(new_list_countries, old_list_countries, i)
  filename <- glue::glue("compare_versions/plots/comparisonPlots_{old}_vs_{new}_{tCutOff}.Rds")
  readr::write_rds(plots2, file = filename)

  invisible(c(plots, plots2))
}
