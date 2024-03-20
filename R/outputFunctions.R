#' returns information on whether scenarios passed critical validation checks
#'
#' @param data data.frame as returned from ``validateScenarios()``
#' @param yellowFail if set to TRUE a yellow check result of a critical
#'        variable will lead to the scenario not passing as validated
#'
#' @importFrom dplyr %>%
#' @export
validationPass <- function(data, yellowFail = FALSE) {

  fail_color <- ifelse(yellowFail, c("red", "yellow"), "red")

  # see if any critical variables have failed per scenario and model
  pass <- data %>%
    dplyr::filter(check %in% fail_color, critical == "yes") %>%
    dplyr::group_by(model, scenario) %>%
    dplyr::summarize(pass = dplyr::n() == 0, n_fail = dplyr::n())

  return(pass)
}
