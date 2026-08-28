#' returns information on whether scenarios passed critical validation checks
#'
#' @param data data.frame as returned from ``validateScenarios()``
#' @param yellowFail if set to TRUE a yellow check result of a critical
#'        variable will lead to the scenario not passing as validated
#'
#' @importFrom dplyr %>%
#' @export
validationPass <- function(data, yellowFail = FALSE) {

  fail_color <- if (yellowFail) c("red", "yellow") else "red"

  # see if any critical variables have failed per scenario and model
  pass <- data %>%
    dplyr::group_by(model, scenario) %>%
    dplyr::summarise(n_fail = sum(check %in% fail_color & critical == "yes"),
                     pass   = n_fail == 0,
                     .groups = "drop")


  return(pass)
}
