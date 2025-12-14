#' Fit baseline risk model for PATH analyses
#'
#' Fits an adjusted outcome model and computes the baseline
#' linear predictor with treatment set to control, as required
#' for PATH risk-based analyses.
#'
#' @param formula Outcome ~ treatment + covariates (no interactions)
#' @param data data.frame containing all variables
#' @param treatment character name of treatment variable
#' @param method currently only "logistic"
#'
#' @return A list with components:
#' \itemize{
#'   \item model: fitted rms::lrm object
#'   \item lp_no_tx: linear predictor with treatment set to control
#'   \item treatment: treatment variable name
#'   \item outcome: outcome variable name
#'   \item data: model frame used for fitting
#' }
#'
#' @export






fit_path_model <- function(formula, data, treatment, method = "logistic") {
  stopifnot(method == "logistic")

  if (!requireNamespace("rms", quietly = TRUE)) {
    stop("Package 'rms' is required")
  }

  mf <- model.frame(formula, data)
  outcome <- all.vars(formula)[1]

  fit <- rms::lrm(formula, data = data, x = TRUE, y = TRUE)

  newdata <- data
  newdata[[treatment]] <- 0

  lp_no_tx <- as.numeric(
    predict(fit, newdata = newdata, type = "lp")
  )

  list(
    model = fit,
    lp_no_tx = lp_no_tx,
    treatment = treatment,
    outcome = outcome,
    data = mf
  )
}



