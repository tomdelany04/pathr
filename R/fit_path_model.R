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



