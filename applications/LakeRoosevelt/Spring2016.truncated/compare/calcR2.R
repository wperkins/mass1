# -------------------------------------------------------------
# file: calcR2.R
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created August 30, 2001 by William A. Perkins
# Last Change: 2019-09-30 07:02:45 d3g096
# -------------------------------------------------------------

Model.CheckVectors <- function(observed, simulated, name = "") {
  nobs <- length(observed)
  nsim <- length(simulated)

  if (nobs != nsim) {
    msg <- "";
    if (length(name) > 0) msg <- "Model.R2: "
    msg <- paste(msg, "error: observed and simulated vectors",
                 "must be the same length (", nobs, "!=", nsim, ")")
    stop(msg)
  }
  TRUE
}
# -------------------------------------------------------------
# Model.R2
# This computes a psuedo-correlation coefficient for the simulated
# values
# -------------------------------------------------------------
Model.R2 <- function(observed, simulated) {
  junk <- Model.CheckVectors(observed, simulated, "Model.R2")
  n <- length(simulated)
  meanobs <- mean(observed)
  meansim <- mean(simulated)
  rsquared <-
    ((sum(simulated*observed)/n - meansim*meanobs)/
     sqrt((sum(observed*observed)/n - meanobs*meanobs)*
          (sum(simulated*simulated)/n - meansim*meansim)))^2
  rsquared
}

# -------------------------------------------------------------
# Model.Bias
# -------------------------------------------------------------
Model.Bias <- function(observed, simulated) {
  junk <- Model.CheckVectors(observed, simulated, "Model.Bias")
  meanobs <- mean(observed)
  meansim <- mean(simulated)
  bias <- meansim - meanobs
  bias
}

# -------------------------------------------------------------
# Model.RMS
# The root-mean-square error 
# -------------------------------------------------------------
Model.RMS <- function(observed, simulated) {
  junk <- Model.CheckVectors(observed, simulated, "Model.RMS")
  rms <- sqrt(sum((simulated - observed)*
                  (simulated - observed))/length(simulated))
  rms
}

# -------------------------------------------------------------
# Model.AME
# -------------------------------------------------------------
Model.AME <- function(observed, simulated) {
  junk <- Model.CheckVectors(observed, simulated, "Model.RMS")
  ame <- mean(abs(simulated - observed))
  ame
}

# -------------------------------------------------------------
# Model.AE
# -------------------------------------------------------------
Model.AE <- function(observed, simulated) {
  junk <- Model.CheckVectors(observed, simulated, "Model.RMS")
  ae <- mean(simulated - observed)
  ae
}

# -------------------------------------------------------------
# Model.Estddev
# -------------------------------------------------------------
Model.Estddev <- function(observed, simulated) {
  junk <- Model.CheckVectors(observed, simulated, "Model.RMS")
  std <- sd(simulated - observed)
  std
}

# -------------------------------------------------------------
# Model.NSE
# Nash-Sutcliffe model efficiency coefficient
# -------------------------------------------------------------
Model.NSE <- function(observed, simulated) {
  numer = sum((simulated - observed)^2.0)
  oavg = mean(observed)
  denom = sum((simulated - oavg)^2.0)
  nse = 1.0 - numer/denom
  return(nse)
}
