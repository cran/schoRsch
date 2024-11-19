bimod_coef <- function(data, moments=FALSE, na.rm = TRUE) {
  # input checks
  x <- data
  if (any(is.na(x)) & !na.rm) {
    return(NA)
  }
  x <- x[!is.na(x)]
  if (length(x) < 1) {
    return(NA)
  }
  if (!is.numeric(x)) {
    warning("Supplied x is not numeric!")
    return(NA)
  }

  # helper variables to avoid redundant computations
  n <- length(x)
  x <- x - mean(x)     # center x => easier to compute moments

  # m_3 is third moment, i.e., skew
  skew_prefactor <- (n*sqrt(n-1)/(n-2))
  m_3 <- skew_prefactor  * (sum(x^3) / sum(x^2)^(3/2))
  #print(m_3)

  # m_4 is fourth moment, i.e., kurtosis
  kurtosis_prefactor <- ( ((n+1)*n*(n-1)) / ((n-2)*(n-3)) )
  kurtosis_adjustmentfactor <- (3 * ((n-1)^2) / ((n-2) * (n-3)))
  m_4 <-  kurtosis_prefactor * ( sum(x^4)/(sum(x^2)^2) ) - kurtosis_adjustmentfactor
  #print(m_4)

  # actual BC
  bc_numerator <- (m_3^2) + 1
  bc_denominator <- m_4 + (3 * ((n-1)^2) / ((n-2)*(n-3)))
  if (moments) {
    bcout <- c(bc_numerator/bc_denominator,m_3,m_4)
    return(bcout)
  } else {
    return(bc_numerator/bc_denominator)
  }
}


