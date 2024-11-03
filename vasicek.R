####
# R script for simulationg bond short rates with the Vasicek model. It includes
# functions to calibrate the Vasicek model, run simulations and derive yield
# curves. 
#
# Three major sources used for this are below. 
#
# http://delta9hedge.blogspot.com/2013/05/simulation-of-vasicek-interest-rates-in.html
# http://www.sitmo.com/article/calibrating-the-ornstein-uhlenbeck-model/
# http://quantcorner.wordpress.com/2013/11/17/least-squares-and-maximum-likelihood-estimation-calibration-with-r/

VasicekHelper <- function(r, kappa, theta, sigma, dt = 1/252) {
  # Helper function that calculates the next rate based on the discretization
  # of the Varice model. 
  #
  # Args: 
  #   r: The interest rate used to generate the next interest rate.
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   dt: The change in time between observations. Defaults to 1/252 because
  #       we assume generation of daily rates and there are 252 trading days 
  #       per year. 
  #
  # Returns:
  #   A vector of simulated short rates. 
  term1 <- exp(-1 * kappa * dt)
  term2 <- (sigma^2) * (1 - term1^2) / (2*kappa)
  result <- r*term1 + theta*(1-term1) + sqrt(term2)*rnorm(n=1)
  return(result)
}

VasicekSimulation <- function(N, r0, kappa, theta, sigma, dt = 1/252) {
  # Generates a single short rate simulation using the Vasicek model.
  #
  # Args: 
  #   N: The number of points to generate in each simulation. For example, 
  #      the number of days when simulating daily rates.
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   dt: The change in time between observations. Defaults to 1/252 because
  #       we assume generation of daily rates and there are 252 trading days 
  #       per year. 
  #
  # Returns:
  #   A vector of simulated short rates. 

  short.rates <- rep(0, N)
  short.rates[1] <- r0
  for (i in 2:N) {
    short.rates[i] <- VasicekHelper(short.rates[i - 1], kappa, theta, sigma, dt)
  }
  return(short.rates)
}

VasicekSimulations <- function(M, N, r0, kappa, theta, sigma, dt = 1/252) {
  # Generates several short rate simulations using the Vasicek model.
  #
  # Args: 
  #   M: The number of simulations to run. 
  #   N: The number of points to generate in each simulation. For example, 
  #      the number of days when simulating daily rates.
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   dt: The change in time between observations. Defaults to 1/252 because
  #       we assume generation of daily rates and there are 252 trading days 
  #       per year. 
  #
  # Returns:
  #   An N row by M column matrix of simulated short rates. 

  sim.mat <- matrix(nrow = N, ncol = M)
  for (i in 1:M) {
    sim.mat[, i] <- VasicekSimulation(N, r0, kappa, theta, sigma, dt)
  }
  return(sim.mat)
}

VasicekZeroCouponBondPrice <- function(r0, kappa, theta, sigma, years) {
  # Calculates th zero coupon bond price.
  #
  # Args:
  #   r0: The initial interest rate.
  #   kappa: The mean reversion rate.
  #   theta: The mean rate or long term rate.
  #   sigma: Volatility.
  #   years: The length or maturity of the bond.
  #
  # Returns:
  #   A decimal price of the bond (i.e. 0.98 for 98).

  b.vas <- (1-exp(-years*kappa)) / kappa
  a.vas <- (theta-sigma^2/(2*kappa^2))*(years-b.vas)+(sigma^2)/(4*kappa)*b.vas^2
  return(exp(-a.vas-b.vas*r0))
}

VasicekYieldCurve <- function(r0, kappa, theta, sigma, max.maturity=10) {
  # Produces a yield curve from the Vasicek model with maturities ranging
  # from 1 year to max.maturity.
  #
  # Args:
  #   r0: The initial interest rate.
  #   kappa: The mean reversion rate.
  #   theta: The mean rate or long term rate.
  #   sigma: Volatility.
  #   max.maturity: Maximum maturity in years (must be integer).
  #
  # Returns:
  #   A decimal price of the bond (i.e. 0.98 for 98).
  yields <- rep(0, max.maturity)
  for (y in 1:max.maturity) {
    yields[y] <- -log(VasicekZeroCouponBondPrice(r0, kappa, theta, sigma, y))/y
  }
  return(yields)
}


VasicekCalibration <- function(data, dt = 1/252) {
  # Calibrates the vasicek model using the maximum likelihood estimator. Details
  # about the maximum likelihood extomator are at the link below. 
  #
  # http://www.sitmo.com/article/calibrating-the-ornstein-uhlenbeck-model/
  #
  # TODO - Add in start and end dates. 
  #
  # Args:
  #   fred.ticker: Ticker used to download the historical rates from the Federal
  #                Reserve Bank of St Louis. Defaults to DSG3MO, the 3-Month 
  #                Treasury Constant Maturity Rate. 
  #   dt: The change in time between observations. Defaults to 1/252 because
  #       we assume generation of daily rates and there are 252 trading days 
  #       per year. 
  #
  # Returns:
  #   A vector of the form c(kappa, theta, sigma, r0), where kappa is the mean
  #   reversion rate, theta is the long-term rate/mean, sigma is the volatility
  #   and r0 is the last observed rate.
  #
  # Requires:
  #   quantmod

  calib_data <- xts(data, order.by = data$Date)
  calib_data$Date <- NULL
  calib_data <- drop(coredata(calib_data))
  calib_data <- as.numeric(calib_data)
  n <- length(calib_data)
  
  # do the calculations
  Sx <- sum(calib_data[1:(length(calib_data) - 1)])
  Sy <- sum(calib_data[2:length(calib_data)])
  Sxx <- as.numeric(crossprod(calib_data[1:(length(calib_data) - 1)],
                              calib_data[1:(length(calib_data) - 1)]))
  Sxy <- as.numeric(crossprod(calib_data[1:(length(calib_data) - 1)], 
                              calib_data[2:length(calib_data)]))
  Syy <- as.numeric(crossprod(calib_data[2:length(calib_data)], 
                              calib_data[2:length(calib_data)]))
  
  theta  <- (Sy * Sxx - Sx * Sxy) / (n* (Sxx - Sxy) - (Sx^2 - Sx*Sy) )
  kappa <- -log((Sxy - theta * Sx - theta * Sy + n * theta^2) / (Sxx - 2 * theta * Sx + n * theta^2)) / dt
  a <- exp(-kappa*dt)
  sigmah2 <- (Syy - 2 * a * Sxy + a^2 * Sxx - 2 * theta * (1-a) * (Sy - a * Sx) + n * theta^2 * (1 - a)^2)/n
  sigma <- sqrt(sigmah2 * 2 * kappa / (1 - a^2))
  
  r0 <- calib_data[length(calib_data)]
  
  return(c(kappa, theta, sigma, r0))
}

VasicekModel <- function(data, test_data, title, legend_pos) {
  N <- nrow(test_data)
  t <- (1:N)
  
  params <- VasicekCalibration(data, dt=1/N)
  kappa <- params[1]
  theta <- params[2]
  sigma <- params[3]
  r0 <- params[4]
  
  forecast <- VasicekSimulation(N, r0, kappa, theta, sigma, dt=1/N)
  test_data$fit <- forecast

  ggplot() +
    geom_line(data = data, aes(x = Date, y = number_sold, color = "Вихідні дані")) +
    geom_line(data = test_data, aes(x = Date, y = number_sold, color = "Тестові дані")) +
    geom_line(data = test_data, aes(x = Date, y = fit, color = "Прогноз за Васічеком")) +
    labs(title = title,
         x = "Дата",
         y = "Продажі") +
    scale_color_manual(values = c("Вихідні дані" = "black", "Тестові дані" = "grey",
                                  "Прогноз за Васічеком" = "blue")) +
    theme(legend.position = legend_pos,
          legend.background = element_rect(fill = "white", color = "black"),
          legend.title = element_blank())  
}

# # test with several (M = 20) simulations
# M <- 20
# test.mat <- VasicekSimulations(M, N, r0, kappa, theta, sigma)
# 
# # plot the paths
# plot(t, test.mat[, 1], type = 'l', main = 'Short Rates', ylab = 'rt', 
#      ylim = c(0, max(test.mat)), col = 1)
# for (count in 2:ncol(test.mat)) {
#   lines(t, test.mat[, count], col = count)
# }
# # plot the expected rate and +- 2 standard deviations (theoretical)
# expected <- theta + (r0 - theta)*exp(-kappa*t)
# stdev <- sqrt( sigma^2 / (2*kappa)*(1 - exp(-2*kappa*t)))
# lines(t, expected, lty=2) 
# lines(t, expected + 2*stdev, lty=2) 
# lines(t, expected - 2*stdev, lty=2)
# 
# # price the zero coupon bonds 
# VasicekZeroCouponBondPrice(r0, kappa, theta, sigma, years) 
# 
# # derive a yield curve 
# # (can do this for several values of r0 to get several curves)
# yields <- VasicekYieldCurve(r0, kappa, theta, sigma, 10)
# plot(1:10, yields, xlab = 'Maturity', type = 'l', ylab = 'Yield', main = 'Yield Curve')