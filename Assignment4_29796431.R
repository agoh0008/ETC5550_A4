generate_arma <- function(n = 100, c = 0, phi = NULL, theta = NULL, sigma = 1) {
  
  # Default phi and theta to 0 if NULL
  if (is.null(phi)) phi <- numeric()
  if (is.null(theta)) theta <- numeric()
  
  # Check for stationarity 
  if (!(!any(abs(polyroot(c(1, -phi))) <= 1))) {
    stop("AR parameters do not satisfy stationarity condition.")
  }
  
  # Check for invertibility 
  if (!(!any(abs(polyroot(c(1, theta))) <= 1))) {
    stop("MA parameters do not satisfy invertibility condition.")
  }
  
  # Generate more data points to allow for burn-in
  extra_points <- max(length(phi), length(theta))  # Adjust based on the maximum lag
  total_points <- n + extra_points
  
  # Generate errors
  error <- rnorm(total_points, mean = 0, sd = sigma)
  
  # Set up vector for the response
  y <- numeric(total_points)
  
  # Generate remaining observations
  for (i in seq_along(y)) {
    # AR part
    ar_sum <- sum(ifelse(length(phi) > 0 && i > length(phi), 
                         phi * y[(i - 1):(i - length(phi))], 0))
    
    # MA part
    ma_sum <- sum(ifelse(length(theta) > 0 && i > length(theta), 
                         theta * error[(i - 1):(i - length(theta))], 0))
    
    y[i] <- c + ar_sum + ma_sum + error[i]
  }
  
  # Discard initial observations
  y <- y[(extra_points + 1):total_points]
  
  return(y)
}


# Testing Function

# library(fpp3)

# set.seed(1408)

# Define parameters
# n <- 500
# c <- 0
# phi <- 0.4
# theta <- c(0.3, -0.6)   
# phi <- NULL
# theta <- NULL
# phi = 0.4
# theta = 0
# sigma <- 1

# tsibble(time = 1:n, y = generate_arma(n = n, c = c,
#                                      phi = phi, theta = theta), 
#        index = time) |>
#  gg_tsdisplay(y, plot_type = "partial")

# generate_arma(n = n, c = c,
#              phi = phi, theta = theta)
