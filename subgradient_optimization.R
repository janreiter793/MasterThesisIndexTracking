################################################################################
#                                                                              #
# 18-03-2025: Last updated                                                     #
# Written by Jan Reiter                                                        #
#                                                                              #
# Contains an implementation of subgradient descent optimization.              #
#                                                                              #
################################################################################

# TRUE: Script is run in test-mode
# FALSE: Functions are just loaded
TEST_MODE <- FALSE

################################## Parameters ##################################
EPSILON <- 1e-6 # Use for estimating subgradients : (f(x + EPSILON * e) - f(x))  
                #                                   / EPSILON
MAX_ITERS <- 100 # The maximum number of iterations performed by the subgradient
                  # algorithm
TOL <- 1e-6 # Tolerance parameter. Used to determine convergence for the sub-
            # gradient optimization algorithm when changes to x is less than TOL

################################## Functions ###################################
# Takes a convex function f, a point of the domain x, and an optional parameter
# epsilon. Approximates a subgradent of f in the point x, using (f(x + epsilon 
# * e) - f(x)) / epsilon. Returns the subgradient vector.
subgradient <- function(f, x, epsilon = EPSILON) {
  n <- length(x)  # Find the dimension of the domain
  g <- numeric(n) # Initialize the subgradient vector g
  
  # Estimate g entrywise by nudging x in each direction with a length of epsilon
  for(i in 1:n) {
    # Initialize the unit-vector with a one in entry i
    e <- rep(0, n)
    e[i] <- epsilon
    
    # Approximate the subgradient
    g[i] <- (f(x + e) - f(x)) / epsilon
  }
  
  return(g)
}

# The Subgradient descent algorithm. Takes the function f to minimize in. A 
# staring point x. Some optional value beta for the step size in the optim-
# ization. An optional value max_iters specifying the maximum number of itera-
# tions for which the algorithm should terminate after. A tolerance value tol,
# for which the algorithm terminates, when changes to x is less than tol.
subgradient_descent <- function(f, x, beta = 1, max_iters = MAX_ITERS, 
                                tol = TOL, start_from_k = 1) {
  # Store x values for visualization
  history <- matrix(nrow = length(x), ncol = max_iters + 2 - start_from_k)
  history[, 1] <- x
  
  # Start iterating
  for (iter in start_from_k:max_iters) {
    if(!(iter %% 10)) {
      cat(paste0(iter, " iterations out of max ", max_iters, ".\n")) 
    }
    
    g <- subgradient(f = f, x = x) # Estimate a subgradient
    x_new <- x - beta / iter * g   # Update x
    history[, iter + 2 - start_from_k] <- x_new    # Save this new estimate
    
    # Check for convergence
    if (sqrt(sum((x_new - x)^2)) < tol) {
      cat("Converged in", iter, "iterations\n")
      break
    }
    
    # Update the x
    x <- x_new
  }
  
  # Returns results as list containing the optimal x-value and the vector of
  # x-values the algorithm went through.
  return(list(optimal_x = x, history = history, iterations = iter))
}

################################## TEST ########################################

# Test the script
test_script <- function() {
  # Four test functions
  func1 <- function(x) { return(x^2) }       
  func2 <- function(x) { return(abs(x)) }      
  func3 <- function(x) { return(abs(x) + x^2) } 
  func4 <- function(x, delta = 1) {             
    if (abs(x) <= 1) {
      return(x^2)
    } else {
      return(10 * abs(x) - 9)
    }
  }
  
  # Do plots
  par(mfrow = c(2, 1))
  
  result <- subgradient_descent(f = func1, x = 5)
  plot(result$history, type = "o", col = "blue", pch = 16, main = "x history",
       xlab = "Iteration", ylab = "x value")
  abline(h = 0, col = "red", lty = 2)  # Optimal solution
  
  y <- numeric(length(result$history))
  for(i in 1:length(y)) {
    y[i] <- func1(result$history[i]) 
  }
  values <- seq(-1.5 * max(abs(result$history)), 1.5 * max(abs(result$history)), length.out = 50)
  fvalues <- numeric(length(values))
  for(i in 1:length(values)) {
    fvalues[i] <- func1(values[i]) 
  }
  plot(y = y, x = result$history, type = "o", col = "blue", pch = 16, main = "Function value history",
       xlab = "Iteration", ylab = "f(x) value",
       xlim = c(1.5 * min(values), 1.5 * max(values)), ylim = c(min(fvalues), max(fvalues)))
  lines(x = values, y = fvalues)
  abline(h = 0, col = "red", lty = 2)  # Optimal solution
  
  result <- subgradient_descent(f = func2, x = 5)
  plot(result$history, type = "o", col = "blue", pch = 16, main = "x history",
       xlab = "Iteration", ylab = "x value")
  abline(h = 0, col = "red", lty = 2)  # Optimal solution
  
  y <- numeric(length(result$history))
  for(i in 1:length(y)) {
    y[i] <- func2(result$history[i]) 
  }
  values <- seq(-1.5 * max(abs(result$history)), 1.5 * max(abs(result$history)), length.out = 50)
  fvalues <- numeric(length(values))
  for(i in 1:length(values)) {
    fvalues[i] <- func2(values[i]) 
  }
  plot(y = y, x = result$history, type = "o", col = "blue", pch = 16, main = "Function value history",
       xlab = "Iteration", ylab = "f(x) value",
       xlim = c(1.5 * min(values), 1.5 * max(values)), ylim = c(min(fvalues), max(fvalues)))
  lines(x = values, y = fvalues)
  abline(h = 0, col = "red", lty = 2)  # Optimal solution
  
  result <- subgradient_descent(f = func3, x = 5)
  plot(result$history, type = "o", col = "blue", pch = 16, main = "x history",
       xlab = "Iteration", ylab = "x value")
  abline(h = 0, col = "red", lty = 2)  # Optimal solution
  
  y <- numeric(length(result$history))
  for(i in 1:length(y)) {
    y[i] <- func3(result$history[i]) 
  }
  values <- seq(-1.5 * max(abs(result$history)), 1.5 * max(abs(result$history)), length.out = 50)
  fvalues <- numeric(length(values))
  for(i in 1:length(values)) {
    fvalues[i] <- func3(values[i]) 
  }
  plot(y = y, x = result$history, type = "o", col = "blue", pch = 16, main = "Function value history",
       xlab = "Iteration", ylab = "f(x) value",
       xlim = c(1.5 * min(values), 1.5 * max(values)), ylim = c(min(fvalues), max(fvalues)))
  lines(x = values, y = fvalues)
  abline(h = 0, col = "red", lty = 2)  # Optimal solution
  
  result <- subgradient_descent(f = func4, x = 3)
  plot(result$history, type = "o", col = "blue", pch = 16, main = "x history",
       xlab = "Iteration", ylab = "x value")
  abline(h = 0, col = "red", lty = 2)  # Optimal solution
  
  y <- numeric(length(result$history))
  for(i in 1:length(y)) {
    y[i] <- func4(result$history[i]) 
  }
  values <- seq(-1.5 * max(abs(result$history)), 1.5 * max(abs(result$history)), length.out = 50)
  fvalues <- numeric(length(values))
  for(i in 1:length(values)) {
    fvalues[i] <- func4(values[i]) 
  }
  plot(y = y, x = result$history, type = "o", col = "blue", pch = 16, main = "Function value history",
       xlab = "Iteration", ylab = "f(x) value",
       xlim = c(1.5 * min(values), 1.5 * max(values)), ylim = c(min(fvalues), max(fvalues)))
  lines(x = values, y = fvalues)
  abline(h = 0, col = "red", lty = 2)  # Optimal solution
  
}

# If test-mode is true, then run test-function
if(TEST_MODE) { 
  test_script()
}