library(ggplot2)
library(tidyverse)
library(latex2exp)
library(SparseM)
source('C:/Users/janre/Documents/uni/10. Semester/Speciale/Kode/subgradient_optimization.R')
set.seed(100)

# Parameters
n <- 20 # Number of stocks to track
q <- 15 # Number of stocks selected for index fund
K <- 1000 # The number of test values for the execution part

# Create positive semidefinite correlation matrix
A <- matrix(rnorm(n^2), nrow = n, ncol = n)
A <- (t(A) %*% A) %>% cov2cor

# Find the indices of the q largest values in a vector in time complexity O(n). 
# Indices are not ordered with respect to size of associated value
indices_q_largest <- function(vec, q) {
  indices <- 1:q
  for(i in (q + 1):length(vec)) {
    if(any(vec[i] > vec[indices])) {
      indices[which.min(vec[indices])] <- i 
    }
  }
  
  return(indices)
}

# Generates a feasible solution x* for (M) on the basis of y obtained from 
# solving (M')
generate_x_star <- function(y) {
  x_star <- matrix(nrow = n, ncol = n)
  for(i in 1:n) {
    for(j in 1:n) {
      x_star[i, j] <- (j == which.max(t(A[i,]) * y))
    }
  }
  
  return(x_star)
}

# Calculates the lower bound for z by putting the solution obtained through (M')
# into the object function of (M)
z_underline <- function(x) {
  sum(A * x)
}

# The maximized value of the objective function in (M')
L <- function(u) {
  # Calculate all the C_j
  C <- numeric(n)
  for(i in 1:n) {
    C[i] <- sum((A[,i] - u) * ((A[,i] - u) > 0))
  }
  
  # Set y = 1 for q largest values of C_j
  indices <- indices_q_largest(C, q)
  y <- numeric(n)
  y[indices] <- 1
  
  # Set x_ij = y_j if rho_ij - u_i > 0, and x_ij = 0 otherwise
  x <- matrix(nrow = n, ncol = n)
  for(i in 1:n) {
    for(j in 1:n) {
      x[i, j] <- ((A[i, j] - u[i]) > 0) * y[j]
    }
  }
  
  return(list(L = sum(C * y + u), J = indices, x = x, y = y))
}

# Extraction function for L to extract the value L
extract_L <- function(obj) {
  obj$L 
}

# Function to use in subgradient descend
optim_L <- function(u) {
  extract_L(L(u)) 
}

#### Execution ####
z_overlines  <- numeric(K)
z_underlines <- numeric(K)
u <- rep(0, n)
start_from_iteration <- 1
for(k in 1:K) {
  cat(paste0("k = ", k, " out of max ", K, ":\n"))
  res <- subgradient_descent(f = optim_L, x = u, 
                             beta = 1, max_iters = k * 10,
                             start_from_k = start_from_iteration)
  u <- res$optimal_x
  x_star <- generate_x_star(L(u)$y)
  z_overlines[k]  <- L(u)$L
  z_underlines[k] <- z_underline(x_star)
  cat(paste0("\nObtained z_overline = ", z_overlines[k], 
             ", and z_underline = ", z_underlines[k], ".\n\n"))
  start_from_iteration <- res$iterations
}

# Plot the convergence lines for z_overline and z_underline
df <- data.frame(
  index = 1:K * 10,
  z_overlines = z_overlines,
  z_underlines = z_underlines
)

# Convert to long format
df_long <- df %>%
  pivot_longer(cols = c(z_overlines, z_underlines), 
               names_to = "variable", values_to = "value")

# Plot with ggplot
ggplot(df_long, aes(x = index, y = value, color = variable)) +
  geom_line(size = 1) +  # Line plot
  scale_color_manual(values = c("z_overlines" = "red", "z_underlines" = "blue"),
                     labels = c("z_overlines" = "z_overline", 
                                "z_underlines" = "z_underline")) +  # Custom colors
  labs(title = "Two Line Plot", x = "Max iterations", y = "Value", color = "Legend") +
  theme_minimal()
