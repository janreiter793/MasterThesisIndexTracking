################################################################################
#                                                                              #
# 07-04-2025: Bias-adjusted modulated realized covariance for integrated cov-  #
#             variance estimation                                              #
# - Author: Jan Reiter Sørensen                                                #
#                                                                              #
# An implementation of an ICov estimator based on the results of the article   #
# by K. Christensen et al (2010): Pre-averaging estimators of the ex-post cov- #
# ariance matrix in noisy diffusion models with non-synchronous data           #
#                                                                              #
################################################################################
library(zoo)
library(magrittr)

##### Functions #####

# The weight function proposed by Podolskij and Vetter (2009): Estimation of
# volatility functionals in the simultaneous presence of microstructure noise 
# and jumps
g <- function(x) {
  sapply(x, FUN = function(y) {
      min(y, 1 - y)
    }) 
}

# Takes a matrix V of stock-price data, where each column is the prices process
# for each asset
preAverage <- function(V, kn = floor(sqrt(nrow(V) - 1))) {
  delta_V <- diff(V)
  
  # Conduct pre-averaging
  preAvgV <-
    apply(delta_V, 2, FUN = function(y) {
    rollapply(y,
              width = kn - 1,
              FUN = function(x) {
                  sum(g((1:(kn - 1)) / kn) * x)
                },
              align = "left",
              fill = NA)
    })
  
  # Remove na-rows and return
  return(preAvgV[!rowSums(is.na(preAvgV)),])
}

# calculate the bias-matrix used for bias-adjustment. Takes a matrix V of stock-
# price data, where columns are the prices processes for each asset.
Psi_hat_n <- function(V) {
  delta_V <- diff(V)
  (t(delta_V) %*% delta_V) / (2 * (nrow(V) - 1))
}

# Calculate the constant psi_1
psi_1 <- function(kn) {
  sum((g((1:kn) / kn) - g((0:(kn - 1)) / kn))^2) * kn 
}

# Calculate the constant psi_2
psi_2 <- function(kn) {
  sum(g((1:(kn - 1)) / kn)^2) / kn 
}

# Bias-adjustment matrix for the mrc-estimator
biasMatrix <- function(V, kn = floor(sqrt(nrow(V) - 1))) {
  psi_1(kn) / psi_2(kn) * Psi_hat_n(V)
}

# The MRC-estimator with bias-adjustment. To ensure positive semidefiniteness
# the MRC-estimate is projected onto the positive semidefinite cone by fact-
# orizing into spectral decomposition, and setting eigenvalues that are less
# than zero to zero. This method is due to Fan et al. Vast Volatility Matrix
# Estimation Using High-Frequency Data for Portfolio Selection
MRC <- function(V, kn = floor(sqrt(nrow(V) - 1)), 
                bias_adjust = TRUE, project_psd = TRUE) {
  n <- nrow(V) - 1
  preAvgV <- preAverage(V, kn = kn)
  
  # Obtain the bias-adjusted mrc-estimate
  MRC_val <- (n / (n - kn + 2)) * (1 / (psi_2(kn) * kn)) * t(preAvgV) %*% preAvgV
  if(bias_adjust) {            
    MRC_val <- (MRC_val - biasMatrix(V, kn = kn)) / 
               (1 - psi_1(kn) / (psi_2(kn) * 2 * n))
  }
  
  # Project the mrc-estimate into nearest positive semidefinite solution
  if(project_psd) {
    eigen_decomp <- eigen(MRC_val)
    eigen_decomp$values[eigen_decomp$values < 0] <- 0
    return(eigen_decomp$vectors %*% diag(eigen_decomp$values) %*% 
             t(eigen_decomp$vectors))
  }
  
  return(MRC_val)
}