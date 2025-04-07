################################################################################
#                                                                              #
# 07-04-2025: Bias-adjusted modulated realized covariance for integrated cov-  #
#             variance estimation
# - Author: Jan Reiter Sørensen                                                #
#                                                                              #
# An implementation of an ICov estimator based on the results of the article   #
# by K. Christensen et al (2010): Pre-averaging estimators of the ex-post cov- #
# ariance matrix in noisy diffusion models with non-synchronous data           #
#                                                                              #
################################################################################

##### Functions #####

# The weight function proposed by Podolskij and Vetter (2009): Estimation of
# volatility functionals in the simultaneous presence of microstructure noise 
# and jumps
g <- function(x) {
  return(min(c(x, 1 - x))) 
}

