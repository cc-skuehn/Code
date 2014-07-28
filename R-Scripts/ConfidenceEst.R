# compute confidence interval

# we need as input
# number of samples = nof_s, default = 10
# alpha = al, default = 0.01, probability that ex_est is in the confidence interval
# estimator for standard deviation, default = 0.1 (computed from the nof_s samples)
# estimator for expectation value, default = 0.9 (computed from the nof_s samples)

conf_est <- function(nof_s=10,al = 0.01,sd_est = 0.1,ex_est = 0.9) {

  # degrees of freedom = df = nof_s-1
  df=nof_s-1
  # quantile of t distribution
  q_al<-qt(1-al/2,df)
  # max difference with probability alpha
  max_d <- q_al*sd_est/sqrt(nof_s)
  # confidence interval
  ex_left <- ex_est - max_d
  ex_right <- ex_est + max_d
  print(paste("alpha:", al,"expectation value:",ex_est,"standard deviation:", sd_est))
  print(paste("Confidence interval",ex_left,ex_est,ex_right))
  ret_val <- c(ex_left,ex_right)
}
