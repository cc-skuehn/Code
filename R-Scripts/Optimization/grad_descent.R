# Gradient Descent using Armijo rule - Basics

grad_descent <- function(start_val=NA, f_name=NULL, grad_f_name=NULL, accuracy=1e-6, max_iterations=100000){
  
  # TODO
  x = start_val # vector in R^n 
  fx = f_name(x) # real number
  gfx = grad_f_name(x) # vector in R^n
  tol = accuracy * sqrt(sum(gfx^2)) # tolerance, stopping criterion, relative to starting point, but:
  if (tol < accuracy) tol = accuracy # stopping criterion should not be too strict, e.g. if we start near the optimum, the gradient can already be almost zero
  print(paste("Start at function value:",fx))
  iter = 0 # iteration counter
  # Start loop, check stopping/running conditions
  while (sqrt(sum(gfx^2)) > tol & iter < max_iterations) {
      
    # Stepsize: Armijo rule
    sigma = 0.1
    alpha = 0.5
    # TODO: Armijo
    gfx = grad_f_name(x)
    j = armijo_rule(sig=sigma, al=alpha,x_val=x,grad_x_val=gfx,direction=-gfx,f_arm=f_name)
    x = x - alpha^j * gfx
    # increment counter
    iter = iter + 1
  }
  print(paste("End at function value:",f_name(x)))
  print(paste("Number of iterations:", iter))
  print(paste("2-Norm of Gradient:", sqrt(sum(gfx^2))))
  #print("Approximate Solution:")
  #print(x)
  return(x)
}

armijo_rule = function(sig=0.1, al=0.5, x_val=NA, grad_x_val=NA, direction=NA, f_arm=NULL){
  
  fx = f_arm(x_val)
  j = 0
  f_new = f_arm(x_val + al^j*direction)
  while (f_new > (fx + sig*al^j * t(grad_x_val) %*% direction)  & j<101) {
    j=j+1
    f_new = f_arm(x_val + al^j*direction)    
  }
  if (j>=100) print("Warning, stepsize very small!")
  return(j)
  
}