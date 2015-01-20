# Nonlinear Conjugate Gradient - Basics

conj_grad = function(start_val=NA, f_name=NULL, grad_f_name=NULL, accuracy=1e-6, max_iterations=100000){
  
  ### Start of the Conjugate Gradient iteration
  x = start_val
  nof_dim = length(x)
  fx = f_name(x)
  gfx = grad_f_name(x)
  gfx_old = gfx
  tol = accuracy * sqrt(sum(gfx^2)) # tolerance, stopping criterion, relative to starting point, but:
  if (tol < accuracy) tol = accuracy # stopping criterion should not be too strict, e.g. if we start near the optimum, the gradient can already be almost zero

  print(paste("Start at function value:",fx))
  iter = 0 # iteration counter
  
  # First step as gradient step, line search, Secant method or Bisection or Newton-Raphson iteration (needs Hessian, but typically less iterations)
  # TODO method "grad" = gradient descent with armijo rule
  method_name = "bis" # choose between "sec" or "nr" or "bis" 
  if (method_name == "sec") {
    alpha = sec_method(x,-gfx,grad_f_name)  
  }
  if (method_name == "bis") {
    alpha = bis_method(x,-gfx,grad_f_name)  
  }
  if (method_name == "nr") {
    # set name of the function that computes the Hessian
    hesse_f_name = sq_hesse_test # 
    alpha = nr_method(x,-gfx,grad_f_name,hesse_f_name)  
  }
  x = x - alpha * gfx
  gfx = grad_f_name(x)
  iter = iter + 1
  s_old = -gfx

  # Start loop, check stopping/running conditions
  while (sqrt(sum(gfx^2)) > tol & iter < max_iterations) {

    #print(paste("iter", iter))
    # CG Iteration
    # Take care of the sign, in the literature the search direction is always -gfx, we use gfx and include the sign otherwise
    beta_FR = sum(gfx*gfx)/sum(gfx_old*gfx_old) # Fletcher-Reeves
    beta_PR = max(0,sum(gfx*(gfx-gfx_old))/sum(gfx_old*gfx_old)) # Polak-Ribière, reset to 0 (=gradient step), if negative
    beta_HS = -sum(gfx*(gfx-gfx_old))/sum(s_old*(gfx-gfx_old)) # Hestenes-Stiefel
    beta_DY = -sum(gfx*gfx)/sum(s_old*(gfx-gfx_old)) # Dai-Yuan
    # Change, if you like, I would recommend beta_PR as default
    beta = beta_PR
    # TODO: Implement restart, either gradient check or every N(=number of variables) steps, or something else
    #if (iter%%nof_dim==0) beta = 0 # 
    if (iter%%min(1,nof_dim)==0) beta = 0
    s = -gfx + beta*s_old
    
    # Determine alpha - Secant method or Newton-Raphson or Bisection method, see above
    if (method_name == "sec") {
      alpha = sec_method(x,s,grad_f_name)  
    }
    if (method_name == "bis") {
      #print("Bisection")
      alpha = bis_method(x,s,grad_f_name)  
    }
    if (method_name == "nr") {
      alpha = nr_method(x,s,grad_f_name,hesse_f_name)  
    }
    x = x + alpha*s
    s_old = s
    gfx_old = gfx
    gfx = grad_f_name(x)
    #print(paste(iter,sum(gfx^2)))
    iter = iter + 1  
  } # end while

  print(paste("End at function value:",f_name(x)))
  print(paste("Number of iterations:",iter))
  print(paste("2-Norm of Gradient:",sqrt(sum(gfx^2))))
  return(x)
  
}

# Newton-Raphson method for finding roots of the directional derivative of a multivariate function
# NEEDS HESSIAN so not applicable to all test functions
nr_method <- function(start_val=NA,direction=NA,grad_f_name=NULL,hesse_f_name=NULL,accuracy=1e-6,max_iterations=10000){
  
  iter = 0
  x_old = 0
  x_new = 1
  fx_new = t(direction) %*% grad_f_name(start_val+x_new*direction)
  gfx_new = sum(t(direction) %*% hesse_f_name(start_val+x_new*direction) %*% direction)
  
  tol = accuracy * sqrt(sum(fx_new^2)) # tolerance, stopping criterion, relative to starting point, but:
  if (tol < accuracy) tol = accuracy # stopping criterion should not be too strict, e.g. if we start near the optimum, the gradient can already be almost zero
  
  while (iter<max_iterations) {
    if (abs(fx_new) < tol | abs(x_new-x_old) < accuracy) {
      return(x_new)
    }  else {
      iter = iter + 1
      x_old = x_new 
      fx_old = fx_new
      gfx_old = gfx_new
      x_new = x_old - (fx_old/gfx_old)
      fx_new = t(direction) %*% grad_f_name(start_val+x_new*direction)
      gfx_new = sum(t(direction)%*%hesse_f_name(start_val+x_new*direction)%*%direction)
    }
  }
  return(x_new)
} # end Newton-Raphson

# Secant method for solving the one-dimensional line search problem (inner iteration)
# Finding the root of the directional derivative of a multivariate function
# Can be used for univariate functions as well, just set direction to 1
# WARNING: Method may converge if initial values are not near the root -> in case of divergence return FALSE
sec_method <- function(start_val=NA,direction=NA,grad_f_name=NULL,accuracy=1e-6,max_iterations=10000){
 
  iter = 0
  # We need two initial values, because we replace the tangent by secant
  x_1 = 0
  x_2 = 1
  fx_1 = t(direction)%*%grad_f_name(start_val+x_1*direction)
  fx_2 = t(direction)%*%grad_f_name(start_val+x_2*direction)
  if (fx_1==fx_2) stop("Error in Secant method")
  sec_12 = (fx_1-fx_2)/(x_1-x_2)
  x_new = x_2 - fx_2/sec_12
  fx_new = t(direction)%*%grad_f_name(start_val+x_new*direction)
  #print(paste(fx_1,fx_2,fx_new))
  if (sum(fx_new^2) > max(sum(fx_1^2),sum(fx_2^2))) stop("Secant method is diverging!")
  
  tol = accuracy * sqrt(sum(fx_new^2)) # tolerance, stopping criterion, relative to starting point, but:
  if (tol < accuracy) tol = accuracy # stopping criterion should not be too strict, e.g. if we start near the optimum, the gradient can already be almost zero
  
  while (iter<max_iterations) {
    if (abs(fx_new) < tol | abs(x_2-x_new) < accuracy)  return(x_new)
    else {
      iter = iter + 1
      x_1 = x_2
      x_2 = x_new
      fx_1 = fx_2
      fx_2 = fx_new
      fx_test1 = t(direction)%*%grad_f_name(start_val+x_1*direction)
      fx_test2 = t(direction)%*%grad_f_name(start_val+x_2*direction)
      sec_12 = (fx_1-fx_2)/(x_1-x_2)
      x_new = x_2 - fx_2/sec_12
      fx_new = t(direction)%*%grad_f_name(start_val+x_new*direction)
      if (sum(fx_new^2) > max(sum(fx_test1^2),sum(fx_test2^2))) stop("Secant method is diverging!")
    }
  }
  return(x_new)
  
}

# Bisection method for solving the one-dimensional line search problem (inner iteration)
# Finding the root of the directional derivative of a multivariate function
# Can be used for univariate functions as well, just set direction to 1
# Convergent, if the initial function values have opposite sign -> there exists root in the interval
# Convergence rate is linear as the interval is halved in each step
bis_method <- function(start_val=NA,direction=NA,grad_f_name=NULL,accuracy=1e-9,max_iterations=10000){
  
  # Find initial values with opposite-signed function values!
  iter = 0
  x_1 = -1
  x_2 = 1
  x_new = 0
    
  fx_1 = t(direction) %*% grad_f_name(start_val+x_1*direction)
  fx_2 = t(direction) %*% grad_f_name(start_val+x_2*direction)
  
  while (sign(fx_1)==sign(fx_2)) {
    count_bis <<- count_bis + 1
    if (count_bis > 50) {
      print(paste(x_1,x_2))
      stop("Could not find appropriate interval")
    }
   if (abs(fx_1)<abs(fx_2)) {
     x_1 = 2 * x_1
     fx_1 = t(direction) %*% grad_f_name(start_val+x_1*direction)
   } 
   else {
     x_2 = 2 * x_2
     fx_2 = t(direction) %*% grad_f_name(start_val+x_2*direction)   
   } 
  }
  
  x_new = (x_1+x_2)/2
  fx_new = t(direction)%*%grad_f_name(start_val+x_new*direction)
  tol = accuracy * sqrt(sum(fx_new^2)) # tolerance, stopping criterion, relative to starting point, but:
  if (tol < accuracy) tol = accuracy # stopping criterion should not be too strict, e.g. if we start near the optimum, the gradient can already be almost zero

  # Start bisection
  while (iter < max_iterations+count_bis) {
    if (abs(fx_new) < tol | abs(x_1-x_2) < tol  ) return(x_new)
    else {
      iter = iter + 1
      count_bis2 <<- count_bis2 + 1
      x_new = (x_1+x_2)/2
      fx_new = t(direction)%*%grad_f_name(start_val+x_new*direction)
      if (sign(fx_new)==sign(fx_1)) {
       x_1 = x_new
       fx_1 = fx_new
     } else {
       x_2 = x_new
       fx_2 = fx_new
     }
    }
  }
  
  return(x_new)
  
}
