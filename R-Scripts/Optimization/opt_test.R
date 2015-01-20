# Testscript for Optimization Routines

# count function and gradient evaluations, initialize as global variables
#initial_values = rep(4,7)
n = 20
initial_values =  1:n / (2*n) 
if (0) {
  fname = sq_test
  grname = sq_grad_test
  hesname = sq_hesse_test
} else {
    fname = rosenbrock_advanced
    grname = grad_rosenbrock_advanced
}

count_f <<- 0
count_g <<- 0
res <- grad_descent(start_val=initial_values, f_name=fname, grad_f_name = grname)
print(paste("Function:", "Gradient:"))
print(paste(count_f,count_g))
count_f <<- 0
count_g <<- 0
count_h <<- 0
count_bis <<- 0
count_bis2 <<- 0
#res2 <- conj_grad(start_val=initial_values, f_name=fname, grad_f_name = grname)
print(paste("Function:", "Gradient:", "Interval:","Bisection:"))
print(paste(count_f,count_g,count_bis,count_bis2))
