# test functions and gradients, hessians (not all)

# TODO - Define third test function and derivative
# proposal: f(x) = sum_i x_i^i * sin(x_i), i=1,..,n
if (0) {
  len = 200
  nofp = 2*len + 1
  x=4*pi*(-len:len)/nofp
  y=x
  z=matrix(0,nrow=nofp,ncol=nofp)
  for (i in 1:nofp) for (j in 1:nofp) z[i,j]=(x[i])*sin(x[i])+y[j]^2*sin(y[j])
  contour(x,y,z,nlevels=80)
}

# Squared-squared euclidean norm of x, simple isotropic function
sq_test = function(x){
  count_f <<- count_f + 1
  return(sum(x^4))
}

sq_grad_test = function(x){
  count_g <<- count_g + 1
  return(4*x^3)
}

sq_hesse_test = function(x){
  count_h <<- count_h + 1
  lx = length(x)
  H = matrix(0,nrow=lx,ncol=lx)
  for (i in 1:lx) H[i,i] = 12*x[i]^2
  return(H)
}
# end squared-squared euclidean norm of x

rosenbrock_advanced = function(x,a=1,b=100){
  count_f <<- count_f + 1
  lx = length(x)
  f_val = 0
  for (i in 1:(lx-1)) {
     f_val = f_val + (a-x[i])^2+b*(x[i+1]-x[i]^2)^2
  }
  return(f_val)
}

grad_rosenbrock_advanced =  function(x,a=1,b=100){
  count_g <<- count_g + 1
  lx = length(x)
  if (lx<2) stop('Not multivariate!')
  grad_val = rep(0,lx)
  grad_val[1] = -2*(a-x[1]) - 4*b*x[1]*(x[2]-x[1]^2)
  if (lx>2) {    
    for (i in 2:(lx-1)) {
      grad_val[i] = grad_val[i] - 2*(a-x[i]) - 4*b*x[i]*(x[i+1]-x[i]^2) + 2*b*(x[i]-x[i-1]^2)
    }
  }
  grad_val[lx] = 2*b*(x[lx]-x[lx-1]^2)
  return(grad_val)
}