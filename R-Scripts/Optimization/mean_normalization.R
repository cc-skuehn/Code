# Mean Normalization - useful for Recommender Systems

# Input
# M: Data matrix
# I: Indicator or Incidence matrix
# dim: Rows(1,default) or Columns(2)

# Mean Normalization is always performed for rows, in case you need it for cols (set dim=2) there is transposing and back-transposing
# There are no compatibility checks concerning the dimensions of M and I or anything else.
# Only the matrix entries M_ij where I_ij=1 are taken into consideration.
# If there are no such point in row i the row-mean is set to 0.
# The matrix entries NormM_ij for I_ij=0 are set to 0 as well. It might be that we change this later when it comes to cross validation, don't know yet.

mean_normalization = function(M,I,dim=1){
  
  # Transpose matrices if necessary
  if (dim==2){
    M=t(M)
    I=t(I)
  }
  # Start mean normalization
  dimM = dim(M)
  nrow = dimM[1]
  ncol = dimM[2]
  
  NormM = matrix(0,nrow,ncol)
  MeanM = matrix(0,nrow,1)
  for (i in 1:nrow){
    # Check for entries in i-th row
    temp_ind = (I[i,]!=0)
    if (sum(temp_ind)>0) {
      MeanM[i] = mean(M[i,temp_ind])
      NormM[i,temp_ind] = M[i,temp_ind] - MeanM[i]
    }   
  }
  # End mean normalization
  # Retranspose if necessary
  if (dim==2){
    NormM=t(NormM)
    MeanM=t(MeanM)
  }
  return(list(NormM,MeanM))
}