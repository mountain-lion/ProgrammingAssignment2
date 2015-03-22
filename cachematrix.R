## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# validate m and n to be square matrices for optimization of performance
is_conformable <-function(m,n)
{
  if(nrow(m)!=ncol(m) || nrow(n)!=ncol(n) ||
       nrow(m)!=nrow(n) || ncol(m)!=ncol(n))
  {
    print("input matrices are not square or conformable")
    return (as.logical(0))
  }
  return (as.logical(1))
}

# check whether m is inverted matrix of n
# both m and n are assumed to be square matrices
is_invert <- function(m=matirx(),n=matrix())
{
  if(!is_conformable(m,n))
    return (as.logical(0))
  #check if A * inv(A) == I
  return (identical(m %*% n, diag(ncol(m))))
  
}
makeCacheMatrix <- function(x = matrix()) {
  
    I <- as.matrix(x)
   
    # validate true matrix construction 
    if(!is.matrix(x))
    {
      print("input data is not a matrix")
      return (x)
    }
    # validate x as a square matrix
    if(nrow(x)!=ncol(x))
    {
      print("input data is not a square matrix")
      return (x)
    }
    #validate if matrix is invertible 
    if(det(x) == 0)
    {
      print("input matrix is not an invertible matrix")
      return (x)
    }
    if(ncol(x) > 1 )
    {
        for(inv in cache_of_inverts)
          if(is_invert(inv,x))
          {
            print ("inverted matrix found in cache")
            return (inv)
          }
       
        t <- try(I <- solve(x))
        if(!("try-error" %in% class(t)))
          cache_of_inverts[[length(cache_of_inverts)+1]] <<- I
    }
    return (I)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      return (makeCacheMatrix(x))
      
}

# run_tests 

  cache_of_inverts <<-NULL
  
  c <-rbind(c(1, -1/4), c(-1/4, 1))
  cacheSolve(c)
  
  d <-rbind(c(2, -1/4), c(-1/4, 2))
  cacheSolve(d)
  
  call <- function(s)
  {
  for (i in s)
    cacheSolve(i)
  }
  s <- list(rbind(c(1, -1/4), c(-1/4, 1)),rbind(c(1, -1/4), c(-1/4, 1)))
  
  call(s)
  
  lapply(s,cacheSolve)
  
  M <- matrix(c(1:9), 3,3)
  N <- matrix(c(10:18), 3,3)
  O <- matrix(c(1:16), 4,4)
  # 
  cacheSolve(M)
  cacheSolve(N)
  cacheSolve(O)
  
  mlist <- list(c,d,M,N,O,rbind(c(1, -1/4), c(-1/4, 1)))
  
  lapply(mlist,cacheSolve)





