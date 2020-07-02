## Put comments here that give an overall description of what your
## functions do
## firstly, this function could generate a "matrix" object so that we could cache its inverse.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}

##test the codes
## for the 3*3 matrix consisted with 9 random numbers
M1 <- matrix(rnorm(9), 3, 3)
M2 <- makeCacheMatrix(M1)
cacheSolve(M2)
# 
# [,1]        [,2]       [,3]
# [1,]  0.01011754  0.38709086 -0.6567908
# [2,] -1.17102817 -0.02183389 -0.2908090
# [3,]  0.03340264  0.28941517  0.9879951

## for the 3*3 matrix consisted with consecutive numbers 1-9
M1 <- matrix(1:9, 3, 3)
M2 <- makeCacheMatrix(M1)
cacheSolve(M2)
##because the numbers are from 1 to 9, there is an error occured, so the matrix does not have an inverse.