# Matrix inversion is usually a costly computation and it may be beneficial
# to cache the inverse of a matrix rather than compute it repeatedly. The
# pair of functions in this file provides functionality for caching the 
# inverse of a given matrix. 

# Examples of usage:
#> m <- matrix(c(-1, -2, 1, 1), 2,2)
#> x <- makeCacheMatrix(m)
#> x$get()
#[,1] [,2]
#[1,]   -1    1
#[2,]   -2    1

#> inv <- cacheSolve(x)
#> inv
#[,1] [,2]
#[1,]    1   -1
#[2,]    2   -1

#> inv <- cacheSolve(x)
#Getting cached inverse matrix
#> inv
#[,1] [,2]
#[1,]    1   -1
#[2,]    2   -1

#> m <- matrix(c(2, 1, 6, 3), 2, 2)
#> x$set(m)
#> cacheSolve(x)
# Error in cacheSolve(x) : Matrix is not inversible

#> m <- matrix(c(1,0,5,2,1,6,3,4,0), 3, 3)
#> x$set(m)
#> x$get()
#[,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    0    1    4
#[3,]    5    6    0
#> inv <- cacheSolve(x)
#> inv
#[,1] [,2] [,3]
#[1,]  -24   18    5
#[2,]   20  -15   -4
#[3,]   -5    4    1
#> inv <- cacheSolve(x)
#Getting cached inverse matrix

# -----------------------------------------------------------------------------

# This function creates a special object that represents an R matrix object and 
# can cache its inverse. This basically creates a list of functions used to:
# a. set the value of the matrix
# b. get the value of the matrix
# c. set the value of the inverse of the matrix
# d. get the value of the inverse of the matrix
makeCacheMatrix <- function(mat = matrix()) {
  # Check whether correct input was provided
  if (!is.matrix(mat)) {
    stop("Please give a matrix")
  }
  
  inv <- NULL
  # Functions for getting/setting the matrix object whose inverse
  # is to be cached
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  
  # Functions for caching the inverse
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # Return list of getters/setters functions for matrix and its inverse
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then 
# the cachesolve function retrieves the inverse from the cache and returns it. Otherwise,
# it computes the inverse using the solve() function in R, sets the inverse value in the matrix
# object through the setinverse function, and returns the calculated inverse. 
#
# This function throws an error if the matrix is non invertible. 
cacheSolve <- function(mat, ...) {
  # Have we already calculated and cached the inverse of this matrix?
  # If so, we return the cached copy
  inv <- mat$getinverse()
  if(!is.null(inv)) {
    message("Getting cached inverse matrix")
    return(inv)
  }
  # Otherwise, inverse the matrix, update the cached value, and return
  # the calculated inverse
  m <- mat$get()
  inv <- try(solve(m), silent=TRUE)
  # Check whether the matrix is actually inversible, and if not, throw an error
  if (class(inv) != "matrix")
    stop("Matrix is not inversible")
  mat$setinverse(inv)
  inv
}