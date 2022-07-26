## The 2 functions used together are meant to cache the inverse of a matrix, and in the case of that same matrix being used again detect if a cache for that matrix's
## inverse exists, meaning the calculation has been done and stored properly (with the directions and in the parent environment). The new object created, the cache (a list object), will 
## contain the matrix, its inverse and the 4 functions that allow to create the cache for the inverse of the matrix and for R to find it simply in the parent 
## environment.
## If the cache exists and contains the data for it uses the content (the inverse of the matrix) of the cache instead of computing it again. If not it the inverse of
## the matrix is computed. 


##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#### makeCacheMatrix

## This function's goal is to create an object that stores 4 functions and 2 data object. All of them are stored in parent environment, they contain all the information
## of the environment in which the makeCacheMatrix was created. This the creation of the cache for the inverse of the matrix. The cache is emptied everytime
## a new matrix is being used in order not to have a matrix associated with a cache for another inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#### cacheSolve 

## This function checks if the inverse of the matrix has already been computed and stored in the parent environment thanks to the data contained in the special "matrix" returned by 
## makeCacheMatrix. If the cache contains the data it returns the inverse without having to calculate it again. ## If no, it computes the inverse of the matrix. In all scenarios 
## the inverse of the matrix is returned. The only difference will be wether or not it had to be calculated or it was there already.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
