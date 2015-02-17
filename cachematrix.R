## These two function should help us to optimize the inversion process of matrix. Firstly, the 
##'makeCacheMatrix' will help to creates a 'matrix' and cache its inverse (assuming that the matrix in inversible)
## Secondly, 'cacheSolve' function will help recall the inverse of matrix defined in the 'makeCacheMatrix' if any.
## In case the inverse was not already processed in the previous function, 'cacheSolve' will process the inverse.

## The 'makeCacheMatrix' may create a matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # sets x equal to an empty matrix
  M <- NULL
  # Set the inverse equal to NULL
  
  set <- function(y){
    x <<- y
    # set function assigns the argument to x
    M <<- NULL
    # Once the set function is called, Inverse is reset to NULL
  }
  get <- function() x
  # get function returns the matrix
  
  setInverse <- function(solve) M <<- solve
  # setInverse overrides the previous value of M and assigns the argument to Inverse assuming that the matrix
  #is inversible
  
  getInverse <- function() M
  # getInverse returns the Inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  # List of the functions
  
}



## The following will compute the inverse of the 'matrix' object created in the 'makeCachematrix' function

cacheSolve <- function(x, ...) {
  M <- x$getInverse()
  # Recall the most recent value for the inverse
  
  if(!is.null(M)){
    message("get cached data")
    return(M)
    # If the value of Inverse is NOT null cacheSolve function returns that value        
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setInverse(M)
  # Sets Inverse to the newly calculated value   
  M #Returns the new Inverse value
}

