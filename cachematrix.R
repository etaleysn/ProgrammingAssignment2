##Two functions that are used to create a special object that stores a matrix and cache's its inverse.

##This function creates a special "matrix" object that can cache its inv.
makeCacheMatrix <- function(x = matrix()) { 
  matrixinv <- NULL                     
  set <- function(y) {                      
    x <<- y
    matrixinv <<- NULL              
  }
  get <- function() x                           
  setinv <- function(solve) matrixinv <<- solve 
  getinv <- function() matrixinv        
  list(set = set, get = get,                    
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inv has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve<- function(x, ...) {                 
  matrixinv <- x$getinv()
  if(!is.null(matrixinv)) {                 
    message("getting cached data")
    return(matrixinv)
  }
  data <- x$get()                               
  matrixinv <- solve(data, ...)
  x$setinv(matrixinv)
  matrixinv
}