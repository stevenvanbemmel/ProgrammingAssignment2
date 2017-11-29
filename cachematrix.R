## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Once initiated, the inverse is unknown. So we set the inverse to null
  inv <- NULL
  
  ## A special function set() allows us to overwrite/reset the stored matrix and
  ## its stored inverse with a new matrix and an empty inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get() will simply return the stored matrix
  get <- function() x
  
  ##setInverse will be called upon by cacheSolve to store the inverse matrix globally
  setInverse <- function(inverse) inv <<- inverse
  ##getInverse() will simply return the inverse. If it is not calculated (yet), it will return null
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  ## First get the inverse from the function 'makeCacheMatrix'
  inv <- x$getInverse()
  ## Now check if the inverse already exists (ie inv is not null)
  if(!is.null(inv)) {
    ## If the inverse already exists, return the inverse. Computation time saved!
    message("Getting cached data")
    return(inv)
  }
  ## Once you reach this point of the function, the inverse does not exist yet and
  ## we need to compute it. Get the matrix first.
  myMatrix <- x$get()
  ## Now get the inverse by using the function 'solve()'
  inv <- solve(myMatrix, ...)
  ## And store the inverse in the function 'makeCacheMatrix'
  x$setInverse(inv)
  ## Finally, return the inverse as requested
  inv
}
