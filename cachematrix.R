## This code include a pair of functions that cache the inverse of a matrix (which is a costly computation)
## so that we can get it from the cache instead of repeatedly computing it

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse available from the cache.
cacheSolve <- function(x, ...) {
  # Retrieve the inverse if it is already in cache
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Otherwise, solve for the inverse and cache it for later retrieval 
  mat <- x$get()
  inv <- solve(mat)
  x$setInverse(inv)
  inv
}
