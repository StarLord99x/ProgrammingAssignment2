## These functions create a special object to calculate the 
## inverse of a matrix. The result is cached so that repeated
## computation of the inverse can be avoided.
##

##
## The makeCacheMatrix function creates a 'special matrix' that 
## calculates the inverse of a matrix and caches the result for
## future use. The 'special matix' is a list of functions to
## manage the cache.
##
## 'x' is a matrix. The function assumes the passed matrix is
## invertible.
##
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

##
## The cacheSolve function makes use of the 'special matrix'
## created by makeCacheMatrix to calculate the inverse of a
## matrix. If the inverse has already been calculated, the 
## result comes from the cache. Otherwise, the inverse is 
## calculated and the result is cached.
##
## 'x' is a 'special matrix' created by makeCacheMatrix. It
## is a list of functions that compute the inverse matrix and
## manage the cache.
##
## '...' are additional arguments to be passed to 'solve'.
## NOTE: No check is made to compare the additional arguments
## used to create the cached result with the additional arguments 
## in the current call.
##
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
