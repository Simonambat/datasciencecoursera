
## This function creates a matrix which
## can cache the inverse of the matrix
## This permits get and set matrix as well as
## set inverse and get inverse
## This function also uses the super assignment operator.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## The following function finds the inverse of the 
## "matrix" returned by `makeCacheMatrix`. If the inverse has
## already been calculated and the matrix is not changed, then
## this function will pick the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
