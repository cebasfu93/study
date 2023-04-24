makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <-- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <-- inverse
  getInverse <- function() inv
}

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if (!is.null(inv))
    message("Getting cached data")
    return(inv)
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}