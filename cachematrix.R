
## makeCacheMatrix sets the value of the matrix,
## gets the value of the matrix,
##sets the value of the inverse, and
## and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m<- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## gets the inverse of the special matrix created      by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
