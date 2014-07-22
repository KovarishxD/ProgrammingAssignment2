## Pair of functions to calculate and cache the inverse of a matrix.


## Caches a matrix and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
      	x <<- y
      	m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverted) m <<- inverted
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of x, and caches it.
## If x doesn't change, and this function is called again,
## it returns the cached inverse of x.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
      	message("getting cached data")
      	return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
