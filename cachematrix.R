## Our aim is to write two functions, "makeCacheMatrix" and "cacheSolve", 
## which cache the inverse of a matrix. We do this because Matrix inversion
## is costly, and it might be better caching the inverse instead of computing
## it repeatedly.

## makeCacheMatrix is a function that creates a special matrix object that can
## cache its inverse for the input (that is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		 x <<- y
		 inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list( set = set,
		get = get,
		setInverse = set Inverse,
		getInverse = getInverse)
}

## cacheSolve is a function that computes the inverse of the special matrix
## that is returned by makeCacheMatrix above. If the inverse has already been
## calculated, then cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x$getInverse()
	  if (!is.null(inv)) {
		    message("getting cached data")
		    return(inv)
	  }
	  mat <- x$get()
	  inv <- solve(mat,...)
	  x$setInverse(inv)
}

