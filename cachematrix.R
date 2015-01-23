## This set of two functions will calculate and eventually cache the inverse of a given matrix.
## Usage: call cacheSolve(makeCacheMatrix(data)) where data is the given matrix.

## This function will make a list of properties on a given matrix.
## The list will contain four elements (set, get, setInverse, getInverse).

makeCacheMatrix <- function(x = matrix()) {
    ## initialize cache
    m <- NULL
    
    ## set the matrix and clear the cache
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
    
    ## get the matrix
	get <- function() x
    
    ## cache the inverted matrix
	setInverse <- function(inverse) m <<- inverse
    
    ## retrieve cached inverted matrix
	getInverse <- function() m
    
    ## return the four properties in a list
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## This function execute the matrix inversion from scratch
## or will retrieve the cached result from a previous call.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
    
    ## return the inverse, if any was cached
	if(!is.null(m)) {
			message("getting cached data")
			return(m)
	}
    
    ## calculate inverse from scratch
	data <- x$get()
	m <- solve(data, ...)
    
    ## cache the new inverted matrix
	x$setInverse(m)
    
    ## return result
	m
}
