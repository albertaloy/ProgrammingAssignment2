## 2 Functions to create a special matrix and getting it's inverse using cache if available
##
##
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
## Initialize the inverse
    i <- NULL
	
    ## Sets the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Gets the matrix
    get <- function() {
     m
    }

    ## Sets the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Gets the inverse of the matrix
    getInverse <- function() {
        i
    }

    ## Gets a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes or gets from the cache the inverse of the matrix given by makeCacheMatrix
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
