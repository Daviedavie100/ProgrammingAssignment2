## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function( m = matrix() ) {
        inv <- NULL

        ## set matrix
        set <- function( matrix ) {
            m <<- matrix
            inv <<- NULL
    }
    ## set the inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ## get and return the inverse of the matrix
    getInverse <- function() {
        inv
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {

    ## returns the matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## get and compute the inverse of matrix
    data <- x$get()
    m <- solve(data) %*% data

     x$setInverse(m)   ## Set the inverse to the object
     m   ## Return the matrix
}
