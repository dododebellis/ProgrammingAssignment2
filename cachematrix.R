## Programming Assignment 2
## Complete 01-25-15
## The makeCacheMatrix and cacheSolve functions work in tandem
## to invert a matrix (assuming the input matrix is invertible!).
## If the matrix has already been inverted and stored in the 
## list object cache, this is what's returned. If not, then the
## inverted matrix is calculated, stored in the cache, and then
## returned.

## Establishes the list object cache for the inverse of the
## passed in matrix
## Params:
##   x: matrix to be inverted (must be square! nrow == ncol)
## Returns:
##   list object cache with accessor and mutator methods

makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    get <- function() x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(get = get, set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Takes a list object cache for a matrix and either returns the cached
## matrix inverse, or returns the inverse after calculating and caching
## it. 
## Params:
##   x: list object cache for matrix
##   ...: other arguments
## Returns:
##   inverted matrix
## Messages and Errors:
##   Notification if inverted matrix is coming from cache
##   Will throw error and terminate if matrix is not square

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get_inverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix)
    x$set_inverse(i)
    i
}
