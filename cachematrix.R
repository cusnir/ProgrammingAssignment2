# Coursera, R Programming(rprog-010), January 2015
# Cache the inverse of a matrix
# Matrix inversion is usually a costly computation and it makes sense 
# to cache the inverse of a matrix rather than compute it repeatedly 
# These two functions calculate and cache the inverse of a matrix.

## Example of usage
## m <- matrix(c(4, 3, 3, 2), nrow = 2)
## store the initial matrix in cached matrix object
## m_cached <- makeCacheMatrix(m)
## first call will solve the inverse and cache it
## cacheSolve(m_cashed)
## second call will return the cached value
## cacheSolve(m_cashed)

# This function creates an interface to access(get) and cache(set)
# the contents of a matrix and the inverse of original matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}
# This function computes the inverse of the special "matrix" 
#  returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inv_matrix of 'x'
    inverse <- x$get_inverse()
    # check if inverse matrix was already calculated and 
    # retrieve the content from the cache.
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # else calculate the inverse matrix
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}
