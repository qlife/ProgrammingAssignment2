## Computing the inverse matrix is a kind of time consuming task. Therefore, it
## is worth to cache the result for the later usages. We introduce
## makeCacheMatrix(x) to create a special matrix object with ability to cache
## its inverse matrix. We also introduce the cacheSolve(x) function. Given
## an instance of the 'special matrix', it replies the cached inverse if the
## matrix has not been modified since the last time its inverse was computed.
## Otherwise, it recomputes the inverse matrix.

## This function creates a specifal matrix that is able to cache its inverse.
## Assume the supplied matrices are always invertible.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL

        ## Setter. Update both the matrix x and dropped the cache.
        set <- function(a_matrix_y) {
                x <<- a_matrix_y
                inv_x <<- NULL
        }

        ## Getter of the matrix x
        get <- function() {
                x
        }

        ## Setter of the inverse
        set_inv <- function(a_inv) {
                inv_x <<- a_inv
        }

        ## Getter of the inverse
        get_inv <- function() {
                inv_x
        }

        list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## It checks if there are cached inverse already; if so, return the cached
## inverse; otherwise, it use solve() to calculate the inverse and update the
## cache.

cacheSolve <- function(x, ...) {
        ## Check if we've have the cache already
        cache <- x$get_inv()
        if(!is.null(cache)) {
                return(cache)
        }

        ## If not, compute the inverse now.
        m <- x$get()
        inv <- solve(m)
        x$set_inv(inv)

        ## Return a matrix that is the inverse of 'x'
        inv
}
