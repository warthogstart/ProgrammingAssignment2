## Functions to implement caching of an inverse matrix.
## Expected to be used together.

## Creates a list of functions to store/fetch a matrix or its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # stores a matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # fetches a matrix
    get <- function () x
    
    # stores the inverse matrix for a matrix that is already stored using set()
    setinv <- function(inverted) inv <<- inverted
    
    # fetches an inverse matrix
    getinv <- function() inv
    
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}

## Calculates an inverted matrix.
## x must be a list of functions created by x <- makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv ()
    
    # if no inverse is stored, calculate it first, else skip
    # the calculation and output the stored version
    if (is.null(inv))
    {
        matr <- x$get()
        inv <- solve(matr)
        x$setinv(inv)
    }
    
    inv
}
