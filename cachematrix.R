
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(
        set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## This function computes the inverse of the special "matrix"
## It first checks to see if the inverse has already been calculated

cacheSolve <- function(cache_x, ...) {
    inv <- cache_x$getInverse()
    if(!is.null(inv)) {
            message("Getting cached inverse matrix")
            return(inv)
    }
    
## If the inverse matrix has not been calculated, it does so here    
    
    make_inverse <- cache_x$get()
    inv <- solve(make_inverse, ...)
    cache_x$setInverse(inv)
    inv
}
