## Matrix inversion functions to compute and cache a square invertable matrix.

## Create "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## Set the matrix and reset the inverse (cache)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Display the matrix
    get <- function() x
    
    ## Set the inverse (cache)
    setinverse <- function(solve) inv <<- solve
    
    ## Display the inverse (cache)
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Provide the inverse of a "matrix" object. Use cache if previously
## calculated or compute the inverse if not.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Get the inverse (cache) first
    inv <- x$getinverse()
    
    ## Is there an inverse in the cache?
    if(!is.null(inv)) {
        ## Return the cached inverse
        message("getting cached data")
        return(inv)
    }
    
    ## No cached inverse so calculate it and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
