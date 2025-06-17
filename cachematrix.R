##these functions store a matrix's inverse. It can take a while to calculate a matrix's inverse.
##The inverse is calculated just once and then stored (cached) to allow for reuse without recalculating in order to enhance performance.

## makeCacheMatrix: Creates a special matrix object that can store its inverse for caching.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

## cacheSolve: Computes or retrieves the cached inverse of the matrix created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv) ## Return a matrix that is the inverse of 'x'
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
