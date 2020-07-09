## As the assigment's example, makeCacheMatrix is a function that creates a list that contains a function to set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
        x <<- y
        inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <- inverse
getInverse <- function() inv
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## cacheSolve computes the inverse of makeCacheMatrix's matrix or  retrieve the inverse from the cache in case is already calculated

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

