# makeCacheMatrix creates a special "matrix" object that stores a matrix and caches its inverse.
# cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache. 

# makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
# which is really a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL  
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# cacheSolve computes the inverse of the matrix created by makeCacheMatrix. 
# If the inverse has already been calculated, it retrieves the inverse from the cache.
# Otherwise, it computes the inverse and caches it.

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()  
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        mat <- x$get()  
        inv <- solve(mat, ...)  
        x$setInverse(inv) 
        inv 
}
