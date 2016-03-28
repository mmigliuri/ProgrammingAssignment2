## These functions cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        matrix_cache <- NULL
        set <- function(y) {
                x <<- y
                matrix_cache <<- NULL
        }
        get <- function() x
        setinversecache <- function(solve) matrix_cache <<- solve
        getinversecache <- function() matrix_cache
        list(set = set, get = get, 
             setinversecache = setinversecache,
             getinversecache = getinversecache)

}


## This function computes the inverse of the special "matrix" returned by 
##      makeCacheMatrix. If the inverse has already been calculated (and the 
##      matrix has not changed), then the cachesolve should retrieve the inverse
##      from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_cache = x$getinversecache()
        if(!is.null(matrix_cache)) {
                message("getting cached data")
                return(matrix_cache)
        }
        data <- x$get()
        matrix_cache <- solve(data, ...)
        x$setinversecache(matrix_cache)
        matrix_cache
}
