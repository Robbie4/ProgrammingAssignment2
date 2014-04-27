# The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.
# The "cacheSolve" function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {                            # set the values of the matrix
                x <<- y
                s <<- NULL
        }
        get <- function() x                             # get the values of the matrix
        setinverse <- function(solve) s <<- solve       # set the values of the inverse of the matrix
        getinverse <- function() s                      # get the values of the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        s <- x$getinverse()                             # query the x matrix's cache
        if(!is.null(s)) {                               # if there is a cache
                message("getting cached data")
                return(s)                               # return the cache; there is no computation needed
        }
        data <- x$get()                                 # if there is no cache
        s <- solve(data, ...)                           # the inverse of the matrix is computed here
        x$setinverse(s)                                 # save the result to x's cache
        s                                               # return the result
}


