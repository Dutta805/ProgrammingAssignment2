## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## this function:- 1.sets the value of the matrix, 2.gets the value of the matrix, 3.sets the value of the mean, 4.gets the value of the mean

##Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


##This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
