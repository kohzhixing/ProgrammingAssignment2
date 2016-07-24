###CacheMatrix
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
# computing it repeatedly.
# This is a pair of functions that cache the inverse of a matrix.

# This function cache the inverse of matrix that is entered as an argument.
# The function then creates a special list containing four functions to

## get the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inverse) m <<- inverse
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# This function accepts a list as an argument. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                ## retrieves the inverse of the matrix if it has been calculated
        }
        data <- x$get()
        m <- solve(data, ...)
        ## Return a matrix that is the inverse of 'x'
        x$setmatrix(m)
        ## cache the inverse in the "setmatrix" element of the list
        m
        
}





