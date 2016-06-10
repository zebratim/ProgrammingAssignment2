
## This function creates a list containing a function to
##
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse of the matrix
##      get the value of the inverse of the matrix

makeCacheMatrix<- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calls the cached inverted matrix. If it has been calculated
## the cached value is returned. Otherwise the function calculates the inverse
## of the matrix

cacheSolve<- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
