## It's a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse. 
## Also it can check if the matrix has not changed.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        set <- function(y) {
                ## At first we should chek if new matrix is different 
                ## and if it is 
                ## then we change our internal matrix with new one and reset inversed matrix

                if (!identical(all.equal(x, y), TRUE)) {
                    x <<- y
                    inv <<- NULL
                }
        }

        get <- function() x

        setinv <- function(inverse) inv <<- inverse

        getinv <- function() inv

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
