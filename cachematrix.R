(## The function makeCacheMatrix creates a matrix that can cache its inverse. The function cacheSolve checks to see 
## if the inverse of the first matrix has been computed. If so, it returns that cached matrix. 
## If not cacheSolve computes the inverse and returns that.
## The idea is to avoid unnecessary computation.

## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  ## Sets the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x  ## Gets the value of the matrix
        setMatrixInverse <- function(solve) m <<- solve ## Sets the value of the inverse of the matrix
        getMatrixInverse <- function() m ## Gets the value of the inverse of the matrix
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}



## This function checks to see if the inverse of 
## the first matrix has been computed. If so, it returns that cached matrix. If not cacheSolve computes the 
## inverse and returns that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getMatrixInverse()       ## This checks to see if m is not equal to NULL, meaning it has  
                                                ## already been computed. If so, then this part returns that cached value for m
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get() ##Otherwise it computes the inverse and returns the newly computed value
                m <- solve(data, ...)
                x$setMatrixInverse(m)
                m
        }


