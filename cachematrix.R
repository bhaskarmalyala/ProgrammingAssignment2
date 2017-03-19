## Caching the inverse of a Matrix to reduce the computation time if inverse of the
## Matrix is already calculated. Two functions have been mentioned below to store 
## Matrix and cache the inverse of the matrix.
## 

## This function creates a matrix and cache the inverse of it

makeCacheMatrix <- function(x = matrix()) {
        
        invers <- NULL ## initialized to NULL such that if any change is there
        ## in matrix, inverse will be calculated again in the next function.
        set <- function(y) {
                x <<- y
                invers <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invers <<- inverse
        getInverse <- function() invers
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function takes the matrix from above function and computes inverse matrix
## if it is not computed already. And it caches inverse matrix if inverse is
## already calculated and the matrix has not been changed.

cacheSolve <- function(x, ...) {
        
        ## Returns the inverse of matrix if it is already done
        invers <- x$getInverse()
        if (!is.null(invers)) {
                message("displaying cached data")
                return(invers)
        }
        ## Computes the inverse of a matrix if it is not done already
        matrix <- x$get()
        invers <- solve(matrix, ...)
        x$setInverse(invers)
        invers
}