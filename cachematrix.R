## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {                 ## makeCacheMatrix will create a custom matrix type capable of running four functions
                                                            ## set stores the matrix in cache, get recalls the matrix from cache
                                                            ## setInverse and getInverse do the same but for the inverse of the original matrix
           inv <- NULL
        set <- function(y) {                                ## if user want to reset matrix
                x <<- y                                     ## reassign "new" matrix to x
                inv <<- NULL                                ## store matrix in cache
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse     ## set inverse matrix
        getInverse <- function() inv                        ## get inverse matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)                       ## create list of functions
}


## This function computes the inverse of the matrix created by makeCacheMatrix function
## If the inverse of the matrix has already been calculated (and the matrix has not changed), 
## then it will retrieve the inverse of the matrix from the cache.

cacheSolve <- function(x, ...) {                ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        if(!is.null(inv)) {                     ## To check if the user had calculated the same matrix before
                message("Getting cached data")
                return(inv)                     ## return old the result, inv, without recalculating
        }
        matrx <- x$get()                        ## otherwise, get the uncalculated matrix
        inv <- solve(matrx, ...)                ## calculate the inverse of the matrix
        x$setInverse(inv)                       ## reassign inverse matrix 
        inv                                     ## print the inverse matrix

}
