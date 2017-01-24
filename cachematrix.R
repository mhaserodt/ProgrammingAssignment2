## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## Creates a matrix object that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(a) {
        x <<- a
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Gets inverse of result of makeCacheMatrix 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
        ## Return a matrix that is the inverse of 'x'
}
##> MatrixA <- matrix(c(1,2,3,4),2,2)
##> MatrixACache<-makeCacheMatrix(MatrixA)
##> cacheSolve(MatrixACache)
##
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
##> cacheSolve(MatrixACache)
##
##getting cached data.
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
