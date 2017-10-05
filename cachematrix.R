##  Introduction
##  Matrix inversion is usually a costly computation and there may be some benefit
##  to caching the inverse of a matrix rather than compute it repeatedly. The
##  following two functions are used to cache the inverse of a matrix.

##  Function 1: makeCacheMatrix creates a special "matrix" object that can cache 
##  its inverse.
##  makeCacheMatrix creates a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of inverse of the matrix
##  4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, 
    setinverse=setinverse, 
    getinverse=getinverse)
}


##  Function 2: cacheSolve computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix function.
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample

## > x = rbind(c(1, -1/3), c(-1/3, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##           [,1]       [,2]
##[1,]  1.0000000 -0.3333333
##[2,] -0.3333333  1.0000000

## No cache in the first run

## > cacheSolve(m)
##      [,1]  [,2]
##[1,] 1.125 0.375
##[2,] 0.375 1.125

## Retrieving from the cache in the second run

## > cacheSolve(m)
##getting cached data
##      [,1]  [,2]
##[1,] 1.125 0.375
##[2,] 0.375 1.125
## > 