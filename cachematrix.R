## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

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


##testing the functions

##> x = cbind(c(-1/8, 3), c(3, -1/8))
##> m=makeCacheMatrix(x)
##> m$get()
##      [,1]   [,2]
##[1,] -0.125  3.000
##[2,]  3.000 -0.125

##There's no cache in first run
##It computes the inverse, sets the value in the cache via
# setinverse function

##> cacheSolve(m)

##      [,1]       [,2]
##[1,] 0.01391304 0.33391304
##[2,] 0.33391304 0.01391304

##The functions retrieves the set cache in second run

##> cacheSolve(m)
##getting cached data.
##[,1]       [,2]
##[1,] 0.01391304 0.33391304
##[2,] 0.33391304 0.01391304
##> 
