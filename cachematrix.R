## This R script contains two functions, makeCacheMatrix and cacheSolve, which
##can caculate the inverse of a matrix and cache it.


## This function creates a special "matrix" object that can cache its inverse.

 makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## set the value of the matrix 
    set <- function(y=matrix()) {
        x <<- y
        m <<- NULL
    }
    
    ##get the value of the matrix
    get <- function() x
    ##set the inverse of a matrix
    setinverse <- function(inverse) m <<- inverse
    ##get the inverse of a matrix
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
 }



## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

 cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
