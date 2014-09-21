## These functions calculate and cache the inverse of a matrix

## makeCacheMatrix creates a special matrix object that can cache its inverse.  
## It returns a list containing functions to set the value of the matrix,
## get the value of the matrix, set the value of the inverse, get the value of the invers

makeCacheMatrix <- function(x = matrix()) {
    # cache the new matrix and set its cached inverse to null
    inv <- NULL
    set <- function(y){
          x <<- y
          inv <<- NULL
    }
    
    # get the cached matrix
    get <- function() x
    
    # cache the inverse to the value passed in
    setinverse <- function(inverse) inv <<- inverse
    
    # get the cached inverse
    getinverse <- function() inv
    
    # return the list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve computes the inverse of the special matrix returned by makeCache Matrix.
## If the inverse has already been calculated and the matrix hasn't changed, 
## the cacheSolve retruns the inverse of the matrix from the cache.

cacheSolve <- function(x, ...) {
    # first check to see if the inverse is cached and if it is, return it
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    # if not, calculate the inverse
    data <- x$get()
    inv <- solve(data)
    # cache the inverse and return it
    x$setinverse(inv)
    inv
}
