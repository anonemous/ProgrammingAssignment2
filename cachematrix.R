## Define a special type of matrix data structure for inverting a matrix
## and storing the inverse for faster serving upon subsequent accesses

## Takes as parameter a matrix and returns a data structure
## allowing to set and get the values of both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Takes as argument an instance of the matrix data structure having
## functions to get and set the values of the matrix and its inverse; 
## returns its inverse of the matrix passed as an argument,
## upon subsequent accesses and also stores it for faster serving

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
