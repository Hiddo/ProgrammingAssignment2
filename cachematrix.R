## makeCacheMatrix() can cache the inverse of a matrix which is the input
## it contains 4 functions: set (set the value of the matrix), 
## get (get the value of the matrix), 
## setinverse (set the value of the inverse of the matrix), 
## getinverse (get the value of the inverse of the matrix)
## the <<- operator is used to assign a value to an object in 
## environment that is different from the current environment

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) m <<- inverse 
    getinverse <- function() m 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function calculates the inverse of a matrix returned by 
## makeCacheMatrix() unless inverse has been calculated before
## in which cases the stored value is returned

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setinverse(m)
    m
}
