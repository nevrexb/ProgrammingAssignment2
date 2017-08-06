## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will define the get and set operators
## for matrix 'x' and its inverse 'i'

makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
       set <- function(y) {
               x <<- y
               i <<- NULL
       }
       get <- function() x
       setinverse <- function(inverse) i <<- inverse
       getinverse <- function() i
       list(set = set, 
            get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

## Write a short comment describing this function
## This function will check for the existance of the
## inverse matrix 'i' as a cached variable and return
## it.  If 'i' does not exist (is NULL) it will calculate
## the inverse of 'x' using the solve function and then 
## assign its result to 'i'


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
