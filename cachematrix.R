## This second programming assignment will require you to write an R function
## that is able to cache potentially time-consuming computations. For example, 
## taking the mean of a numeric vector is typically a fast operation. However,
## for a very long vector, it may take too long to compute the mean, especially
## if it has to be computed repeatedly (e.g. in a loop). If the contents of a 
## vector are not changing, it may make sense to cache the value of the mean so
##that when we need it again, it can be looked up in the cache rather than recomputed.


## functions do

## he first function, makeCacheMatrix creates a special "Matrix", which is really
## a list containing a function t:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


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


## cacheSolve computes the inverse of the special “matrix” returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
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
