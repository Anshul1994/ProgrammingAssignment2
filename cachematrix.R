##Below are two functions that are used to create a special object that stores
##a matrix and cache's its inverse.

## makeCacheMatrix is a function that creates a special matrix which is really
## a list containing functions to get the value of the input matrix, set and
## cache the value of inverse matrix and get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(get=get,setinverse=setinverse,getinverse=getinverse)

}


## cacheSolve function first checks if the inverse of the special matrix created
## by above function exists, if so it gets that matrix from cache and returns it.
## If no, then it calculates the inverse of the matrix, and sets the value of the
## inverse in cache by setinverse function and returns that inverse matrix.
##Note : Argument of cacheSolve() must be an object created by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat)
        x$setinverse(i)
        i
}
                
