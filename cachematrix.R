##Below are two functions that are used to create a special object that stores
##a matrix and cache's its inverse.

## makeCacheMatrix is a function that creates a special matrix which is really
## a list containing functions to get the value of the input matrix, set and
## cache the value of inverse matrix and get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) { ##function definition
        i <- NULL ## object to store inverse of the given matrix x
        get <- function() x ##function to return the value of x
        setinverse <- function(inverse) i <<- inverse ##function to cache the value
        ##of inverse matrix to i
        getinverse <- function() i ##function to return to the value of i 
        list(get=get,setinverse=setinverse,getinverse=getinverse) ##returning 
        ##the special matrix which is actually a list

}


## cacheSolve function first checks if the inverse of the special matrix created
## by above function exists, if so it gets that matrix from cache and returns it.
## If no, then it calculates the inverse of the matrix, and sets the value of the
## inverse in cache by setinverse function and returns that inverse matrix.
##Note : Argument of cacheSolve() must be an object created by makeCacheMatrix()

cacheSolve <- function(x, ...) {
        i <- x$getinverse() ##to get the value of inverse of the matrix x if 
        ##already exists.
        if(!is.null(i)) { ##to check if inverse already exists and return if yes
                ##with a message "getting cached data"
                message("getting cached data")
                return(i)
        }
        mat <- x$get() ##if inverse doesn't exists, this will get the value of 
        ##matrix x and store it to object mat
        i <- solve(mat)  ## to calculate the inverse of mat and assign it to i
        x$setinverse(i) ## to cache the value of inverse 
        i ##return the value of inverse
}
                
