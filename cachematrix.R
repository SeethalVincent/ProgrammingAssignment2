# Two functions enable caching and retrival the cached data
# makeCacheMatrix function takes a matrix as its input and creates a cache for the inverse of that matrix
# cacheSolve function retives the stored value of matrix, calculates the inverse and stores in the
# cache provided by makeCacheMatrix.
# A list of functions are provided by makeCacheMatrix to store and retrive the matrix and its inverse

## makeCacheMatrix
## Parameters : matrix x
# Returns: List of 4 functions:
# - set: sets a matrix to a variable x
# - get: gets the value stored in the x
# - setinverse : sets the inverse of a matrix
# - getinverse : gets the inverse of matrix set by setinverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(
        set=set,
        get=get,
        setinverse=setinverse,
        getinverse=getinverse
    )
 }

# This function finds the inverse of a matrix
# Input Parameters :
#    x : is a list of functions
# returns a the inverse of the matrix
# The inverse calculation is done the first time, cache the inverse and
# retrieve from cache next time onwards
cacheSolve <- function(x) {
     i <- x$getinverse()
     if(!is.null(i)) {
         message("getting cached data")
         return(i)
     }
     data <- x$get()
     i <- solve(data)
     x$setinverse(i)
     i
}
