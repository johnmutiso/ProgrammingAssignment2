## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #Initializing the matrix inverse to NULL
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) inv <<- solve
        get_inverse <- function() inv
        list(set = set, get = get, 
             set_inverse = set_inverse, 
             get_inverse = get_inverse)
}
## This function gets an invertible matrix 'x' solves and caches its inverse



cacheSolve <- function(x, ...) {
        inv <- x$get_inverse() #recalling the inverse from the makeCacheMatrix function
        if(!is.null(inv)) {
                message("getting cached matrix inverse data")
                return(inv)
        }
        #getting the matrix inverse if not found, its calculated,
        #cached and returned
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
