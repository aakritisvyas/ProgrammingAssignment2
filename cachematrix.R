## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix created a list of functions including setting inverse and getting inverse
## Passing a vector of values into makeCacheMatrix will cache the function with those values

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set=set, get=get,
             setinv = setinv,
             getinv = getinv)
}


## This function either calculated the inverse of a matrix or retrieves it from cache
## if the values have already been parsed into the previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
