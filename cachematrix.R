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

# Potential Entries
# a<- makeCacheMatrix(matrix(1:4,2,2))
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# # b<- makeCacheMatrix(matrix(rnorm(n=9),3,3))
# [,1]       [,2]        [,3]
# [1,] -0.2840163 -3.7511456 -1.02852273
# [2,] -0.3923121  0.9376273  0.01711668
# [3,] -0.2475683 -0.3430920  0.33722210
