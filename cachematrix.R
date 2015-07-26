
library("Matrix")

## cache the value of an inverse of a matrix, the result is a list
## containing the following elements:
# set the value of the vector
#  get the value of the vector
#  set the value of the mean
#  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## calculates the inverse of the cached matrix that we just cached
## with the function makeCacheMatrix()
# it first checks to see if the inverse has already been calculated. 
# If yes, it takes the inverse from the cache and skips the computation.

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


# test of the inverse with an inversible matrix

x <- cbind(c(-3,-1,1), c(5,2,-1), c(6,2,-1))
temp <- makeCacheMatrix(x)
y <- cacheSolve(temp)

y %*% x
x %*% y
