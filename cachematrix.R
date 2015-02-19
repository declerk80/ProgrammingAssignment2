## These functions computes the inverse of the matrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function retrieve the inverse from the cache

## Matrix example:  
## > z <- c(1,4,7,26,34,23)
## > m <- matrix(c(z,z,z,z,z), nrow = 5, ncol =5)

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}
## Execution example: > a <- makeCacheMatrix(m)


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } else { message("NO CACHED DATA")}
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m               ## Return a matrix that is the inverse of 'x'
}

## Execution example: > cacheSolve(a)
