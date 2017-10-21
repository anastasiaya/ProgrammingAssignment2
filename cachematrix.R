## The 2 functions below (1) creates a special "matrix" object 
## that can cache its inverse, and (2) computes the inverse of 
## the special "matrix" if it wasn't yet, and otherwise 
## retrieves it from cache. 

## (1) makeCacheMatrix: 
## This function creates a special "matrix" object that can cache 
## its inverse. Following the steps: 
## 1. set matrix
## 2. get matrix
## 3. set inverse
## 4. get inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # initially inv (inversed matrix) is set to null
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: 
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        ## if the inversed matrix of x is already calculated 
        ## (and stored in cache), get it out of cache
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        ## if it is not available, inverse x
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
