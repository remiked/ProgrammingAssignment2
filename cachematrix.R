## The functions use caching to facilitate calculation of matrix inverses. If the 
## inverse has been previously calculated, unnecessary repetition of computations 
## can be avoided by caching the result and pointing to the cached value.

## The first function, "makeCacheMatrix", creates a list containing a function to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## inverse value is initially set to NULL by default
    set <- function(y) {
        x <<- y ## sets the value of the matrix 
        inv <<- NULL ## resets the cache in case the matrix is changed
    }
    get <- function() x ## returns the matrix
    setinverse <- function(inverse) inv <<- inverse ## setinverse(value) sets "inv", the cached value of the inverse, to "value" 
    getinverse <- function() inv ## retrieves the value of the inverse from cache
    list(set = set, get = get, ## returns a list of the functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function, "cacheSolve", calculates the inverse of the matrix passed
## to it after checking whether this inverse has been calculated and stored in cache.
## If it has, it returns the cached value, rather than recalculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) { ## if the inverse is cached, return the cached value instead of recalculating
        message("getting cached data") ## this message indicates that the value is indeed cached
        return(inv) ## the use of "return" ensures that the below block of code is not executed if the inverse value is already cached
    }
    data <- x$get() ## if the inverse has not been calculated, the matrix is retrieved from the list using the get() function
    inv <- solve(data, ...) ## and calculated using the solve() function
    x$setinverse(inv) ## and then stored in cache for future use
    inv ## finally, the inverse is returned
}
