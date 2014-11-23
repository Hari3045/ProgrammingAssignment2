## Given a matrix, the function computes its inverse.
## The computation happens for the 1st time and the answer is stored in cache.
## Next time, the cache value is retrieved and returned. The whole computation is skipped.

## This function has the steps to compute inverse, cache it and return.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    #return list of function
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function checks for cache using above function, returns it if found else computes and returns.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    message("No cached data. Calculating the Answer..")
    data <- x$get() #get the matrix
    inv <- solve(data) #compute matrix
    x$setinverse(inv) #set the inverse matrix
    inv
}

## Test:
## > B = matrix(c(2, 4, 3, 1),nrow=2, ncol=2)
## > z <- makeCacheMatrix(B)
## > cacheSolve(z)
## 1st time:
## No cached data. Calculating the Answer..
##      [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2
## > cacheSolve(z)
## 2nd time:
## getting cached data.
##      [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2
