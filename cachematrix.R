## Matrix inversion is usually a costly computation and there may be some benefit to
## catching the inverse of a matrix rather than computing it repeatedly. 
## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to:
## a. Set the value of the matrix.
## b. Get the value of the matrix.
## c. Set the value of inverse of the matrix.
## d. Get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setmean <- function(mat) inv <<- mat
        getmean <- function() inv
        list(set=set, get=get, setmean=setmean, getmean=getmean)
    
}

## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. 
## If so, it gets the result and skips the computation. 
## If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)){
                message("getting data, please wait")
                return(m)
        }
        else {
               data <- x$get()
               result <- solve(data,...)
               x$setmean (result)
               result
        }
        
}