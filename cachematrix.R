## Author : Travis Ho
## A code for the Couursera course "R programming" assignment #2 
##
## This is a function to reduce the repeated calculation by utilizd cache 

## Define a funtion to set iverse or return already computed inverse of given matrix 

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        get <- function () {
                x
        }
        setInverse <- function (input) {
                inver <<- input
        }
        getInverse <- function() {
                inver                
        }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)               
}


## cachesolve will return the inverse of matrix x 

cacheSolve <- function(x, ...) {
        inver <- x$getInverse()
        if(!is.null(inver)) {
                message("Retreving the inverse of matrix from cache")
                return(inver)
        }
        ## assgin a given matrix to m 
        m <- x$get()
        inver <- solve(m, ...)
        x$setInverse(inver)
        inver
}
