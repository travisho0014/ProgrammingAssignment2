## Author : Travis Ho
## A code for the Couursera course "R programming" assignment #2 
##
## This is a function to reduce the repeated calculation by utilizd cache 

## Define a funtion to set iverse or return already computed inverse of given matrix 

## makeCachMatrix is a function for inversing a matrix
makeCacheMatrix <- function(x = matrix()) {
        ## the inverse is empty initially 
        inver <- NULL
        ## set new matrix, but inverse is set to empty
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        ## get existed matrix
        get <- function () {
                x
        }
        ## set a computed matrix 
        setInverse <- function (input) {
                inver <<- input
        }
        ## get a computed matrix
        getInverse <- function() {
                inver                
        }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)               
}

## cachesolve will return the inverse of matrix x 

## a function for solving inverse of matrix 
cacheSolve <- function(x, ...) {
        ## get a inverse from a matrix 
        inver <- x$getInverse()
        ## check if the inversed of matrix is solved 
        if(!is.null(inver)) {
                message("Retreving the inverse of matrix from cache")
                return(inver)
        }
        ## get the matrix and solve it for inverse
        m <- x$get()
        inver <- solve(m, ...)
        ##  save the inverse matrix into current struct
        x$setInverse(inver)
        inver
}
