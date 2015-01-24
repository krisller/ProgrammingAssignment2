## There are two functions in this file, the first is makeCacheMatrix that create a list of functions
## and variables to store Matrix X value and inverse Matrix X. The second is cacheSolve that calculate
## the inverse matrix and return its value.


## makeCacheMatrix return a list of functions that is used to get and set values to variables (memory).
## Param: A matrix

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL                   ## create a variable 'cache' to store the result of inverse matrix
        set <- function(y) {            ## create 'set' function to change value of param 'x'
                x <<- y                 ## change 'x' value in parent environment
                cache <<- NULL          ## set 'cache' value to NULL in parent environment,... 
        }                               ##...it forces cacheSolve recalculate the inverse matrix
        get <- function() x             ## create 'get' function to return the matrix value in 'x'
        setsolve <- function(solveValue) cache <<- solveValue   ## create 'setsolve' function to set value
                                                                ## to 'cache' variable in parent environment
        getsolve <- function() cache    ## create 'getsolve' function to return 'cache' value
        list(set = set, get = get,        
             setsolve = setsolve,       
             getsolve = getsolve)       ## return a list of functions
}


## cacheSolve function calculate and return the inverse matrix of 'x'.
## If inverse matrix already calculated it will just return the value.
## This function use makeCacheMatrix functions to get and set the inverse matrix value. 
## Param: list of functions create in makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        inverse <- x$getsolve()         ## get matrix inverse value in cache 
        if(!is.null(inverse)) {         ## test if inverse value is not null
                message("getting cached data")  ## ...print message showing that value is from memory
                return(inverse)                 ## Return value from cache
        }
        data <- x$get()                 ## Get matrix value
        inverse <- solve(data, ...)     ## Calculate inverse matrix from 'data'
        x$setsolve(inverse)             ## set value from inverse matrix to cache (memory)
        inverse                         ## return the inverse matrix value
}
