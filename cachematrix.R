## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object to cache "matrix" inverse. 

makeCacheMatrix <- function(x = matrix()) {

        iv <- NULL              ## iv reset to "NULL" each time called.
        
        set <- function(y) {    ## takes and store input vector
                                ## and make iv "NULL" with each new object
                x <<- y
                iv <<- NULL
        }
        get <- function() {x}
        
        setiv <- function(solve) {iv <<- solve} ## called during first 
                                                ## cacheSolve call.
        getiv <- function() {iv}                ## return iv for same matrix
                                                ## if called > once.
        
        list(set = set, get = get,
             setiv = setiv,
             setiv = setiv)             ## list internal functions for repeat        
        
        
}


## computes inverse of "matrix" returned from "makeCacheMatrix" but
## will not recalculate inverse of "matrix" if already calculated 
## before given the contents of the "matrix" has not changed.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'

        iv <- x$getiv           ## access mean of matrix x
        
        if(!is.null(iv)) {                      ## check if x inverse already 
                                                ## calculated.
                message("getting cached data")
                return(iv)
        }
        
        data <- x$get()                 ## calculate inv of x if only "NULL"
        iv <- solve(data, ...)
        x$setiv(iv)
        iv
        

}
























