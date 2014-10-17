## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
        ## Initialize the inverse property
    i <- NULL
    
    ## set the matrix
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    
    ## get the matrix
    get <- function () {
        
        ## return the matrix
        m
    }

    ## set the inverse of the matrix
    setInverse <- function(inverse){
        i <<- inverse
    }
    
    ## get the inverse of the matrix
    getInverse <- function(){
        
        ## return the inverse
        i
    }
    
    ## Return list of methods
    list(set=set, get=get,
         setInverse=setInverse
         getInverse=getInverse)
}

## Inverse of special matrix returned by "makeCacheMatrix" above

## If the inverse has already been calculated (and the matrix 
## has not changed), then `cacheSolve` should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Return inverse if already set
        if (!is.null(m)){
            message("getting cached data")
            return(m)
        }
        
        ## get matrix
        data <- x$get()
        
        ## compute inverse
        m <- solve(data) %*% data
        
        ## set inverse
        x$setInverse(m)
        
        ## Return matrix
        m
}
