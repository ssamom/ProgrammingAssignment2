## "makeCacheMatrix" function caches the inverse of a matrix, computed by "cacheSolve" function
## Caching is achieved by using the "<<-" operator

## This function creates a special "matrix" object that can cache its inverse
## It returns a list of functions which allows to set and get the values of the matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  
    mxInverse <- NULL
    
    set <- function(y){
        x <<- y
        mxInverse <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setInverse <- function(m_inverse){
        mxInverse <<- m_inverse
    }
    getInverse <- function(){
        mxInverse
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function first checks if the cache is NULL, if not it returns the value
## otherwise it computes the inverse, assign it to the cache and then returns the result

cacheSolve <- function(x, ...) {
  
    mi <- x$getInverse()
    
    if(!is.null(mi)){
        
        message("getting cache value")
        return(mi)
    
    }
    m <- x$get()
    mi <- solve(m)
    x$setInverse(mi)
    mi
}
