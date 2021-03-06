## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
    ## Matrix Init
    inv <- NULL
    
    ## Set Matrix
    set <- function(matrix){
          x <<- matrix
          inv <<- NULL
      
    }
    ## Get Matrix and return
    get <- function() x
    
    ## Set Inverse
    setInverse <- function(inverse) inv <<- inverse
    
    ## Get Inverse
    getInverse <- function()inv
    
    ## Return methods in a list
    list(set = set, get = get,setInverse = setInverse,
         getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    ## Return inverse if already set
    if(!is.null(inv)){
          message("getting cached data")
          return(inv)
    }
    
    ## Get matrix
    data <- x$get()
    
    ## Calculate inverse
    inv <- solve(data, ...)
    
    ## Set Inverse
    x$setInverse(inv)
    
    ## Return
    inv
}
