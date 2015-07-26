## This functions mianly cater for the assignment in order to cache the inverse of a Matrix

## This function sets a matrix and caches its Inverse

makeCacheMatrix <- function(x=matrix()){
    inv <- NULL
    
    ## This is to set the Matrix
    set <- function(y){
        matrix <<- y
        inv <<- NULL
    }
    
    ## This is to retrieve the Matrix
    get <- function(){
        matrix
    }
    
    ## Set the Inverse of the Matrix
    setInverse <- function(inverse) {
        ## To store the Inverse
        inv <<- inverse
    }
    
    ## This is to Get the Inverse of the Matix
    getInverse <- function() {
        inv
    }
    
    ## This is to print the list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function commputes the Inverse of the Matrix returned by the "MakeCacheMatrix" function

cacheSolve <- function(x, ...) {
    ## This is to get the Inverse of x
    inv <- x$getInverse()
    
    ## If the Inverse was computed, then returns
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If the inverse was not computed, then do the following
    
    ## Get the matrix
    data <- x$get()
    
    ## Compute the Inverse
    m <- solve(data) %*% data
    
    ## Store the Inverse
    x$setInverse(m)
    
    ## returning a matrix that is the inverse of 'x'
    m 
}
