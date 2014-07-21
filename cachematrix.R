## The following functions cache the inverse of a matrix,
## so that if we need the matrix inverse again, 
## it can be looked up in the cache rather than recomputed.

## The "makeCacheMatrix" function returns a cache in the form of a list 
## which contains four functions and which can access the original matrix 
## and the matrix inverse if it's computed and cached by function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
             x <<- y
             inv <<- NULL
     }
     get <- function()x  # Returns original matrix
     setinverse <- function(solve) inv <<- solve
     getinverse <- function() inv  # Returns matrix inverse if it has been cached
     list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## The "cacheSolve" function calculates the inverse matrix on the list returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  
        ## Computes, caches, and returns matrix inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
