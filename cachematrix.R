## Allows to create a matrix object that caches its inverse
## and to solve the matrix after checking whether a cached inverse is available for use


## Creates an object that can contains the matrix along with its cached inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Tries to find a cached version of the inverse
## if it is not found (NULL),
## it computes the inverse and sets its value to the cache (for further use)

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if(!is.null(inv)){
          message("Getting cached inverse")
          return(inv)                        # Cached inverse found, returning it
     }
     matrix <- x$get()
     inv <- solve(matrix)                    #Cached inverse not found. Computing it
     x$setInverse(inv)
     inv
}
