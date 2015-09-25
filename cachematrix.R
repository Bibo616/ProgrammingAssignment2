## These functions are used for creating, inverting
## and caching a matrix


## The function below creates a special
## type of matrix which is actually a list
## containing functions to:

## 1: Set the value of the matrix
## 2: Get the value of the matrix
## 3: Set the value of the matrix inverse
## 4: Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    
    ## 1:
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## 2:
    get <- function() x
    
    ## 3:
    setInverse <- function(solve) m <<- solve
    
    ## 4:
    getInverse <- function() m
    
    
    ## the list that is returned
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function below calculates the matrix inverse
## or just returns the cached inverse if possible

cacheSolve <- function(x, ...) {

    m <- x$getInverse()
    
    ## check whether m has already
    ## been computed and cached before
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## else: get and invert the matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    
    return(m)
}
