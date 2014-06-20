## The following functions were created in order to cache the inverse of a matrix so that it
## wouldn't have to be calculated reapeatedly if the matrix stays unchanged 

## The makeCacheMatrix function creates a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {   ## Reads in an empty matrix by default
  I <- NULL                       ## Sets object I, which will be the inverse, as NULL
  set <- function(y) { ## This function assigns values to x and I in the call environment
    x <<- y
    I <<- NULL
  }
  get <- function() x                         ## This function prints the value of matrix x
  setinv <- function(inv) I <<- inv   ## This assigns a vlaue to I in the call environment
  getinv <- function() I           ## This function prints the value of I (the inverse of x)
  list(set = set, get = get, setinv = setinv, getinv = getinv) ## Creates a list of functions
}


## The cacheSolve function computes the inverse of the special matrix that was returned by
## the makeCacheMatrix function above. If the matrix hasn't changed (and the inverse has
## already been calculated), this function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  I <- x$getinv()   ## Looks up the value of the inverse as found in the getinv function
  if(!is.null(I)) {  ## Checks if there is already an inverse for this matrix 
    message("Getting cached data")
    return(I)        ## And fetches that inverse form the cache if there is one
  }
  data <- x$get()        ## If there isn't an inverse already, it takes the matrix x, and:
  I <- solve(data, ...)  ## Calcultes its inverse
  x$setinv(I)            ## And assign the inverse using the setinv function    
  I                      ## Return a matrix that is the inverse of matrix x
}

