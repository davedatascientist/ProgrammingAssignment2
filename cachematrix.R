## The next two functions are designed in order to avoid to compute the
## inverse of a matrix repeatedly. It will be calculated once and cached so
## the next time the inverse is needed, it will be retrieved from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
  ## First we check if the argument introduced is really a square matrix.
  if (class(x) != "matrix")
    message("The argument has to be a matrix.")
  else if (nrow (x) != ncol(x))
    message("The argument has to be a square matrix.")
  else    ## If 'x' is a square matrix, we can proceed.
  {
    inv <- NULL
    
    set <- function (y)
    {
      ## First we check if the argument introduced is really a square matrix.
      if (class(y) != "matrix")
        message("The argument has to be a matrix.")
      else if (nrow (y) != ncol(y))
        message("The argument has to be a square matrix.")
      else    ## If 'y' is a square matrix, we can proceed.
      {
        x <<- y
        inv <<- NULL
      }
    }
    
    get <- function () x
    
    setinverse <- function (inverse) inv <<- inverse
    
    getinverse <- function () inv
    
    list (set = set, get = get, setinverse = setinverse,
          getinverse = getinverse)
  }
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then this function retrieves the inverse from
## the cache.

cacheSolve <- function(x, ...)
{
  inv <- x$getinverse ()
  
  ## If the inverse has already been calculated, this retrieves it.
  if (!is.null(inv))
  {
    message ("getting cached data")
    return (inv)
  }
  
  ## If the inverse has not already been calculated, we need to do it now.
  data <- x$get ()
  inv <- solve(data, ...)
  x$setinverse (inv)
  inv
}
