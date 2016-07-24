## In cachematrix.R the following functions compute the inverse of a matrix
## given as an input(the input matrix shoud be inversible) and then cache that
## result for future use. This eliminates the need to compute the inverse repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      mInverse <- NULL
      set <- function(y) {
            x <<- y
            mInverse <<- NULL
      }
      get <- function() x
      setInv <- function(Inverse) mInverse <<- Inverse
      getInv <- function() mInverse
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      mInverse <- x$getInv()
      if(!is.null(mInverse)) {
            message("getting cached data")
            return(mInverse)
      }
      data <- x$get()
      mInverse <- solve(data, ...)
      x$setInv(mInverse)
      mInverse
}