## Put comments here that give an overall description of what your
## functions do

## makecachematrix is the function which creates the matrix and the list for the values
## INIGO FERNANDEZ SANCHEZ

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
         set <- function(y) {
           x <<- y
             I <<- NULL
        }
        get <- function() x
          setInverse <- function(inverse) I <<- inverse
          getInverse <- function() I
          list(set = set,
              get = get,
              setInverse = setInverse,
                getInverse = getInverse)

}


## cachesolve checks if the inverse has been calculated before and if it is not, calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
        if (!is.null(I)) {
                    print("The inverse has been calculated before, using cache memory")
    return(I)
  }
      mat <- x$get()
       I <- solve(mat, ...)
       x$setInverse(I)
       I
}
