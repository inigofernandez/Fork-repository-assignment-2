## INIGO FERNANDEZ SANCHEZ

## makecachematrix is the function which creates the matrix and the list for the values


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
      matrix <- x$get()
       I <- solve(matrix, ...)
       x$setInverse(I)
       I
}
