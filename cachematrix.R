## Functions that cache the inverse of a matrix.
## 
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
          ## This variable holds the cached inverse value, and is always set to NULL
          ## when creating a new matrix or it is changed
          matrix_inverse <- NULL
          ## this inner function set a  new matrix
          set <- function(y) {
            x <<- y
            m <<- NULL ##the matrix has changed and therfore the cache must be set to NULL
          }
          ## this inner function gets the matrix
          get <- function() x
          ## 
          setinverse <- function (inverse) matrix_inverse <- inverse
          getinverse <- function() matrix_inverse
          ##a list that contain the different inner function of the makeCache function
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Gets the current inverse matrix from the matrix object
        matrixinverse <- x$getinverse()
        ## It the inverse matrix is avaible, the use the cached one and return it
        if(!is.null(matrixinverse)) {
              message("getting cached data")
              return(matrixinverse)
        }
        ## if no inverse matrix i avaible, get the matrix, calculate the inverse matrix,
        ##set it on the matrix object, and return the inverse matrix
        matrixdata <- x$get()
        matrixinverse <- solve(matrixdata, ...)
        x$setinverse(matrixinverse)
        matrixinverse
}
##For testpurpose:
matrix <- makeCacheMatrix(matrix(c(3,7,7,9), nrow=2, ncol =2))
cacheSolve(matrix)
