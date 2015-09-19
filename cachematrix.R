## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly.  The following pair of function will cache
## the inverse of a matrix.  If the matrix has not changed, its 
## previously computed inverse will be retrieved from the cache
## rather than recomputed.


## This function creates a special "matrix" object that can cache its inverse.
## The special "matrix" is a list containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matirx
## 4) get the value of the inverse of the matirx
makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inversematrix) invm <<- inversematrix
  getInverseMatrix<- function() invm
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getInverseMatrix()
  if(!is.null(invm)) {
    message("getting cached inverse of matrix")
    return(invm)
  }
  matrixdata <- x$get()
  invm <- solve(matrixdata)
  x$setInverseMatrix(invm)
  invm
}
