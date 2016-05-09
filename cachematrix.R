## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(myMatrix = matrix()){
    inverseMatrix <- NULL
    setMatrix <- function(passedMatrix) {
        myMatrix <<- passedMatrix
        inverseMatrix <<- NULL
    }
    
    getMatrix <- function() {
        myMatrix
    }
    
    setInverse <- function(passedInversedMatrix) {
        inverseMatrix <<- passedInversedMatrix
    }
    
    getInverse <- function() {
        inverseMatrix
    }
    
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(myMatrix, ...) {
    inverseMatrix <- myMatrix$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    tempMatrix <- myMatrix$getMatrix()
    inverseMatrix <- solve(tempMatrix, ...)
    myMatrix$setInverse(inverseMatrix)
    inverseMatrix
}
