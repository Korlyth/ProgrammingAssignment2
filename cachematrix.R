## Created by Joshua Barnes

## makeCacheMatrix takes in a matrix value. It stores the matrix as a local variable
## you can call setMatrix or getMatrix to return the saved matrix
## you can call setInverse or getInverse to return the saved inversed matrix

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


## cacheSolve takes in a makeCacheMatrix as a parameter
## it will attempt to call get InverseMatrx on the passed parameter
## if no value returns, it will call solve on the provided matrix
# finally it will then call setInverse passing the inverted matrix

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
