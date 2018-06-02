## These functions will recieve a square matrix as a parameter and will return the inverse value
## of the matrix.

####################################################################################################
## The makeCacheMatrix() function will recieve a square matrix as a parameter to be inverse.
## The function will return a list of 4 functions. The set(), get(), setInverse() and
## getInverse() functions.
##
makeCacheMatrix <- function(x = matrix()) {
    isqm <- NULL

    set <- function(y) {

        x <<- y

        isqm <<- NULL

    }

    get <- function() x
    setInverse <- function(solve) isqm <<- solve
    getInverse <- function() isqm
    list(set = set, get = get,

         setInverse = setInverse,

         getInverse = getInverse)
}
####################################################################################################

####################################################################################################
## The cacheSolve() function will recieve the list, returned by the makeCacheMatrix() function,
## as a parameter. This function will inverse the square matrix passed to the makeCacheMatrix()
## function. And it will cached the computed inverse matrix value into memory before returning the
## inverse matrix as the function return value.
##
## Note: 
## 1) If the parameter matrix has been previously processed and cached in memory, the function will
##    retrieve the inverse matrix from memory instead of re-processing it.
## 2) If a numeric vector is paased as a parameter, the function will convert the vector into a
##    square matrix. However, it will issue a warning that the parameter passed was not a matrix
##    before returning the inverse matrix value.
## 3) The function will issue an error if the matrix passed is not a square matrix. And it will
##    return a null value.
##
cacheSolve <- function(x, ...) {
    isqm <- x$getInverse()
    if(!is.null(isqm)) {
        message("Getting cached data.")
        return(isqm)
    }

    data <- x$get()

    if (is.matrix(data)) {     ## check if argument data passed is a matrix
        if (! dim(data)[1] == dim(data)[2]) {     ## check if matrix passed is square
           message(paste("Error in matrix values to process. It is not a square matrix (",
                         nrow(data), " x ", ncol(data), ").", sep = ""))
           return()
        }
    }
    else {
        message("Warning... values passed is not a matrix item. Will convert to a square matrix.")

        if (! sqrt(length(data)) - round(sqrt(length(data))) == 0) {     ## check if nrow equals ncol
            message("Error... cannot convert to a square matrix.")
            return()
        }
        else {
            data <- matrix(data, sqrt(length(data)), sqrt(length(data)))
        }
    }

    isqm <- solve(data, ...)
    x$setInverse(isqm)

    ## Return a matrix that is the inverse of 'x'
    isqm
}
####################################################################################################

####################################################################################################
## Testing for makeCacheMatrix() and cacheSolve() functions
## Run each command without the "#" in R

# # Testing for a square matrix parameter
# A <- matrix(c(1, 2, 3, 4), 2, 2)
# A
# mcA <- makeCacheMatrix(A)
# cacheSolve(mcA)
# cacheSolve(mcA)

## Testing for another square matrix parameter
# B <- matrix(c(1, 22, 35, 4, 5, 6, 74, 8, 9), 3, 3)
# B
# mcB <- makeCacheMatrix(B)
# cacheSolve(mcB)
# cacheSolve(mcB)

## Testing if the previously cached matrix will be re-processed or if the
## inverse value will be retrieved from memory
# cacheSolve(mcA)
# cacheSolve(mcB)
# mcB <- makeCacheMatrix(B)
# cacheSolve(mcB)
# cacheSolve(mcB)
# cacheSolve(mcA)
# cacheSolve(mcB)

## Testing for a matrix parameter that is not square
# C <- matrix(c(1, 22, 35, 4, 5, 6), 2, 3)
# C
# mcC <- makeCacheMatrix(C)
# cacheSolve(mcC)

## Testing for a vector parameter that can be converted into a square matrix
# D <- c(1, 2, 3, 4)
# D
# mcD <- makeCacheMatrix(D)
# cacheSolve(mcD)

## Testing for a vector parameter that cannot be converted to a square matrix
# E <- c(1, 22, 35, 4, 5, 6)
# E
# mcE <- makeCacheMatrix(E)
# cacheSolve(mcE)
####################################################################################################
