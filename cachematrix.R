## makeCasheMatrix creates a special "matrix", which is list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <-function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inv) {inverse <<- inv}
    getinverse <- function() {inverse}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## cacheSolve function calculates the inverse of the special "matrix" created with 
## makeCacheMatrix function. It first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in 
## the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <-x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return (inverse)
    }
    data <-x$get()
    inverse <-solve(data, ...)
    x$setinverse(inverse)
    inverse 
}
