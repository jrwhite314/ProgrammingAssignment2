## Cache the inverse of a matrix for easy retrieval after initial calculation
## Programming Assignment 2

## makeCacheMatrix defines a set of functions that are associated with the
## matrix x.  These functions are used to assign (set) a matrix for operation,
## retrieve the matrix for manipulation, set the inverse, and get the inverse.
##
## Example call(inputMatrix is an invertible matrix):
## z <- makeCacheMatrix(inputMatrix)
## 
## z can then be used with cacheSolve


makeCacheMatrix <- function(x.mat = matrix()) {
    x.inv <- NULL
    set.mat <- function(y) {                        ## Specifies new matrix
        x.mat <<- y
        x.inv <<- NULL
    }
    get.mat <- function() x.mat                     ## Grab working matrix
    set.inv <- function(mat.inv) x.inv <<- mat.inv  ## Commit to cache
    get.inv <- function() x.inv                     ## Retrieve from cache
    list(set.mat = set.mat,
         get.mat = get.mat,
         set.inv = set.inv,
         get.inv = get.inv)
}


## cacheSolve uses the makeCacheMatrix object defined above to check if the
## matrix inverse has already been calculated and if so retrieves the inverse
## from cache, otherwise the inverse is calculated and returned.

cacheSolve <- function(x.mat, ...) {
    x.inv <- x.mat$get.inv()            ## Retrieve inverse from cache
    if(!is.null(x.inv)) {               ## If inverse exists, return
        message("getting cached data")
        return(x.inv)
    }
    in.mat <- x.mat$get.mat()           ## Otherwise retrieve data
    x.inv <- solve(in.mat)              ## Calculate inverse
    x.mat$set.inv(x.inv)                ## Place inverse in cache
    x.inv                               ## Return the inverse
}
