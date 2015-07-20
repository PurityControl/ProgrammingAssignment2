## makeCacheMatrix takes a matrix with the view of caching its
## inverse in order to eliminate expensive repetitive computation
## The inverse is lazily evaluated. It won't be calculated until
## it is needed. Once calculated, however, it will be cached until
## the matrix itself changes.

## makeCacheMatrix takes a matrix whose inverse you would like to cache.
## If given no arguments it creates an empty matrix
## Assumptions:-
##   - A valid square matrix will be passed in
## Permitted Operations
##  -  makeCacheMatrix$get returns the matrix
##  -  makeCacheMatrix$set(matrix)  sets a new matrix
##  -  makeCacheMatrix$getinverse gets the matrix inverse if it has
##     been calculated yet
##  -  makeCacheMatrix$setinverse(matrix) sets the matrix inverse to the
##     new value
makeCacheMatrix <- function(cache_matrix = matrix()) {
    inverse <- NULL
    set <- function(new_matrix) {
        cache_matrix <<- new_matrix
        inverse <<- NULL
    }
    get <- function() cache_matrix
    setinverse <- function(new_inverse) inverse <<- new_inverse
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve takes a instance of makeCacheMatrix and returns the
## inverse of the matrix.
## if the inverse has already been calculated it returns the cache
## otherwise it calculates the inverse, caches it and returns the value.

cacheSolve <- function(cache_matrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- cache_matrix$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data - inverse of matrix.")
        return(inverse)
    }
    data <- cache_matrix$get()
    inverse <- solve(data)
    cache_matrix$setinverse(inverse)
    inverse
}
