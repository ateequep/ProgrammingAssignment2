## Below functions help compute the inverse of a matrix smartly by 
## retrieving the previous calculated inverse if any provided the 
## matrix has not changed.

## Create a special "matrix" object to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(matrix) {
                x <<- matrix
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverse <<- inverse
        getInverse <- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Compute the inverse of a matrix if no cached result available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("retrieving cached data")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix,...)
        x$setInverse(inverse)
        inverse
}
