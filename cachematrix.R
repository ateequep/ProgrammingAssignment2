# Below functions help compute the inverse of a matrix smartly by 
# retrieving the previous calculated inverse if any provided the 
# matrix has not changed.
#
# Create a special "matrix" object to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) { # take matrix as input
        
        inverse <- NULL  # set inverse to NULL for each iteration
        
        set <- function(matrix) {
                x <<- matrix  # perform superassignment to store input matrix
                inverse <<- NULL  # perform superassignment to store inverse matrix
        }
        
        get <- function() x  # return the original input matrix
        setInverse <- function(inverse) inverse <<- inverse  # superassign the inverse of matrix    
        getInverse <- function() inverse  # return the inverse of matrix on calling object
        
        #  return the list of access methods to access using calling object
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# Compute the inverse of a matrix if no cached result available

cacheSolve <- function(x, ...) {
        
        inverse <- x$getInverse()  # retrieve the inverse matrix of 'x'
        
        # check if the inverse of matrix 'x' is cached and return if available
        if(!is.null(inverse)) {
                message("retrieving cached data")
                return(inverse)
        }
        matrix <- x$get()  # retrieve the original matrix 
        inverse <- solve(matrix,...)  # compute the inverse matrix
        x$setInverse(inverse)  # set the inverse matrix of 'x'
        inverse  # Return a matrix that is the inverse of 'x'
}
