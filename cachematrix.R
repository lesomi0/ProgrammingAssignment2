## Put comments here that give an overall description of what your
## functions do
# functions that cache the inverse of a matrix

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { #initialize x
        i <- NULL # initialize i, will hold the inverse
        set <- function(y) { 
                x <<- y    # assign the input argument to the x object in the parent environment
                i <<- NULL # clears any value of i that had been cached by a prior execution of cacheSolve()
        }
        get <- function() x  # getter for the matrix x
        setInverse <- function(inverse) i <<- inverse  ## setter for the inverse i
        getInverse <- function() i ## gets the value of i
        list(set = set, get = get,  # give names to the elements
             setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {  # check whether the result is null
                message("getting the cahced data")
                return(i)
        }
        data <- x$get() 
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
