# makeCacheMatrix is a function created to: set the value of matrix, 
# get the value of matrix, set the value of inverse and set the value of inverse for the matrix
makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
	# set the value of the matrix
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
	# get the value of the matrix
        get <- function() x
	# set the value of inverse of the matrix
        setinverse <- function(inverse) invs <<- inverse
	# get the value of inverse of the matrix
        getinverse <- function() inv
        list(set=set, get=get, 
		setinverse=setinverse, 
		getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 
# Checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation
# If not, it computes the inverse, sets the value in the cache via setinverse function


# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        invs <- x$getinverse()
        if(!is.null(invs)) {
                message("getting cached data.")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data)
        x$setinverse(invs)
        invs
}