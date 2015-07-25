## The following functions will compute the inverse of a matrix and cache 
## the result for further use

## This function creates a special list to be stored in the memory containing the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL ## Inverse of the matrix
	set <- function(y){ ## Initializes the values of the matrix and inverse setting the initial value of inverse to NULL
		x <<- y
		inv <<- NULL
	}
	get <- function() x ## Returns the original matrix
	setinv <- function(i) inv <<- i ## Sets the value of inverse as passed to the function
	getinv <- function() inv ## Returns the inverse of the matrix
	list(set = set, get= get, getinv = getinv, setinv = setinv)
}


## The following function returns the inverse of the matrix

cacheSolve <- function(x, ...) {

    inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse") ## If the inverse is already cached, the cached value is returned and inverse is not calculated again
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv ## Value of inverse is calculated and returned if not already calculated
}
