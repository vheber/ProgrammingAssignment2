## The two functions below get the inverse of a matrix
## and cache the result

## Function makeCacheMatrix creates a list containing
## a matrix and a function to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        orig <<- x
	inv <- NULL
	getinv <- function(A) {
		inv <<- solve(A)
		inv
	}
	list(orig = orig, inv = inv, getinv = getinv)
}


## Function cacheSolvle returns the inverse of 'x'
## that has been cached

cacheSolve <- function(x, ...) {
	B <- x$inv
	if(!is.null(B)) {
		return(B)
	}
	A <- x$orig
	B <- x$getinv(A)
	return(B)
}
