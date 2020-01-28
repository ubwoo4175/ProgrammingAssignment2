## Put comments here that give an overall description of what your
## functions do

## creates a vector for the function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(i) inv <<- i
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## computes the inverse of the special matrix returned by the function makeCacheMatrix()
## if the inverse already been calculated, retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message('getting cached data')
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix)
	x$setinv(inv)
	inv
}
