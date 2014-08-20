##The <makeCacheMatrix> function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	mat <- NULL
	set <- function(y) {
		x <<- y
		mat <<- NULL
	}
	get <- function() x 
	setinv <- function(solve)  mat <<- solve 
	getinv <- function()  mat 
	list(set = set, get = get,
	setinv = setinv,
	getinv = getinv)
}

##The <cacheSolve> function computes the inverse of the special "matrix"
##returned by the <makeCacheMatrix> function. If the inverse has already
##been calculated (and there has been no change to the matrix), then
##the function <cacheSolve> retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
	mat <- x$getinv()
	if(!is.null(mat)) {
		message("getting cached data")
		return(mat)
	}
	data <- x$get()
	mat <- solve(data, ...)
	x$setinv(mat)
	mat
}