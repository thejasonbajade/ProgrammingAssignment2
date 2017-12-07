## Computes and caches the inverse of a matrix.

## Creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(inputMatrix = matrix()) {
	inversedMatrix <- NULL
	set <- function(y) {
		inputMatrix <<-y
		inversedMatrix <<- NULL
	}
	get <- function()inputMatrix
	setInverse <- function(inverse) inversedMatrix <<- inverse
	getInverse <- function() inversedMatrix 
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## Computes the inverse of the special matrix object returned 
## by makeCacheMatrix.

cacheSolve <- function(x, ...) {
	inversedMatrix <- x$getInverse()
	if(!is.null(inversedMatrix)) {
		print("Returning cached data")
		return(inversedMatrix )
	}
	inputMatrix <- x$get()
	inversedMatrix <- solve(inputMatrix )
	x$setInverse(inversedMatrix )
	inversedMatrix 
}
