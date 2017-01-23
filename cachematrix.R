## Put comments here that give an overall description of what your
## functions do
## Functions create a matrix that can cache its inverse  

## Write a short comment describing this function
## Creates a matrix that can store its cached inverse 
makeCacheMatrix <- function(x = matrix()) 
{
	inverseMatrix <- NULL 
	set <- function(y)
	{
		x <<- y
		inverseMatrix <<- NULL
	}
	get <- function() x 
	setInverse <- function(inverse) inverseMatrix <<- inverse 
	getInverse <- function() inverseMatrix 
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Gets inverse of the matrix, uses inverse from the cache if inverse already has been calculated

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix))
        {
        	message("getting the cached data")
        	return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
