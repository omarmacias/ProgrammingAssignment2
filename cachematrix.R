## The makeCacheMatrix function creates a matrix that will allow its inverse to be cached.
## The cacheSolve function inverses the matrix and retrieves the cache of the matrix.

## This function will make the matrix that can be cached if necessary.

makeCacheMatrix <- function(x = matrix()) {
# Create a variable that is null and a set function for the matrix.
	m <- NULL
    set <- function(y)
	{
        x <<- y
        m <<- NULL
    }
# Creates the get function and the get, and set matrix functions.
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function returns the cached matrix if necessary and inverses the matrix.

cacheSolve <- function(x, ...) {
# Puts the value of x from the getmatrix function from makeCacheMatrix in m.
		m <- x$getmatrix()
# If m is not null, a message is displayed that the cached data is being retrieved
# and the value of m is returned.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
# Gets the value of get from makeCacheMatrix, creates its inverse, and returns its value.
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
