## Matrix caching and inversion functions

## Function to create a matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## Function to set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Function to get the matrix
        get <- function() x
        
        
        ## Function to set a matrix in cache
        setSolve <- function(sol) m <<- sol
        
        ## Function to get a matrix from cache
        getSolve <- function () m
        
        list (set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Function that returns the inverse of a matrix from cache if exists in cache
##      else it would computes inverse of a matrix and cache it
cacheSolve <- function(x, ...) {
        
        ## Get the matrix from cache. If the matrix is in cache 'm' will 
        ##      have the value from cache. If not in cache m will be NULL
        m <- x$getSolve()
        
        ## Check if we have value for 'm'. If not null return the inverse of
        ##      matrix and exit the function
        if (!is.null(m)) {
                message("From Cache")
                return(m)
        }
        
        ## If the invese doesnot exist in cache. Get the matrix into a variable 
        ##      and call the 'solve' function to inverse the matrix
        data <- x$get()
        m <- solve (data, ...)
        
        ## Cache the newly inversed matrix
        x$setSolve(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
        
}
