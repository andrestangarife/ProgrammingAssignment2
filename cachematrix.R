#Assigment 2

## --No comments here--

# This function makes a particular matrix available to be cached by the function
#cachesolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#cacheSolve takes the matrix created below and solve it. Then, stores the result
#in a list available to be used in case to require it. I.e if we want to 
#calculate the inverse of the same matrix twice the function don't calculates 
#the inverse again, just takes the result that has been calculated before.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}