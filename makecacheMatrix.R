## makecacheMatrix() turns a given input matrix x
## into a list of functions that allows x
## to be processed by the second function in the assignment:
## cacheinverse()
makecacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## Calls x, the input matrix
        setsolve <- function(solve) m <<- solve
        ## caches the inverse of x as object m
        getsolve <- function() m
        ## retrieves the cached inverse matrix
        
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        ## this list will be used by cacheinverse to operate on the 
        ## cachable version of the original input matrix x
}