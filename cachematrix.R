## together, these two functions allow us to create a special 
## matrix object that can store its own inverse out of a 
## normal matrix object x

## makeCacheMatrix() turns a given input matrix x
## into a list of functions that allows x
## to be processed by the second function in the assignment:
## cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
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
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        ##retrieves the cached m value if it exists
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##if it does exist, the command line fetches it and indicates as such
        data <- x$get()
        ##x$get() is equal to our original x matrix
        m <- solve(data, ...)
        ##sets the m free variable in the cachable matrix object to be 
        ##equal to the inverse of x
        x$setsolve(m)
        ##caches the inverse in the cachable matrix object
        m
        ##prints the inverse m of the original matrix x
}


## below is the output of the cacheSolve function when passed 
## the special matrix object n, which is made from the normal
## matrix object d
> d <- matrix(rnorm(9,0), 3,3)
> n <- makeCacheMatrix (x = d)
> d
            [,1]       [,2]       [,3]
[1,] -0.03023722 -1.4349634 -0.4078301
[2,] -0.22969828 -0.9757108 -0.6018175
[3,]  0.51192432 -0.4896223 -0.3113785
> cacheSolve(n)
            [,1]       [,2]       [,3]
[1,]  0.03103749 -0.8381030  1.5791937
[2,] -1.28736079  0.7399568  0.2559765
[3,]  2.07531786 -2.5414234 -1.0177456
> cacheSolve(n)
getting cached data
            [,1]       [,2]       [,3]
[1,]  0.03103749 -0.8381030  1.5791937
[2,] -1.28736079  0.7399568  0.2559765
[3,]  2.07531786 -2.5414234 -1.0177456
> 