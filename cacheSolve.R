cacheinverse <- function(x, ...) {
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

