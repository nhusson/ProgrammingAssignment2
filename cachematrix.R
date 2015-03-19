## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## this function returns a special list structure that aims to cache
## the inverse of a matrix object x that is passed as a parameter to this 
##function
makeCacheMatrix <- function(x = matrix()) {
        #inv (the cached value of the inverse matrix) is initalized to NULL, 
        # i.e. no value is cached
        inv <- NULL
        
        # this is the set function that we used to assign the matrix to be
        #inverted
        # we also reset the cache since the value may be different
        #(note: ideally we could check if the x matrix actually changed and 
        #only reset if it is, but we'll keep it simple)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # this function will allow us to access the original matrix x 
        get <- function() x
        
        #this function will allow us to set the inverse matrix value to the
        #"cached" value
        setinverse <- function(inverse) inv <<- inverse
        
        #this function will allows us to get the value of the inverse matrix
        #from the "cached" value...
        #(note: this value will be NULL if the cached value hasn't been set
        # yet)
        getinverse <- function() inv
        
        #finally we build a list with all our helper functions, with the
        #appropriate labels (to facilate the function calls)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve
## this function will return the inverse of object 'x'...
## note that x is not a "pure" matrix object, it's actually a list object
## that will contain the matrix to be inverted, as well as the cached value
## of the inverse of this matrix (if available)
## basically if the cached value is present, it will be returned directly
## otherwise the value is computed, then saved in the "cache" and returned
cacheSolve <- function(x, ...) {
        
        #let's get the inverse matrix from the cache
        inv <- x$getinverse()
        
        #if the cached value is null
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}
