##Coursera Class R Programming
##

## makeCacheMatrix
## this function returns a special list structure that aims to cache
## the inverse of a matrix object x that is passed as a parameter to this 
##function
makeCacheMatrix <- function(x = matrix()) {
        #inv (the cached value of the inverse matrix) is initalized to NULL, 
        # i.e. no value is cached
        inv <- NULL
        
        # this is the set function that we use to assign the matrix to be
        #inverted
        # we also reset the cache since the value may be different
        #(note: ideally we could check if the x matrix actually changed and 
        #only reset if it did, but we'll keep it simple)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # this function will allow us to access the original matrix x 
        get <- function() x
        
        #this function will allow us to set the inverse matrix value to the
        #"cached" value
        #note that we need to use << symbol to make sure the inv variable is
        #just a local version
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
## that will contain a few helper functions that we can use to set or access
## the matrix to be inverted and retrive or set the cached value of the
## inverse operation.
## Basically if the cached value is present, it will be returned directly
## otherwise the value is computed, then saved in the "cache" and then returned
cacheSolve <- function(x, ...) {
        
        #let's get the inverse matrix from the cache
        inv <- x$getinverse()
        
        #if the cached value is NOT null, i.e. we have cached the value before
        #let's use and return the value directly
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #otherwise, let's calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        #and set the value in the cache for later use
        x$setinverse(inv)
        
        #and return the value
        inv
}
