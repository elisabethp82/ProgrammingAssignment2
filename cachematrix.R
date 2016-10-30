## makeCacheMatrix creates a cache in which an inverted matrix may be stored
## cacheSolve returns a matrix that is the invert of "x", using cached data if
## the same matrix has been assigned to x in the previous call.


## makeCacheMatrix initializes a cache in which the inverted matrix is stored

## initializing x and defining it as a matrix
makeCacheMatrix <- function(x = matrix()) {
        
        ## initializing cache within the makeCacheMatrix environment
        cache <- NULL
        
        ## defining set function
        set <- function(y) {
                ## assigning the input argument y to x in the parent environment 
                x <<- y
                ## resetting cache to NULL in parent environment if x changes
                cache <<- NULL
        }
        
        ## defining get function, taking x from parent environment
        get <- function() x
        
        ## defining set function for inversion of matrix
        ## setting cache to inversion of matrix in parent environment
        setinvert <- function(invert) cache <<- invert
        
        ## defining get function for inversion of matrix
        getinvert <- function() cache
        
        ## assigning functions to an element within a list
        list(
                ## gives the name "set" to the set() function
                set = set, 
                
                ## gives the name "get" to the get() function
                get = get,
             
                ## gives the name "setinvert" to the setinvert() function
                setinvert = setinvert,
                
                ## gives the name "getinvert" to the getinvert() function
                getinvert = getinvert)
}


## returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        ## assigns the inverted matrix of x to cache
        cache <- x$getinvert()
        
        ## checks if chache is empty  
        if(!is.null(cache)) {
                ## IF NOT prints out the message "getting cached data"
                message("getting cached data")
                ## and returns value of cache
                return(cache)
        }
        
        # else assigns ????
        data <- x$get()
        
        ## calculates inverted matrix on input from data and assigns it to cache
        cache <- solve(data, ...)
        
        ## assigns inverted matrix to cache 
        x$setinvert(cache)
        
        ## prints out cache
        cache
}