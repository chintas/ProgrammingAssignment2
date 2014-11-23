## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        #initialize temp matrix function s
        s <- NULL
        
        #declare set method 
        set <- function(y) {
                x <<- y
                s <<- NULL #inialize superassignment
        }
        
        #declare get method
        get <- function() x
        
        setinverse <- function(solve) s <<- solve #returns matrix inverse
        getinverse <- function() s #returns matrix inverse
        
        #It returns a list with named values where the values are the functions 
        #(aka closures) defined within the function. The fact that they're named 
        #allows you to reference them like this "x$get()" in the other method.
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
# This function computes the inverse 
#of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        s <- x$getinverse()
        if(!is.null(s)) { #return cached data
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...) #use solve function to calculate matrix inverse.
        x$setinverse(s)
        s
}
