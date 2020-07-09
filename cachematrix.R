## Put comments here that give an overall description of what your
## function to create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## defining the matrix
        Inv <- NULL                         ## iniatialize inv as NULL    
        set <- function(y){                 ## set function to assign new
                x <<- y                     ## value of matrix
                Inv <<- NULL                ##  if new matrix reset to null
        }
        get <- function() x                 ## define the get fucntion - 
        ## returns value of the matrix argument
        setInverse <- function(solve) Inv <<- solve
        ## assigns value of inv in parent environment
        getInverse <- function() Inv
        ## gets the value of inv where called
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## Write a short comment describing this function
## function that computes the inverse of the special matrix returned by 
##makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInverse()
        if(!is.null(Inv)){
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        Inv      
}