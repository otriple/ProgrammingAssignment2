## This piece of code contains two functions
## 1. makeCacheMatrix which creates a special matrix object that can cache its inverse
## 2. cacheSolve - computes the inverse of a matrix returned by makeCacheMatrix by calling the solve function

## This function returns a list of functions is used to set, get, set inverse and get inverse of the input matrix. It caches the inverse matrix if available

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setInv <- function(Inverse) {
                m <<- Inverse
        }
        getInv <- function(){
                m    
        } 
        list(set = set, get = get,setInv = setInv,getInv = getInv)
}


## cacheSolve is used to return the inverse of the matrix if it already exists from the cache or solve it in the event the matrix is new

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
