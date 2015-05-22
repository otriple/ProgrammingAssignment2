## This piece of code contains two functions
## 1. makeCacheMatrix which creates a special matrix object that can cache its inverse
## 2. cacheSolve - computes the inverse of a matrix returned by makeCacheMatrix by calling the solve function

## This function returns a list of functions used to set, get, set inverse and get inverse of the input matrix. 
## It caches the inverse matrix if available
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y           ## assigns the matrix to x
                m <<- NULL        ## resets the inverse to null 
        }
        get <- function() {       ## for seeing the value of x
                x
        }
        setInv <- function(Inverse) {
                m <<- Inverse     ## assigns the inverse to m
        }
        getInv <- function(){
                m                 ## for getting m when desired
        } 
        list(set = set, get = get,setInv = setInv,getInv = getInv)
}

## cacheSolve is used to return the inverse of the matrix if it already exists from the cache or solve 
## it in the event the matrix is new
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()          ## attempts getting the inverse from the cache
        if(!is.null(m)) {        ## if m actually has a value then cached data exists
                message("getting cached data")
                return(m)
        }
        data <- x$get()          ## if there is no cached data, it gets the input matrix
        m <- solve(data, ...)    ## solves the inverse of the matrix
        x$setInv(m)              ## then sets the value of the inverse to m in makeCacheMatrix, caching the inverse
        m
}
