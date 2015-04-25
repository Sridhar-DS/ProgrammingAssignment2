## Put comments here that give an overall description of what your
## functions do

## 1. makecacheMatrix:-
## This function creates a LIST object with four elements
## that are required hold the inverse of the matrix. 
## In short the variables and environments are set.
## The input square matrix is saved in the list

## 2.CacheSolve
## This function invokes the elemnets of the list by 
## utlizing the output of makecacheMatrix
## For the first time the inverse of the matrix is calculated 
## by this function. Then it is cached.
## From the second time the inverse is searched in the cache.

## 3. How to execute from R
## Example:
## a<-matrix(c(1,10,3,8,9,7,5,4,6),nrow=3,ncol=3)
## k<-makeCacheMatrix(a)
## cacheSolve(k)



## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    ## @x: a square invertible matrix
    ## return: a list containing functions to
    ##              1. set the matrix
    ##              2. get the matrix
    ##              3. set the inverse
    ##              4. get the inverse
    ##         this list is used as the input to cacheSolve()
    
    inv = NULL
    set = function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment. 
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)    
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## @x: output of makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
    
    inv = x$getinv()    
    
    # if the inverse has already been calculated
    if (!is.null(inv)){
        # get it from the cache and skips the computation. 
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, calculates the inverse 
    mat.data = x$get()
    inv = solve(mat.data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(inv)
    
    return(inv)  
    
}
