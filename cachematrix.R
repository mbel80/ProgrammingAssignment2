##
## This R file includes two functions -- 
##
##    makeCacheMatrix()
##    cacheSolve()
##
## -- that can be used to:
## 
## - calculate the inverse of a matrix 
##
## - cache both the matrix and the calculated inverse for possible future use
##
## - for a subsequent request to invert the matrix:
##
##   - check to see if the inverse matrix already exists in the cache:
##
##     - if so, return the cached matrix
##     - if not, calculate the inverse matrix & cache it for future reference  
##
## Assumptions:
##
##  - it is assumed that the inputted matrix IS INVERTIBLE.
##


##
## makeCacheMatrix()
## -----------------
##
## This function:
##
## - initializes the cached matrix
##
## - sets the initial cached inverse to NULL
##
## - returns a special "matrix" object with functions for  
##   setting and retrieving the cached matrix and cached inverse matrix
##
##
## input  = the matrix to be inverted/cached
## 
## output = a 4 element list of functions:
##
## 1. set()    = a function that caches the matrix
## 2. get()    = a function that retrieves the cached matrix 
## 3. setinv() = a function that caches the matrix inverse 
## 4. getinv() = a function that retrieves the matrix inverse 
##
makeCacheMatrix <- function(x = matrix()) {
        
        ##
        ## initialize inverse matrix i to NULL
        ##
        i <- NULL
        
        ##
        ## set() is a function that initializes the cached 
        ## input matrix x and sets the cached inverse to NULL
        ##
        set <- function(y) {
                
                ##
                ## assign y to x
                ##
                x <<- y
                
                ##
                ## initialize the inverse i to NULL
                ##
                i <- NULL
        }
        
        ##
        ## get() is a function that returns cached matrix x
        ##
        get <- function() x
        
        ##
        ## setinv() is a function that sets the cached inverse i to
        ## the inverse of x
        ##
        setinv <- function(inv) i <<- inv
        
        ##
        ## getinv() is a function that returns the cached inverse
        ##
        getinv <- function() i
        
        ## 
        ## makeCacheMatrix returns the 4 element list of functions
        ##
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


##
## cacheSolve()
## ------------
##
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix().
##
## If the original matrix has not been altered and the inverse matrix has already been 
## cached, then cacheSolve() skips the inverse calculation and instead retrieves the 
## inverse from the cache.
##
## input  = a special matrix as outputted by makeCacheMatrix()
## 
## output = the inverse of the matrix that was the input to makeCacheMatrix()
##
cacheSolve <- function(x, ...) {
        
        ##
        ## set i to the retrieved cached inverse matrix
        ##
        i <- x$getinv()
        
        ##
        ## if i <> NULL...
        ##
        if(!is.null(i)) {
                
                ##
                ## ...the inverse already exists so retrieve it...
                ##
                message("getting cached inverse matrix")
                
                ##
                ## cacheSolve returns the cached i
                ##
                return(i)
        }
        
        ##
        ## otherwise...
        ##
        
        ##
        ## set data = the retrieved cached matrix x
        ##
        data <- x$get()
        
        ##
        ## set i = the inverse of data
        ##
        ## note: solve() calculates the inverse of a matrix
        ##
        i <- solve(data, ...)
        
        ##
        ## set the cached inverse to i
        ##
        x$setinv(i)
        
        ##
        ## cacheSolve returns the newly calculated inverse
        ## 
        i
}
