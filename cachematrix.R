## So there's two functions for calculating inversed matrix to a some
## original matrix. Since this calculations are quite costly, we'll need to
## store once computed inverse matrix in cache.
## Since there's no objects in R in a sense of such languages as C++,
## we can define a similar entity — a list of functions for manipulations of
## variables defined in an environment of some function.

## So we need two separate functions. One of them creates pseudo-object,
## containing original and cached inversed matrix in an environment of this
## function, and also sub-functions for reading and setting these variables.
## It returns the list of these sub-functions along with the environment 
## where associated variables are stored.
## And the second function works with the pseudo-object and actually computes 
## the inversed matrix. But if these computations has been already done,
## the function returns the computed matrix from cache.

## This function effectively creates an object (similar to objects in 
## object-oriented programming languages)
## The object consists of two variables: 'x' for storing the original matrix 
## and 'cch' for storing inversed matrix (or NULL value if such matrix 
## hasn't been computed yet).
## Variables should not be manipulated directly, so there's four nested 
## functions for reading and setting them. The function returns the list 
## of these four nested functions, so the list acts as an objectm

makeCacheMatrix <- function(x = matrix()) {
    cch <- NULL ## cache is NULL by default
    set <- function(y){ ## this nested function stores the matrix 
                        ## in an internal 'x' variable
        x <<- y
        cch <<- NULL    ## if we change the matrix, we must clear the cache, 
                        ## since it hasn't been calculated yet
    }
    get <- function() x ## returns the stored matrix associated with the Object
    setInversed <- function(mtrInv) cch <<- mtrInv  ## stores the inversed matrix 
                                                    ## in the cache
    getInversed <- function() cch ## returns the inversed matrix from the cache
    list(set = set, get = get, setInv = setInversed, getInv = getInversed)  
        ## returns the list of four functions
}


## This function receives a list of four functions (one of them returns 
## the associated matrix) and returns the inversed matrix. 
## If the inversed matrix has been already calculated, then the cached value 
## is returned. Otherwise function calculates the inversed matrix 
## and stores it in the cache.

cacheSolve <- function(x, ...) {
    mtrInv <- x$getInv()    ## trying to get cached inversed matrix
    if(!is.null(mtrInv)) ## if the inversed matrix is already calculated
        return(mtrInv) ## then the cached value is returned
    ## otherwise we calculate it
    mtr <- x$get() ## getting the associated matrix from received vector
    mtrInv <- solve(mtr, ...) ## getting the inversed matrix
    x$setInv(mtrInv)    ## storing in the cache
    mtrInv ## returning the inversed matrix
}
