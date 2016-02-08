## Functionally, these pair of functions, makeCacheMatrix and cacheSolve, implement
## a working cache mechanism for the Inverse ( via solve) of a matrix to assist with 
## performance challenge. This is accomplished through the caching of the
## inverse of a matrix for subsequent use within the makeCacheMatrix.
##
## It should be noted that there are two fundamental triggering mechanisms in
## cache mechanism consisting of:
##       - Original copy of underlying matrix is saved during the 
##         set function/method which also marks the saved version of the 
##         inverse of the matrix as out of date through a null assignment
##       - The actual inverse matrix re-calculation or pull from the cache if
##         the inverse matrix is still up to date is accomplished through the 
##         cacheSolve function that decouples the calling function from the
##         mgmt process of leveraging/updating the cache
## Notes:
##      1- These functions leverage superparent lexical scoping to:
##          - retain the cache contents and inter function call
##          - also facilitate the ability to create multiple matrix caches
##      2- for those familiar with object oriented design
##          - the makeCacheMatrix function is really a contructor method for the
##            caching object
##          - the makeCacheMatrix also defines, effectively, additional methods
##            to set, get the underlying matrix and setinv, getinve for the
##            inverse matrix
##          - the cacheSolve is the principle function to either leverage the
##            cached inverse matrix or to recalc the inverse matrix when it is 
##            marked as out of date
## 

## Write a short comment describing this function
##
## The makeCacheMatrix effectively creates the Inverse Matrix cache Class. The
## functions/methods are as follows:
##
##      - set: updates the underlying matrix to the cache and marks the inverse
##             matrix as out of date.  Typically called from the main calling
##             program/functions.
##      - get: returns the latest version underlying matrix          
##      - setInv: used by cacheSolve to update and cache the Inverse matrix
##      - getInv: use by cacheSolve when pulling the Inverse matrix from the
##                cache and to determine ts in/out of date status

makeCacheMatrix <- function(x = matrix()) {
        #Constructor for Inverse Matrix Cache
        
        ## super-parent objects
        invMatrix <<- NULL
        dataMatrix <<- x
        
        ## Set ffunction/Method definition
        set <- function(y) {
                dataMatrix <<- y
                invMatrix <<- NULL     #empties inv cache and effectively marks
                                        # as out of date
        }
        ## Get function/method definition
        get <- function() dataMatrix
        
        ## SetInv function/method Definition
        setInv <- function(inv)   invMatrix <<- inv #store the calced inv to cache
        
        ## GetInv function/method Definition
        getInv <- function() invMatrix
        
        ## effectively return back a list of the functions/methods as part of cache
        list( set= set,get= get,
              setInv= setInv,
              getInv= getInv )
       
}


## The cacheSolve function, in effect a helper function, allows the caller to
## calculate the Inverse of the previous underling matrix that was either passed
## during makeCacheMatrix or through its set function.  If the underlying data has
## changed (not up to date)then a new Inverse will be calculated, saved in cache 
## and returned, otherwise it is assumed that the underlying matrix is up to 
## date and the cache Inverse matrix is returned. You need supply the cache 
## object when cacheSolve is called

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', 
        ## note, x is of class makeCacheMatrix
        
        inv <- x$getInv()
        if( !is.null(inv)) {      # check to see if invMarix is up to date (!NULL)
                message("Pulling invMatrix from cache")
                return(inv)
        }
        ## Otherwise, the cached InvMatrix is out of date (NULL), therefore:
        data <- x$get()         # pull latest underlying matrix from cache
        m <- solve(data, ...)   # recalc the Inv Matrix
        x$setInv(m)             # cache the recalc Inverse Matrix
        m                       # return the recalc Inverse Matrix
        
}
