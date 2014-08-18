## Overall description of the two functions that are defined below:

## We want to compute the inverse of a matrix. Since this operation is 
## not so efficient when the dimension of the matrix gets bigger, instead 
## of computing the inverse of a matrix every time we need it, sometimes it
## is useful to cache the inverse. This is what the following functions do.

## Description of the first function:

## The first function, makeCacheMatrix, takes a matrix as its argument 
## (we assume that the matrix is always invertible, so we don't specify
## anything about the cases when the matrix is not invertible), and returns
## a list containing four functions. The first function in this list, denoted
## by set, sets the value of the matrix and initializes its inverse (that we 
## want to compute) with NULL.The second function, get, gets the value of 
## the matrix. The third one, named setinv, assigns (sets) the value of the 
## inverse matrix. Finally, the last function, getinv, gets the value of this 
## inverse. All these functions, except the set function, will be later used in 
## the definition of the second function, cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Description of the second function:

## The second function, cacheSolve, takes as an argument the list 
## returned by the makeCacheMatrix function, and it returns the inverse 
## of the initial matrix (the one that appears in the argument of makeCacheMatrix
## function). It is important to notice here that whereas the first function, 
## makeCacheMatrix, takes a "normal" matrix as its argument, the cacheSolve
## function has a list as argument. 
## The function cacheSolve first gets the inverse from the list x (and stores
## its value in the inv variable). Then it checks to see if the value of
## inv is a matrix or NULL. If inv is a matrix, this means that the inverse
## matrix is cached, so the function prints a message and returns the value
## of the inverse. In this case, the return() function ends the execution of 
## the cacheSolve function, so the remaining lines are skipped. On the other
## hand, if inv is NULL, it means that the inverse matrix hadn't been computed
## before, so cacheSolve has to compute it. It first gets the value of the matrix
## from the list and it stores it in a variable named data. Then it computes 
## the inverse of data using the function solve() and it assigns this value to
## the variable inv. This value is set in the cache using the setinv function, 
## and then it is returned.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
