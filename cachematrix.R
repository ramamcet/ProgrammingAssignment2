## Put comments here that give an overall description of what your


## functions do
1.Set the matrix
2.Get the matrix
3.Set the inverse of the matrix
4.Get the inverse of the matrix

## Write a short comment describing this function
# Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {  
        
m <- NULL   # Initially set to NULL 
        set <- function(y) { # Sets the matrix itself but not the inverse
                x <<- y
                m <<- NULL
        }
        get <- function() x   # Gets the matrix itself but not the inverse 
        setInverse <- function(inverse) m <<- inverse  ## set the inverse of the matrix
        getInverse <- function() m  ## get the inverse of the matrix
        list(set = set,get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
##The following function cacheSolve calculates the inverse of the “matrix” created with the above function it first checks to see if the inverse
##of the matrix has already been calculated.If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
##the inverse of the data and sets the inverse matrix in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse() ##Get the current state of the inverse and see if it has been computed yet
        if (!is.null(m)) {
                message("getting cached data") ##simply return the computed inverse
                return(m)
        }
        dat <- x$get() ##If it hasn't...Get the matrix itself
        m <- solve(dat, ...) ##Find the inverse
        x$setInverse(m) #Cache this result in the object
        m    # Return this new result
}


 
## sample run

> Testmatrix <- makeCacheMatrix(matrix(2:5, 2, 2))
> Testmatrix$get()
     [,1] [,2]
[1,]    2    4
[2,]    3    5
> Testmatrix$getInverse()
NULL
> cacheSolve(Testmatrix)
     [,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1
> cacheSolve(Testmatrix)
getting cached data
     [,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1
> Testmatrix$getInverse()
     [,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1
> Testmatrix$set(matrix(c(2, 4, 6, 8), 2, 2))
> Testmatrix$get()
     [,1] [,2]
[1,]    2    6
[2,]    4    8
> Testmatrix$getInverse()
NULL
> cacheSolve(Testmatrix)
     [,1]  [,2]
[1,] -1.0  0.75
[2,]  0.5 -0.25
> cacheSolve(Testmatrix)
getting cached data
     [,1]  [,2]
[1,] -1.0  0.75
[2,]  0.5 -0.25
> Testmatrix$getInverse()
     [,1]  [,2]
[1,] -1.0  0.75
[2,]  0.5 -0.25

