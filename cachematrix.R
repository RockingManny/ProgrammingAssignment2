# Coursera Programming Assignment 2
# R Programming - John Hopkins University

# Name - Manimit Haldar, A.K.A RockingManny
# Date - 25 OCT 2022

## ABSTRACT :---
# Matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than computing it repeatedly.
# This program takes an invertible matrix and computes the inverse of the input and stores the result in cache.  
# The matrix is input by calling the "cacheSolve()" function.  
# Subsequent calls will retrieve the inversed matrix from the cache
# rather than inversing the orginal input everytime.

## FUNCTION DESCRIPTION :---

# makeCacheMatrix() : It creates a matrix object needed to be inverted, which can be stored in cache.
makeCacheMatrix <- function(x = matrix()) {
        f <- NULL
        set <- function(y) {
                x <<- y
                f <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) f <<- solve
        getinverse <- function() f
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve() : It inverses the target matrix created by makeCacheMatrix. 
#                If the inverse has already been calculated (and the matrix has not changed), 
#                then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        f <- x$getinverse()
        if(!is.null(f)) {
                message("Getting the inversed matrix from the cache")
                return(f)
        }
        data <- x$get()
        f <- solve(data, ...)
        x$setinverse(f)
        f
}
