## This code is written in view of completing 
## a Peer Assignment for R Programming course 
## conducted by Roger D. Peng of Johns Hopkins University
## Version: 1.0
## Author : afiq
## Date  : 28/04/2014
## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. 
## The purpose of this program is to write a pair of functions that 
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## We assume that the matrix passed to the function is ALWAYS invertible
makeCacheMatrix <- function(x = matrix()) {
  # declare the inverse
  xInv <- NULL
  
  # return original matrix
  get <- function() x
  # override current matrix
  set <- function(y) {
    x <<- y
    xInv <<- calInv()
  }
  # calculate matrix inverse using solve()
  # solve() is in the base package
    calInv <- function(){
    xInv <<- solve(x)
  }
  # return the matrix inverse
  getInv <- function() {
    if(is.null(xInv)){
      calInv()
    }
    xInv
  }
  
  # set inverse to another matrix
  setInv <- function(nInv = matrix()) xInv <<- nInv
  
  list(set = set, get = get,
       calInv = calInv,
       getInv = getInv,
       setInv = setInv)
}

## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x) {
    xInv <- x$getInv()
    if(!is.null(xInv)){
      message("getting cached data")
      return(xInv)
    }
    mat <- x$get()
    xInv <- solve(x)
    x$setInv(xInv)
    xInv
}

## TEST
## Create two matrices
# c1 = rbind(c(1, -1/4), c(-1/4, 1))
# c2 = rbind(c(1, 2), c(2, 1))
# myMatrix <- makeCacheMatrix(c1)
## Check the return of getInv()
# identical(myMatrix$getInv(), solve(c1))
## Override matrix
# myMatrix$set(c2)
## Check the return of getInv()
## identical(myMatrix$getInv(), solve(c2))
# cacheSolve(myMatrix)