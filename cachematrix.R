## Put comments here that give an overall description of what your
## functions do

##The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve(). 
## makeCacheMatrix() stores a matrix and its inverse. 
## cachemean() requires an argument that is returned by makeCacheMatrix() in order to
##retrieve the inverse from the cached value that is stored in the makecacheMatrix() object's environment.

## Write a short comment describing this function
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix function first initialize two objects ,x which is a matrix and inv (which is the inverse of matrix)
## Then makeCacheMatrix defines the set(),get() then defines the set_inv() and get_inv () for the inv
## lastly it  assigns each of these functions as an element within a list() 

makecacheMatrix <- function(x=matrix()){
  inv<-NULL
  
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  get<-function()x
  set_inv<-function(inverse) inv<-inverse
  get_inv<-function()inv
  list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
  
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
##retrieve the inverse from the cache.
##first cacheSolve function accepts an argument x and ellipsis
## Then, it calls the get_inv function on the input object
## check if the inv value is NULL.if not return the inv value
##else,it gets the input object,find its inverse using solve function 
## and set the inverse using the set_inv() on the input object
## and returns the value of inverse

cacheSolve<-function(x,...){
  inv<-x$get_inv()
    if(!is.null(inv)){
      return (inv)
      
    }
    data<-x$get()
    inv<-solve(data,...)
    x$set_inv(inv)
    inv
        
  
}


  ##solution test 1 (2*2 matrix)
> matrix1<-matrix(1:4,2,2)
> cacheSolve(makecacheMatrix(matrix1))
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

   ## test2(3*3 matrix)
## not every square matrix is invertible
##A square matrix that has an inverse is called invertible or non-singular.
## A matrix that does not have an inverse is called singular.determinant should not be 0

> matrix1<-matrix(1:9,3,3)
> cacheSolve(makecacheMatrix(matrix1))
 Show Traceback
 
 Rerun with Debug
 Error in solve.default(data, ...) : 
  Lapack routine dgesv: system is exactly singular: U[3,3] = 0 

  ##test3 (3*3 matrix)
> matrix1<-matrix(c(1,2,3,0,1,4,5,6,0),3,3)
> cacheSolve(makecacheMatrix(matrix1))
     [,1] [,2] [,3]
[1,]  -24   20   -5
[2,]   18  -15    4
[3,]    5   -4    1