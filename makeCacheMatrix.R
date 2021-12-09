## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function is the makeCacheMatrix
## The makeCacheMatrix function has four sub-functions (set, get, setinv, and getinv)
## library(MASS) is used to calculate inverse for squared matrices and non squared matrices 
library(MASS)

makeCacheMatrix <- function(x1 = matrix()) {
  inv<-NULL              # resetting inverse as NULL
  set<-function(y1){
    x1<<-y1
    inv<<-NULL
  }
  get<-function()x1       # function to get matrix x1
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x1)
    inver%*%x1           # function to obtain inverse of the matrix
  }
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)   
}

## Write a short comment describing this function

## The second function is the cacheSolve function

cacheSolve <- function(x1, ...)  ## gets cache data
{
  inv<-x1$getinv()
  if(!is.null(inv)){           #checking whether inverse is NULL
    message("getting cached data!")
    return (inv)      # return inverse value
  }
  data<-x1$get()
  inv<-solve(data, ...)        # calculating inverse value
  x1$setinv(inv)
  inv ## return a matrix that is the inverse of 'x1'
}

## after run the above functions in R console
## do the following example

h<-makeCacheMatrix (matrix(2:17, 4,4))
h$get()

h$getinv() 

## or
cacheSolve(h)




