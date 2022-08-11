## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##the function stored in the cache

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y) {
          x<<-y
          m<<-Null
        }
        get<-function() x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list( set=set,get=get,
              setinverse=setinverse,
              getinverse=getinverse)
}


## Write a short comment describing this function
##cacheSolve takes the function makeCacheMatrix as input and checks

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
     m<-x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
       return(m)
     }
     data<-x$get()
     m<-solve(data,...)
     x$setinverse(m)
     m
}
