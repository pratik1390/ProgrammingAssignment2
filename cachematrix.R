## 
## 
## makeCacheMatrix is a function which returns a list containing four functions
## get() returns the matrix itself
## set(y) assignt value y to the parent environment'x and inverse to NULL
## setinverse assigns inv to parent environ's inverse
## getinverse basically returns the inverse value
makeCacheMatrix <- function(x = matrix()) 
{
        inverse<-NULL
        set<-function(y)
        {
                x<<-y
                inverse<<-NULL
                
        }
        get<-function() x
        setinverse<-function(inv) inverse<<-inv
        getinverse<-function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## In the above function, <- is used to assign in the current enviroment while <<-assigns it to the parent environment.
## cachesolve in a nutshell checks if there is a cache inverse stored beforehand. 
## If there is one, it directs return that value. 
## Otherwise it calculates the inverse, stores it and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getinverse()
        if(!is.null(inverse))
        {
                message("getting cached data")
                return(inverse)
        }
        data<-x$get()
        inverse<-solve(data, ...)
        x$setinverse(inverse)
        inverse
        
}
