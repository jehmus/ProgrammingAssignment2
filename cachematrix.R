## These functions create an object that stores a matrix and can cache its inverse.


## This function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set<-function(y) {
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setInv<-function(inverse)  i<<-inverse
        getInv<-function() i
        list(set=set,
             get=get,
             setInv=setInv,
             getInv=getInv)
}


## This function computers the inverse of the matrix created by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then it should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data, ...)
        x$setInv(i)
        i
}
