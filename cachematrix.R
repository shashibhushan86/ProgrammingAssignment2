## This Function was developed in order to complete the Week 2 assignment of Coursera R Data science course.
## This Function creates a matrix, calculates the mean, return it and cache it.
## Usage: 
## a<-makeCacheMatrix()
## a$set(matrix(1:4,2,2));
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## This Function was developed in order to complete the Week 2 assignment of Coursera R Data science course.
## This Function determines whether the data is returned from cache hit or is calculated as a new value.
## Usage:
## cacheSolve(a)
## First time it will calculate the value, return the inverted matrix and store it.
## If we run the command cacheSolve(a) again on the console then the Inverted matrix will be returned from cache.
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}