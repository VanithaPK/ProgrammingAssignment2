##Caching the inverse of a matrix:
##Matrix inversion is usually a costly computation and there maybe some benefit to 
##cache the inverse of a matrix rather than compute it repeatedly
##There are pair of functions that are used to create a special objecr that stores a matrix and caches its inverse
##This functions creates a special "matrix" object than can cache its inverse
makeCacheMatrix <- function(x = matrix()){
        # holds the cached value or NULL if nothing is cached
        #initially nothing is cached so set it to NULL
        inv<-NULL
        #store a matrix
        set<-function(y) {
                x<<-y
                #since the matrix is assigned a new value,flush the cache
                inv<-NULL
                }
        #returns the stored matrix
        get<-function() x
        #cache the given arguement
        setInverse <- function(inverse) inv<<-inverse
        #get the cached value
        getInverse <- function() inv
        #return a list.Each named element of the list is a function
        list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
        }
##This function computes the inverse of the special "matrix" created by makecachematrix above.
##If the inverse has already been calculated,then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #get the cached value
        inv<-x$getInverse()
        #if the cached value exixts return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                }
        #otherwise get the matrix,calculate the inverse and store it in the cache
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        #return the inverse
        inv
}
