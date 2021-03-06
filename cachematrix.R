## These two functions create an object that is able to have a matrix and its inverse cached with 
## it so that the inverse can later be called without having to re calculate it.

## Calling the makeCacheMatrix function creates an instance that can later have matrix values saved 
## to it as well as creating an empty value that can have the inverse of the matrix saved to it.  

makeCacheMatrix <- function(mat=matrix(,r=nrow,c=ncol)){
        inv<-NULL
        set<-function(y){
                mat<<-y
                inv<<-NULL
        }
        get<-function() mat
        setin <- function(solve) inv<<- solve
        getin <- function () inv
        list(set=set, get=get, setin=setin, getin=getin)
}


## The cacheSolve function calls on the matrix created by the makeCacheMatrix, and checks if there
## it's inverse is saved. If it is, the function returns it, and if not, the function calculates the inverse
## and saves it with the original matrix. 

cacheSolve <- function(mat=matrix(),...){
        inv <mat$getin()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data.matrix<-mat$get()
        inv<-solve(data.matrix,...)
        mat$setin(inv)
        inv
}
