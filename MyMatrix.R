makecachematrix<- function(mat=matrix(,r=nrow,c=ncol)){
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
##########
makecachematrix<-function(mat=matrix()){
        is.matrix(inverse)
        set<-function(y){
                mat<<-matrix(y)
                inverse<<-NULL                
        }
        get<-function() mat
        setin<-function(solve) inv<<-solve
        getin<-function()inverse
        list(set=set, get=get, setin=setin, getin=getin)
}

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(mat=matrix(), ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}



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





