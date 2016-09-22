##These are a pair of functions that cache the inverse of a matrix.
## Function below creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { ##Defines argument of the function as a form of matrix
        i<-NULL##i is used as inverse of matrix. It is initialized as NULL
        set<-function(y){##function used to assign new values on execution
                x<<-y##value of matrix in parent env.
                i<<-NULL##sets value of inv to null again
        }
        get<-function(){x}##returns matrix value
        setInv<-function(solve){i<<-solve}##sets inverse value
        getInv<-function(){i}##returns inverse value of matrix
        list(set=set,get=get,setInv=setInv,getInv=getInv)##used to access the functions defined herein
}
##Function below solves the value for inverse of "matrix" object
##Function will retrieve the inverse from the cache(if already calculated)
cacheSolve <- function(x, ...) {
        i<-x$getInv()##accesses function for obtaining inverse of matrix
        if(!is.null(i)){##checks if inverse has already been calculated
                message("Getting cached data..")
                return(i)##returns chached value
        }
        data<-x$get()
        i<-solve(data,...)
        x$setInv(i)
        i
}
