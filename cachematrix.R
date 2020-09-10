##This will make the required functions for the matrix whose inverse we want.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get<- function()x
  setvalue<- function(inverse)m<<- inverse
  getvalue<-function()m
  list(get=get, set=set,getvalue=getvalue,setvalue=setvalue)
}                                                                                                    


##This will calculate the inverse of matrix

cacheSolve <- function(x, ...) {
      m<- x$getvalue()
      if(!is.null(m)){
        message("getting cached data now")
        return(m)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
      }
      data<- x$get()
      m<- solve(data,...)
      x$setvalue(m)
      m
}
