## Make cache matrix makes a list of functions that 
## set the matrix, get the value of the matrix, 
## set the inverse and get the inverse


makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	 set<-function (y){
		 x<<-y
		 m<<-NULL
	 }
	get<-function() x
 	setinverse<-function(inverse) m<<-inverse
	getinverse<-function() m
	list(set=set,
	get=get,
	setinverse=setinverse,
	getinverse=getinverse)
}


## ## this functions returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	m<-x$getinverse() ## gets the inverse from x and then checks if the inverse is already calculated or not
	if (!is.null(m)){  
		message("getting data from Cache")
		return(m) ## the inverse that existed is returned
	}
	data<-x$get()
	m<-solve(data,...) ## if the inverse didn't exist it'll be calculated here
	x$setinverse(m)
	return(m)          ## the value is returned
	
}
        
