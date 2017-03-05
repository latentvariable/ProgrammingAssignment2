#R Programing Assignment - Week 3
#Caching inverse of a Matrix
#Function 1: Make Cache Matrix
#Specifies an empty matrix 'm' as argument
makeCacheMatrix <- function(m = matrix()){
  inv <- NULL #setting the inverse of the matrix to be empty
  #Using the set function below to set the value of the matrix
  set <- function(n){
    m <<- n
    inv <<- NULL
  }
  
  get <- function() m #Retrieves the input matrix
  setinv <- function(inverse) inv <<- inverse #Defines the setter for inverse
  getinv <- function() inv #Defines the getter for inverse to retrieve value of inverse
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#Function 2: Function to solve for the inverse of given matrix

#This function starts with the matrix 'm' as argument
cacheSolve <- function (m,...){
  inv <- m$getinv()    #Calling function getinv to retrieve inverse
  #To check if the 'inv' variable above is empty, it runs the IF loop below. If it is empty, it returns NULL.
  #If not, it displays the inverse stored in inv and the message "Getting Cached Data"
  if(!is.null(inv)){
    message("Getting Cached Data")
    return(inv)
  }    
  
  #Calculation of the inverse
  data <- m$get() #Assigns the input matrix to variable 'data'
  inv <- solve(data, ...) #Uses the solve function to get the inverse of 'data'
  m$setinv(inv) #Uses the setter function 'setinv' to set the value of the matrix inverse to 'inv'
  inv #Prints the calculated inverse to screen
}