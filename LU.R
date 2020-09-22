#followong function loads the matrix via keyboard
loadMatrix = function(){
  s = readline("Enter the size of the mxn matrix separated by coma (,):")
  sNum = strtoi(strsplit(s,",")[[1]]) #here we split and parse 
  #the input data to integer
  m = sNum[1]
  n = sNum[2]
  v = c()
  print("Enter the data by rows, separated by coma (,)")
  for (i in 1:m) {
    vS = readline(paste("Row ", i, ": ", sep = ""))
    v = c(v,strtoi(strsplit(vS, ",")[[1]])) #here we split the row data
    #and parse the input data to integer
  }
  #matrix creation by using the matrix data structure
  mat = matrix(v, nrow = m, byrow = T)
  return(mat)
}

#following function verifies if a vector is the zero vector
is_zero = function(c){
  for(num in c){
    if(num != 0){
      return(FALSE)
    }
  }
  return(TRUE)
}

#following function performs the type 1 elementary row operation
elementary_1 = function(m, i, j){
  aux = m[i,]
  m[i,] = m[j,]
  m[j,] = aux
  return(m)
}

#following function performs the type 2 elementary row operation
elementary_2 = function(m, i, a){
  m[i,] = a*m[i,] 
  return(m)
}

#following function performs the type 3 elementary row operation
elementary_3 = function(m, i, j, b){
  m[i,] = m[i,] + b*m[j,]
  return(m)
}
