# multiGrep: function to search for multiple strings (stringVector) in character vectors (x)
# result must have all strings in stringVector

# multiGrep2:function to search for multiple strings (stringVector) in character vectors (x)
# result need only have one of the strings in stringVector

multiGrep <- function(stringVector,x,...){
  for (string in stringVector){
    x <- grep(string,x,value = TRUE)
  }
  x
}

multiGrep2 <- function(stringVector,x,...){
  y <- character()
  for (string in stringVector){
    y <- c(y,grep(string,x,value = TRUE))
  }
  y <- unique(y)
  y
}