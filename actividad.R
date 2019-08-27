encriptar <- function(palabra, cpub){
  letras <- unlist(strsplit(palabra, ""))
  numeros <- sapply(unlist(strsplit(letras, "")), function(x) which(x == letters))
  encriptado <- sapply(numeros, function(x){
    x^cpub[2] %% cpub[1]
  })
  return(as.numeric(encriptado))
}


desencriptar <- function(palabra, cpub, cpriv){
  desencriptado <- sapply(palabra, function(x){
    x^cpriv %% cpub[1]
  })
  # return(sapply(desencriptado, function(x){
  #   which(x == letters)
  # }))
  paste0(letters[desencriptado], collapse = "")
}

# encriptar("erre", c(35, 5))
# desencriptar(encriptar("erre", c(35, 5)), c(35, 5), 5)
# 
# p <- 3
# q <- 5
# 
