# Funcion para obtener el R0
R0 <- function(infect, dia_n) {
  val <- numeric()
  for (i in dia_n:length(infect)) {
    val[i] <- infect[i]/infect[i - dia_n + 1]
  }
  return(val)
}