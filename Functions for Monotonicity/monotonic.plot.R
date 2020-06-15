monotonic.plot <- function(file, header = TRUE){
  data <- read.csv(file = file, header = header)
  S <- data$y
  plot(S)
  points(which(diff(S)>0),S[which(diff(S)>0)],col="red",lwd=2)
}