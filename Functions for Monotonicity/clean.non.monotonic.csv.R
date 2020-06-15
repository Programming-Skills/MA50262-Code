clean.non.monotonic.csv <- function(file, header = TRUE){
  data <- read.csv(file = file, header = header)
  DT <- setDT(data)
  setnames(DT, old = c('x','y'), new = c('Time','Survival'))
  
  while (which(diff(DT$Survival)>0)) {
    DT <- DT[!which(diff(Survival)>0), ]
    S <- DT$Survival
    plot(S)
    points(which(diff(S)>0),S[which(diff(S)>0)],col="red",lwd=2)
    fwrite(DT, file = paste0("cleaned ", strsplit(file, "\\.", perl=TRUE)[[1]][1],".csv"), sep = " ")
  }
}