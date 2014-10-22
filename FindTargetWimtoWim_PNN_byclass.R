library(pnn)

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_basemagdif.RData ")


sigma <- 1
numoffeatures <- 100


for (i in 1: length(candidate) ){
  seqcandidate <- seq( from = 1, to = length(candidate[[i]]) )
  if ( ( length(candidate[[i]][1]) == 0)) {
    cat[[i]] <- NA 
    prob[[i]] <- NA }
  
  else{
  output <- matrix(unlist( candidate[[i]]), ncol = length(candidate[[i]]))
  output2 <-t( output[seq(1, length(output[,1]), 
                        by = round (1000 / numoffeatures)),] )
  pnn_md <- data.frame(seqcandidate, output2)
#   pnn_md <- cbind( seqcandidate, output2 )
  nn <- learn( pnn_md, category.column=1)
  nn <- smooth(nn, sigma)
  
  Down <- Downobjout[i,]
  Down <- Down[seq(1, length(Down), by = round (1000 / numoffeatures) )]
  candi_guess <- list(guess(nn, Down))  
  cat[[i]]<- candi_guess[[1]][1]
  prob[[i]]<- candi_guess[[1]][2]
  }
}

