

Upcandidates <- list()
Upcandidates_attribute <- list()
Downtarget_attributes <- data.frame()
thresholdForDif <- 1.5
Attribute_diff <- list()
Attribute_difftemp <- list()
Upcandidatesindex<- list()

Downtarget_attributes <- cbind(Downheader_new[,13:20],Downheader_new[,7],Downheader_new[,12]  )

for (i in 1: length(Upsiglist)) {  
  
  Upcandidatesindex[[i]] <- which(a_magdif[[i]] < thresholdForDif * min(a_magdif[[i]]) )
  Upcandidates[[i]] <- subset (Upsiglist[[i]], a_magdif[[i]] < thresholdForDif * min(a_magdif[[i]]) )

  Upcandidates_attribute[[i]] <- cbind(      
    Upheader_new[ match( as.numeric(Upcandidates[[i]]), as.numeric(Upheader_new[,13])),13:20] ,
    Upheader_new[ match( as.numeric(Upcandidates[[i]]), as.numeric(Upheader_new[,13])),7] ,
    Upheader_new[ match( as.numeric(Upcandidates[[i]]), as.numeric(Upheader_new[,13])),12])

    for (j in 1: length(Upcandidates[[i]])) {  
    Attribute_difftemp[[j]] <- abs( as.numeric( (unlist (Upcandidates_attribute[[i]][j,3:length(Upcandidates_attribute[[i]])]))) - 
      as.numeric(Downtarget_attributes[i,3:length(Upcandidates_attribute[[i]]) ])  )
    }
  
  Attribute_diff[[length( Attribute_diff)+1]] <- Attribute_difftemp # list in the list

}

#  utils::View( Upcandidates_attribute[[1]] )
View( Upcandidates_attribute[[i]][j,])
View( Downtarget_attributes[i,])

jointprobtemp <- data.frame()
jointprob <- list()
idxjointprob <- vector()
UpFinalcandidates <- vector()

for (i in 1:length(Upsiglist)){
  for (j in 1: length(Upcandidates[[i]])) {
    jointprobtemp[j,1] <-(approx(kernel.length_m$x, kernel.length_m$y, Attribute_diff[[i]][[j]][1]) )$y
    jointprobtemp[j,2] <-(approx(kernel.gvw_m$x, kernel.gvw_m$y, Attribute_diff[[i]][[j]][2]) )$y
    jointprobtemp[j,3] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, Attribute_diff[[i]][[j]][3]) )$y
    jointprobtemp[j,4] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, Attribute_diff[[i]][[j]][4]) )$y
    jointprobtemp[j,5] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, Attribute_diff[[i]][[j]][5]) )$y
    jointprobtemp[j,6] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, Attribute_diff[[i]][[j]][6]) )$y
    jointprobtemp[j,7] <-(approx(kernel.dur_m$x, kernel.dur_m$y, Attribute_diff[[i]][[j]][7]) )$y
    jointprobtemp[j,8] <-(approx(kernel.utc_m$x, kernel.utc_m$y, Attribute_diff[[i]][[j]][8]) )$y
    jointprobtemp[j,9] <- as.numeric (prob[[i]][[1]][Upcandidatesindex[[i]][j]] ) # CHECK!!!
    
    jointprobtemp [is.na(jointprobtemp )] <- 0.000000000001
    jointprobtemp[j,10] <- jointprobtemp[j,1]* jointprobtemp[j,2] * jointprobtemp[j,3] * jointprobtemp[j,4] *
       jointprobtemp[j,5] * jointprobtemp[j,6] * jointprobtemp[j,7] * jointprobtemp[j,8] * jointprobtemp[j,9] 

  }
  jointprob[[length(jointprob)+1]] <-  jointprobtemp
  jointprobtemp <- data.frame()
  idxjointprob[i] <- which.max(unlist (jointprob[[i]][10]) )
  
  UpFinalcandidates[i] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i]] ]
}

rm(test2)
test2 <- cbind(Target_baseanalysis_Jan0910_table, UpFinalcandidates)
