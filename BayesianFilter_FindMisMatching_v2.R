rm(list=ls())
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/20141125Jan0910.RData") 
## kernel estimation 
rm(sub_matching, sub_nonmatching, sub_all)


### kernal estimation based on NN

sub_all <- TargetTable_NN[[5]][,1:4]
sub_all <- cbind(sub_all,TargetTable_NN[[5]][,6],TargetTable_NN[[5]][,8])
sub_all <- cbind( sub_all ,      
                  Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),13:20] ,
                  Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),7] ,
                  Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),12] ,
                  Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),13:20] , # should be 6?
                  Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),7] ,
                  Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),12] ) 
sub_all <- na.omit(sub_all)

# install.packages("stringr")
library(stringr)


# train (01/09)
DownheaderTrainIdx <- which (Downheader_new[,12] < 1357804800000)
DownheaderTestIdx <- which (Downheader_new[,12] > 1357804800000)
Upsiglist_train <- Upsiglist[DownheaderTrainIdx]
Upsiglist_test <- Upsiglist[DownheaderTestIdx]


sub_all_train <- subset(sub_all, as.numeric (str_sub (sub_all [,4],-13,-1) ) < 1357804800000    ) 
sub_matching_train <- subset(sub_all_train  , as.numeric (sub_all_train  [,5]) == as.numeric(sub_all_train  [,6]) &
                         as.numeric (sub_all_train [,5]) != 999)
sub_nonmatching_train <- subset(sub_all_train , as.numeric (sub_all_train [,5]) != as.numeric( sub_all_train [,6]) )
sub_matching_train <- cbind( sub_matching_train ,      
                       na.omit ( Downheader_new[ match( sub_matching_train [,4], as.numeric(Downheader_new[,13])),13:20] ),
                       na.omit ( Downheader_new[ match( sub_matching_train [,4], as.numeric(Downheader_new[,13])),7] ),
                       na.omit ( Downheader_new[ match( sub_matching_train [,4], as.numeric(Downheader_new[,13])),12] ),
                       na.omit ( Upheader_new[ match( sub_matching_train [,6], as.numeric(Upheader_new[,13])),13:20] ),
                       na.omit ( Upheader_new[ match( sub_matching_train [,6], as.numeric(Upheader_new[,13])),7] ),
                       na.omit ( Upheader_new[ match( sub_matching_train [,6], as.numeric(Upheader_new[,13])),12] ) )

sub_nonmatching_train  <- cbind( sub_nonmatching_train ,
                          Downheader_new[ match( sub_nonmatching_train [,4], as.numeric(Downheader_new[,13])),13:20],
                          Downheader_new[ match( sub_nonmatching_train [,4], as.numeric(Downheader_new[,13])),7],
                          Downheader_new[ match( sub_nonmatching_train [,4], as.numeric(Downheader_new[,13])),12],
                          Upheader_new[ match( sub_nonmatching_train [,6], as.numeric(Upheader_new[,13])),13:20],
                          Upheader_new[ match( sub_nonmatching_train [,6], as.numeric(Upheader_new[,13])),7],
                          Upheader_new[ match( sub_nonmatching_train [,6], as.numeric(Upheader_new[,13])),12])

colnames(sub_matching_train )[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_matching_train )[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_nonmatching_train )[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_nonmatching_train )[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_all_train )[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_all_train )[19:26 ] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")


# test
sub_all_test  <- subset(sub_all, as.numeric (str_sub (sub_all [,4],-13,-1) ) >= 1357804800000    ) 
sub_matching_test <- subset(sub_all_test  , as.numeric (sub_all_test  [,5]) == as.numeric(sub_all_test  [,6]) &
                              as.numeric (sub_all_test [,5]) != 999)
sub_nonmatching_test <- subset(sub_all_test , as.numeric (sub_all_test [,5]) != as.numeric( sub_all_test [,6]) )
sub_matching_test <- cbind( sub_matching_test ,      
                            na.omit ( Downheader_new[ match( sub_matching_test [,4], as.numeric(Downheader_new[,13])),13:20] ),
                            na.omit ( Downheader_new[ match( sub_matching_test [,4], as.numeric(Downheader_new[,13])),7] ),
                            na.omit ( Downheader_new[ match( sub_matching_test [,4], as.numeric(Downheader_new[,13])),12] ),
                            na.omit ( Upheader_new[ match( sub_matching_test [,6], as.numeric(Upheader_new[,13])),13:20] ),
                            na.omit ( Upheader_new[ match( sub_matching_test [,6], as.numeric(Upheader_new[,13])),7] ),
                            na.omit ( Upheader_new[ match( sub_matching_test [,6], as.numeric(Upheader_new[,13])),12] ) )

sub_nonmatching_test  <- cbind( sub_nonmatching_test ,
                                Downheader_new[ match( sub_nonmatching_test [,4], as.numeric(Downheader_new[,13])),13:20],
                                Downheader_new[ match( sub_nonmatching_test [,4], as.numeric(Downheader_new[,13])),7],
                                Downheader_new[ match( sub_nonmatching_test [,4], as.numeric(Downheader_new[,13])),12],
                                Upheader_new[ match( sub_nonmatching_test [,6], as.numeric(Upheader_new[,13])),13:20],
                                Upheader_new[ match( sub_nonmatching_test [,6], as.numeric(Upheader_new[,13])),7],
                                Upheader_new[ match( sub_nonmatching_test [,6], as.numeric(Upheader_new[,13])),12])

colnames(sub_matching_test )[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_matching_test )[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_nonmatching_test )[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_nonmatching_test )[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_all_test)[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_all_test)[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")


# normalized factor (train set)
maxlength_train <- max(abs( na.omit(sub_all_train[,9] - sub_all_train[,19]) ))
maxgvw_train    <- max(abs( na.omit(sub_all_train[,10] -sub_all_train[,20]) ))
maxax12sp_train <- max(abs( na.omit(sub_all_train[,11] - sub_all_train[,21]) ))
maxax23sp_train <- max(abs( na.omit(sub_all_train[,12] - sub_all_train[,22]) ))
maxax34sp_train <- max(abs( na.omit(sub_all_train[,13] - sub_all_train[,23]) ))
maxax45sp_train <- max(abs( na.omit(sub_all_train[,14] - sub_all_train[,24]) ))
maxdur_train <- max(abs( na.omit(sub_all_train[,15] - sub_all_train[,25]) ))
maxutc_train <- max(abs( na.omit(sub_all_train[,16] - sub_all_train[,26]) ))
maxmag_train <- max( na.omit(sub_all_train[,2]) )

# normalized factor (test set)
maxlength_test <- max(abs( na.omit(sub_all_test[,9] - sub_all_test[,19]) ))
maxgvw_test    <- max(abs( na.omit(sub_all_test[,10] - sub_all_test[,20]) ))
maxax12sp_test <- max(abs( na.omit(sub_all_test[,11] - sub_all_test[,21]) ))
maxax23sp_test <- max(abs( na.omit(sub_all_test[,12] - sub_all_test[,22]) ))
maxax34sp_test <- max(abs( na.omit(sub_all_test[,13] - sub_all_test[,23]) ))
maxax45sp_test <- max(abs( na.omit(sub_all_test[,14] - sub_all_test[,24]) ))
maxdur_test <- max(abs( na.omit(sub_all_test[,15] - sub_all_test[,25]) ))
maxutc_test <- max(abs( na.omit(sub_all_test[,16] - sub_all_test[,26]) ))
maxmag_test <- max( na.omit(sub_all_test[,2]) )

# normalized Difference (train)
Diff.lengthdiff_m_train <- na.omit ( abs (sub_matching_train [,9] - sub_matching_train[,19]) )
Diff.lengthdiff_m_train <- Diff.lengthdiff_m_train / maxlength_train
# Diff.lengthdiff_m <- (Diff.lengthdiff_m - min(Diff.lengthdiff_m)) / 
#   (max (Diff.lengthdiff_m) - min(Diff.lengthdiff_m))
Diff.gvwdiff_m_train    <- na.omit( abs (sub_matching_train [,10] - sub_matching_train[,20]) )
Diff.gvwdiff_m_train   <- Diff.gvwdiff_m_train   / maxgvw_train
# Diff.gvwdiff_m  <- (Diff.gvwdiff_m   - min(Diff.gvwdiff_m  ) )/
#   (max(Diff.gvwdiff_m  ) - min(Diff.gvwdiff_m  ))
Diff.ax12spdiff_m_train <- na.omit( abs (sub_matching_train [,11] - sub_matching_train[,21]) )
Diff.ax12spdiff_m_train <- Diff.ax12spdiff_m_train / maxax12sp_train
# Diff.ax12spdiff_m <- (Diff.ax12spdiff_m - min (Diff.ax12spdiff_m)) /
#   (max(Diff.ax12spdiff_m) - min(Diff.ax12spdiff_m ))
Diff.ax23spdiff_m_train <- na.omit( abs (sub_matching_train [,12] - sub_matching_train[,22]) )
Diff.ax23spdiff_m_train <- Diff.ax23spdiff_m_train  / maxax23sp_train
# Diff.ax23spdiff_m  <- (Diff.ax23spdiff_m  - min(Diff.ax23spdiff_m )) / 
#   (max(Diff.ax23spdiff_m ) -min (Diff.ax23spdiff_m ))
Diff.ax34spdiff_m_train <- na.omit( abs (sub_matching_train [,13] - sub_matching_train[,23]) )
Diff.ax34spdiff_m_train <- Diff.ax34spdiff_m_train  / maxax34sp_train
# Diff.ax34spdiff_m  <- (Diff.ax34spdiff_m  - min(Diff.ax34spdiff_m )) / 
#   (max(Diff.ax34spdiff_m ) -min (Diff.ax34spdiff_m ))
Diff.ax45spdiff_m_train <- na.omit( abs (sub_matching_train [,14] - sub_matching_train[,24]) )
Diff.ax45spdiff_m_train <- Diff.ax45spdiff_m_train  / maxax45sp_train
# Diff.ax45spdiff_m  <- (Diff.ax45spdiff_m  - min(Diff.ax45spdiff_m )) / 
#   (max(Diff.ax45spdiff_m ) -min (Diff.ax45spdiff_m ))
Diff.durdiff_m_train    <- na.omit( abs (sub_matching_train [,15] - sub_matching_train[,25]) )
Diff.durdiff_m_train <- Diff.durdiff_m_train  / maxdur_train
# Diff.ax45spdiff_m  <- (Diff.durdiff_m  - min(Diff.durdiff_m  )) / 
#   (max(Diff.durdiff_m  ) -min (Diff.durdiff_m  ))
Diff.utcdiff_m_train    <- na.omit( abs (sub_matching_train [,16] - sub_matching_train[,26]) )
Diff.utcdiff_m_train    <- Diff.utcdiff_m_train / maxutc_train
# Diff.utcdiff_m  <- (Diff.utcdiff_m - min(Diff.utcdiff_m )) / 
#   (max(Diff.utcdiff_m) -min (Diff.utcdiff_m))
Diff.magdiff_m_train    <- na.omit( abs (sub_matching_train [,2]  ))
Diff.magdiff_m_train    <- Diff.magdiff_m_train / maxmag_train
# Diff.magdiff_m  <- (Diff.magdiff_m - min(Diff.magdiff_m )) / 
#   (max(Diff.magdiff_m ) -min (Diff.magdiff_m ))

Diff.lengthdiff_nm_train <- na.omit ( abs (sub_nonmatching_train [,9] - sub_nonmatching_train[,19]) )
Diff.lengthdiff_nm_train <- Diff.lengthdiff_nm_train / maxlength_train
# Diff.lengthdiff_nm <- (Diff.lengthdiff_nm - min(Diff.lengthdiff_nm ))/
#   (max(Diff.lengthdiff_nm ) - min (Diff.lengthdiff_nm ))
Diff.gvwdiff_nm_train    <- na.omit( abs (sub_nonmatching_train [,10] - sub_nonmatching_train[,20]) )
Diff.gvwdiff_nm_train    <- Diff.gvwdiff_nm_train   / maxgvw_train
# Diff.gvwdiff_nm  <- (Diff.gvwdiff_nm  - min(Diff.gvwdiff_nm  ))/
#   (max(Diff.gvwdiff_nm  ) - min (Diff.gvwdiff_nm  ))
Diff.ax12spdiff_nm_train <- na.omit( abs (sub_nonmatching_train [,11] - sub_nonmatching_train[,21]) )
Diff.ax12spdiff_nm_train <- Diff.ax12spdiff_nm_train / maxax12sp_train
# Diff.ax12spdiff_nm <- (Diff.ax12spdiff_nm  - min(Diff.ax12spdiff_nm  ))/
#   (max(Diff.ax12spdiff_nm  ) - min (Diff.ax12spdiff_nm ))
Diff.ax23spdiff_nm_train <- na.omit( abs (sub_nonmatching_train [,12] - sub_nonmatching_train[,22]) )
Diff.ax23spdiff_nm_train <- Diff.ax23spdiff_nm_train / maxax23sp_train
# Diff.ax23spdiff_nm <- (Diff.ax23spdiff_nm  - min(Diff.ax23spdiff_nm  ))/
#   (max(Diff.ax23spdiff_nm  ) - min (Diff.ax23spdiff_nm ))
Diff.ax34spdiff_nm_train <- na.omit( abs (sub_nonmatching_train [,13] - sub_nonmatching_train[,23]) )
Diff.ax34spdiff_nm_train <- Diff.ax34spdiff_nm_train / maxax34sp_train
# Diff.ax34spdiff_nm <- (Diff.ax34spdiff_nm  - min(Diff.ax34spdiff_nm  ))/
#   (max(Diff.ax34spdiff_nm  ) - min (Diff.ax34spdiff_nm ))
Diff.ax45spdiff_nm_train <- na.omit( abs (sub_nonmatching_train [,14] - sub_nonmatching_train[,24]) )
Diff.ax45spdiff_nm_train <- Diff.ax45spdiff_nm_train / maxax45sp_train
# Diff.ax45spdiff_nm <- (Diff.ax45spdiff_nm  - min(Diff.ax45spdiff_nm  ))/
#   (max(Diff.ax45spdiff_nm  ) - min (Diff.ax45spdiff_nm ))
Diff.durdiff_nm_train    <- na.omit( abs (sub_nonmatching_train [,15] - sub_nonmatching_train[,25]) )
Diff.durdiff_nm_train <- Diff.durdiff_nm_train / maxdur_train
# Diff.durdiff_nm  <- (Diff.durdiff_nm   - min(Diff.durdiff_nm   ))/
#   (max(Diff.durdiff_nm   ) - min (Diff.durdiff_nm ))
Diff.utcdiff_nm_train    <- na.omit( abs (sub_nonmatching_train [,16] - sub_nonmatching_train[,26]) )
Diff.utcdiff_nm_train <- Diff.utcdiff_nm_train / maxutc_train
# Diff.utcdiff_nm  <- (Diff.utcdiff_nm  - min(Diff.utcdiff_nm   ))/
#   (max(Diff.utcdiff_nm   ) - min (Diff.utcdiff_nm  ))
Diff.magdiff_nm_train   <- na.omit( abs (sub_nonmatching_train [,2]  ))
Diff.magdiff_nm_train   <- Diff.magdiff_nm_train / maxmag_train
# Diff.magdiff_nm  <- (Diff.magdiff_nm   - min(Diff.magdiff_nm  ))/
#   (max(Diff.magdiff_nm  ) - min (Diff.magdiff_nm  ))



# normalized Difference (test)
Diff.lengthdiff_m_test <- na.omit ( abs (sub_matching_test [,9] - sub_matching_test[,19]) )
Diff.lengthdiff_m_test <- Diff.lengthdiff_m_test / maxlength_test

Diff.gvwdiff_m_test    <- na.omit( abs (sub_matching_test [,10] - sub_matching_test[,20]) )
Diff.gvwdiff_m_test   <- Diff.gvwdiff_m_test   / maxgvw_test

Diff.ax12spdiff_m_test <- na.omit( abs (sub_matching_test [,11] - sub_matching_test[,21]) )
Diff.ax12spdiff_m_test <- Diff.ax12spdiff_m_test / maxax12sp_test

Diff.ax23spdiff_m_test <- na.omit( abs (sub_matching_test [,12] - sub_matching_test[,22]) )
Diff.ax23spdiff_m_test <- Diff.ax23spdiff_m_test  / maxax23sp_test

Diff.ax34spdiff_m_test <- na.omit( abs (sub_matching_test [,13] - sub_matching_test[,23]) )
Diff.ax34spdiff_m_test <- Diff.ax34spdiff_m_test  / maxax34sp_test

Diff.ax45spdiff_m_test <- na.omit( abs (sub_matching_test [,14] - sub_matching_test[,24]) )
Diff.ax45spdiff_m_test <- Diff.ax45spdiff_m_test  / maxax45sp_test

Diff.durdiff_m_test    <- na.omit( abs (sub_matching_test [,15] - sub_matching_test[,25]) )
Diff.durdiff_m_test <- Diff.durdiff_m_test  / maxdur_test

Diff.utcdiff_m_test    <- na.omit( abs (sub_matching_test [,16] - sub_matching_test[,26]) )
Diff.utcdiff_m_test    <- Diff.utcdiff_m_test / maxutc_test

Diff.magdiff_m_test    <- na.omit( abs (sub_matching_test [,2]  ))
Diff.magdiff_m_test    <- Diff.magdiff_m_test / maxmag_test


Diff.lengthdiff_nm_test <- na.omit ( abs (sub_nonmatching_test [,9] - sub_nonmatching_test[,19]) )
Diff.lengthdiff_nm_test <- Diff.lengthdiff_nm_test / maxlength_test

Diff.gvwdiff_nm_test    <- na.omit( abs (sub_nonmatching_test [,10] - sub_nonmatching_test[,20]) )
Diff.gvwdiff_nm_test    <- Diff.gvwdiff_nm_test   / maxgvw_test

Diff.ax12spdiff_nm_test <- na.omit( abs (sub_nonmatching_test [,11] - sub_nonmatching_test[,21]) )
Diff.ax12spdiff_nm_test <- Diff.ax12spdiff_nm_test / maxax12sp_test

Diff.ax23spdiff_nm_test <- na.omit( abs (sub_nonmatching_test [,12] - sub_nonmatching_test[,22]) )
Diff.ax23spdiff_nm_test <- Diff.ax23spdiff_nm_test / maxax23sp_test

Diff.ax34spdiff_nm_test <- na.omit( abs (sub_nonmatching_test [,13] - sub_nonmatching_test[,23]) )
Diff.ax34spdiff_nm_test <- Diff.ax34spdiff_nm_test / maxax34sp_test

Diff.ax45spdiff_nm_test <- na.omit( abs (sub_nonmatching_test [,14] - sub_nonmatching_test[,24]) )
Diff.ax45spdiff_nm_test <- Diff.ax45spdiff_nm_test / maxax45sp_test

Diff.durdiff_nm_test    <- na.omit( abs (sub_nonmatching_test [,15] - sub_nonmatching_test[,25]) )
Diff.durdiff_nm_test <- Diff.durdiff_nm_test / maxdur_test

Diff.utcdiff_nm_test    <- na.omit( abs (sub_nonmatching_test [,16] - sub_nonmatching_test[,26]) )
Diff.utcdiff_nm_test <- Diff.utcdiff_nm_test / maxutc_test

Diff.magdiff_nm_test   <- na.omit( abs (sub_nonmatching_test [,2]  ))
Diff.magdiff_nm_test   <- Diff.magdiff_nm_test / maxmag_test


# norm.length <- density ( rnorm(n=length(Diff.lengthdiff_m),m=mean(Diff.lengthdiff_m),sd=sd(Diff.lengthdiff_m)) )
# approx(norm.length$x, norm.length$y, c(5, 0, 1, 2)) # plug in new data

kernel.length_m <- density(Diff.lengthdiff_m_train)
kernel.length_nm <- density(Diff.lengthdiff_nm_train)
kernel.gvw_m <- density(Diff.gvwdiff_m_train)
kernel.gvw_nm <- density(Diff.gvwdiff_nm_train)
kernel.axsp12_m <- density(Diff.ax12spdiff_m_train)
kernel.axsp12_nm <- density(Diff.ax12spdiff_nm_train)
kernel.axsp23_m <- density(Diff.ax23spdiff_m_train)
kernel.axsp23_nm <- density(Diff.ax23spdiff_nm_train)
kernel.axsp34_m <- density(Diff.ax34spdiff_m_train)
kernel.axsp34_nm <- density(Diff.ax34spdiff_nm_train)
kernel.axsp45_m <- density(Diff.ax45spdiff_m_train)
kernel.axsp45_nm <- density(Diff.ax45spdiff_nm_train)
kernel.dur_m <- density(Diff.durdiff_m_train)
kernel.dur_nm <- density(Diff.durdiff_nm_train)
kernel.utc_m <- density(Diff.utcdiff_m_train)
kernel.utc_nm <- density(Diff.utcdiff_nm_train)
kernel.magdif_m <- density(Diff.magdiff_m_train)
kernel.magdif_nm <- density(Diff.magdiff_nm_train)



## Extract attributes (train)
Upcandidates<- list()
Upcandidates_train<- list()
Upcandidates_test<- list()
Upcandidates_attribute_train <- list()
Upcandidates_attribute_test <- list()
Downtarget_attributes <- data.frame()
thresholdForDif <- 1.5

Attribute_difftemp_train <- list()
Attribute_diff_nonnormal_train <- list()
Attribute_difftemp_test <- list()
Attribute_diff_nonnormal_test <- list()
Upcandidatesindex <- list()
Upcandidatesindex_train <- list()
Upcandidatesindex_test <- list()

Downtarget_attributes_all <- cbind(Downheader_new[,13:20],Downheader_new[,7],Downheader_new[,12]  )
Downtarget_attributes_train <- subset( Downtarget_attributes_all[,], as.numeric(str_sub (Downtarget_attributes_all[,10],-13,-1) ) <  1357804800000  )
Downtarget_attributes_test <- subset( Downtarget_attributes_all[,], as.numeric(str_sub (Downtarget_attributes_all[,10],-13,-1) ) >   1357804800000   )

Upheader_new_train <- data.frame()
Upheader_new_test <- data.frame()
Upheader_new_train <-  subset(Upheader_new[,], as.numeric(str_sub (Upheader_new[,12],-13,-1) ) <  1357804800000  )
Upheader_new_test <-  subset(Upheader_new[,], as.numeric(str_sub (Upheader_new[,12],-13,-1) ) >  1357804800000  )

a_magdif_train <- a_magdif[DownheaderTrainIdx]
a_magdif_test <- a_magdif[DownheaderTestIdx]

for (i in 1: length(Upsiglist_train)) {  
  
  Upcandidatesindex_train[[i]] <- which(a_magdif_train[[i]] < thresholdForDif * min(a_magdif_train[[i]]) )
  Upcandidates_train[[i]] <- subset (Upsiglist_train[[i]], a_magdif_train[[i]] < thresholdForDif * min(a_magdif_train[[i]]) )
  
  # train 
  Upcandidates_attribute_train[[i]] <- cbind(      
    Upheader_new_train[ match( as.numeric(Upcandidates_train[[i]]), as.numeric(Upheader_new_train[,13])),13:20] ,
    Upheader_new_train[ match( as.numeric(Upcandidates_train[[i]]), as.numeric(Upheader_new_train[,13])),7] ,
    Upheader_new_train[ match( as.numeric(Upcandidates_train[[i]]), as.numeric(Upheader_new_train[,13])),12])
  
  for (j in 1: length(Upcandidates_attribute_train[[i]])) {  
    Attribute_difftemp_train[[j]] <- abs( as.numeric( (unlist (Upcandidates_attribute_train[[i]][j,3:length(Upcandidates_attribute_train[[i]])]))) - 
                                      as.numeric(Downtarget_attributes_train[i,3:length(Upcandidates_attribute_train[[i]]) ])  )

  }


  Attribute_diff_nonnormal_train[[length(Attribute_diff_nonnormal_train)+1]] <- Attribute_difftemp_train # list in the list
  Attribute_difftemp_train <- list()
}

# test
for (i in 1: length(Upsiglist_test)) {  
  
  Upcandidatesindex_test[[i]] <- which(a_magdif_test[[i]] < thresholdForDif * min(a_magdif_test[[i]]) )
  Upcandidates_test[[i]] <- subset (Upsiglist_test[[i]], a_magdif_test[[i]] < thresholdForDif * min(a_magdif_test[[i]]) )
  
  
  Upcandidates_attribute_test[[i]] <- cbind(      
    Upheader_new_test[ match( as.numeric(Upcandidates_test[[i]]), as.numeric(Upheader_new_test[,13])),13:20] ,
    Upheader_new_test[ match( as.numeric(Upcandidates_test[[i]]), as.numeric(Upheader_new_test[,13])),7] ,
    Upheader_new_test[ match( as.numeric(Upcandidates_test[[i]]), as.numeric(Upheader_new_test[,13])),12])
  
  for (j in 1: length(Upcandidates_test[[i]])) {  
    Attribute_difftemp_test[[j]] <- abs( as.numeric( (unlist (Upcandidates_attribute_test[[i]][j,3:length(Upcandidates_attribute_test[[i]])]))) - 
                                           as.numeric(Downtarget_attributes_test[i,3:length(Upcandidates_attribute_test[[i]]) ])  )
    
  }
  
  Attribute_diff_nonnormal_test[[length(Attribute_diff_nonnormal_test)+1]] <- Attribute_difftemp_test # list in the list
  Attribute_difftemp_test <- list()
}

Attribute_diff_train <- Attribute_diff_nonnormal_train
Attribute_diff_test <- Attribute_diff_nonnormal_test

# # normalize attribute_diff
# for (i in 1: length(Upsiglist)) {  
#   for (j in 1: length(Upcandidates[[i]])) {  
#     Attribute_diff[[i]][[j]][1] <- as.numeric (Attribute_diff_nonnormal[[i]][[j]][1]) / maxlength
#     Attribute_diff[[i]][[j]][2] <- as.numeric (Attribute_diff_nonnormal[[i]][[j]][2]) / maxgvw
#     Attribute_diff[[i]][[j]][3] <- as.numeric (Attribute_diff_nonnormal[[i]][[j]][3]) / maxax12sp
#     Attribute_diff[[i]][[j]][4] <- as.numeric (Attribute_diff_nonnormal[[i]][[j]][4]) / maxax23sp
#     Attribute_diff[[i]][[j]][5] <- as.numeric (Attribute_diff_nonnormal[[i]][[j]][5]) / maxax34sp
#     Attribute_diff[[i]][[j]][6] <- as.numeric (Attribute_diff_nonnormal[[i]][[j]][6]) / maxax45sp
#     Attribute_diff[[i]][[j]][7] <- as.numeric (Attribute_diff_nonnormal[[i]][[j]][7]) / maxdur
#     Attribute_diff[[i]][[j]][8] <- as.numeric (Attribute_diff_nonnormal[[i]][[j]][8]) / maxutc
#     Attribute_diff[[i]][[j]][9] <- as.numeric (Attribute_diff_nonnormal[[i]][[j]][9]) / maxmag
#   
#   }}



#  utils::View( Upcandidates_attribute[[1]] )
# View( Upcandidates_attribute[[i]][j,])
# View( Downtarget_attributes[i,])

# Only Class 9
jointprobtemp_train <- data.frame()
jointprob_train <- list()
idxjointprob_train <- vector()
UpFinalcandidates_train <- vector()

jointprobtemp_test <- data.frame()
jointprob_test <- list()
idxjointprob_test <- vector()
UpFinalcandidates_test <- vector()
k <- 0
m <- 0

Target_baseanalysis_Jan0910_table_train <- subset(Target_baseanalysis_Jan0910_table, 
      as.numeric(str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) <  1357804800000  )
Target_baseanalysis_Jan0910_table_test<- subset(Target_baseanalysis_Jan0910_table, 
      as.numeric(str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) >  1357804800000  )


for (i in 1:length(Upcandidates_train)){
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
    
# for (i in 1:length (Target_baseanalysis_Jan0910_table_train[,1])){
#   if ( as.numeric (Target_baseanalysis_Jan0910_table_train[i,1] ) == 9 ) { 
  
   k <- k+1
                                                           
      for (j in 1: length(Attribute_diff_train[[i]])) {
                                                             
         jointprobtemp_train[j,1] <-(approx(kernel.length_m$x, kernel.length_m$y, Attribute_diff_train[[i]][[j]][1]) )$y
         jointprobtemp_train[j,2] <-(approx(kernel.gvw_m$x, kernel.gvw_m$y, Attribute_diff_train[[i]][[j]][2]) )$y
         jointprobtemp_train[j,3] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, Attribute_diff_train[[i]][[j]][3]) )$y
         jointprobtemp_train[j,4] <-(approx(kernel.axsp23_m$x, kernel.axsp23_m$y, Attribute_diff_train[[i]][[j]][4]) )$y
         jointprobtemp_train[j,5] <-(approx(kernel.axsp34_m$x, kernel.axsp34_m$y, Attribute_diff_train[[i]][[j]][5]) )$y
         jointprobtemp_train[j,6] <-(approx(kernel.axsp45_m$x, kernel.axsp45_m$y, Attribute_diff_train[[i]][[j]][6]) )$y
         jointprobtemp_train[j,7] <-(approx(kernel.dur_m$x, kernel.dur_m$y, Attribute_diff_train[[i]][[j]][7]) )$y
         jointprobtemp_train[j,8] <-(approx(kernel.utc_m$x, kernel.utc_m$y, Attribute_diff_train[[i]][[j]][8]) )$y
         jointprobtemp_train[j,9] <- as.numeric (prob[[i]][[1]][Upcandidatesindex_train[[i]][j]] ) # CHECK!!!
                                                             
         jointprobtemp_train [is.na(jointprobtemp_train )] <- 0.000000000001
         jointprobtemp_train[j,10] <- jointprobtemp_train[j,1]* jointprobtemp_train[j,2] * jointprobtemp_train[j,3] *
            jointprobtemp_train[j,4] *  jointprobtemp_train[j,5] * jointprobtemp_train[j,6] *
            jointprobtemp_train[j,7] * jointprobtemp_train[j,8] * jointprobtemp_train[j,9] 
                                                                                                                       
       }
    jointprob_train[[length(jointprob_train)+1]] <-  jointprobtemp_train
    jointprobtemp_train <- data.frame()
    idxjointprob_train[k] <- which.max(unlist (jointprob_train[[k]][10]) )
                                                           
    UpFinalcandidates_train[k] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[k]] ]
  }
}
  
  #test
# for (i in 1:length(Target_baseanalysis_Jan0910_table_test[,1])){
#   if ( as.numeric (Target_baseanalysis_Jan0910_table_test[i,1] ) == 9 ) { 
    
for (i in 1:length(Upcandidates_test)){
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    m <- m+1
                                                                
    for (j in 1: length(Attribute_diff_test[[i]])) {
                                                                  
       jointprobtemp_test[j,1] <-(approx(kernel.length_m$x, kernel.length_m$y, Attribute_diff_test[[i]][[j]][1]) )$y
       jointprobtemp_test[j,2] <-(approx(kernel.gvw_m$x, kernel.gvw_m$y, Attribute_diff_test[[i]][[j]][2]) )$y
       jointprobtemp_test[j,3] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, Attribute_diff_test[[i]][[j]][3]) )$y
       jointprobtemp_test[j,4] <-(approx(kernel.axsp23_m$x, kernel.axsp23_m$y, Attribute_diff_test[[i]][[j]][4]) )$y
       jointprobtemp_test[j,5] <-(approx(kernel.axsp34_m$x, kernel.axsp34_m$y, Attribute_diff_test[[i]][[j]][5]) )$y
       jointprobtemp_test[j,6] <-(approx(kernel.axsp45_m$x, kernel.axsp45_m$y, Attribute_diff_test[[i]][[j]][6]) )$y
       jointprobtemp_test[j,7] <-(approx(kernel.dur_m$x, kernel.dur_m$y, Attribute_diff_test[[i]][[j]][7]) )$y
       jointprobtemp_test[j,8] <-(approx(kernel.utc_m$x, kernel.utc_m$y, Attribute_diff_test[[i]][[j]][8]) )$y
       jointprobtemp_test[j,9] <- as.numeric (prob[[i]][[1]][Upcandidatesindex_test[[i]][j]] ) # CHECK!!!
                                                                  
       jointprobtemp_test [is.na(jointprobtemp_test )] <- 0.000000000001
       jointprobtemp_test[j,10] <- jointprobtemp_test[j,1]* jointprobtemp_test[j,2] * jointprobtemp_test[j,3] *
           jointprobtemp_test[j,4] *  jointprobtemp_test[j,5] * jointprobtemp_test[j,6] *
           jointprobtemp_test[j,7] * jointprobtemp_test[j,8] * jointprobtemp_test[j,9] 
                                                                  
     }
    jointprob_test[[length(jointprob_test)+1]] <-  jointprobtemp_test
    jointprobtemp_test <- data.frame()
    idxjointprob_test[m] <- which.max(unlist (jointprob_test[[m]][10]) )
                                                                
    UpFinalcandidates_test[m] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[m]] ]
  } 
}


                                                                     
                                                   


rm(ResultMisMatching_train, ResultMisMatching_test)

ResultMisMatching_train <- cbind(Target_baseanalysis_Jan0910_table_train[Target_baseanalysis_Jan0910_table_train[,1] == 9, 6],
      Target_baseanalysis_Jan0910_table_train[Target_baseanalysis_Jan0910_table_train[,1] == 9, 4], UpFinalcandidates_train)
ResultMisMatching_test<- cbind(Target_baseanalysis_Jan0910_table_test[Target_baseanalysis_Jan0910_table_test[,1] == 9, 6],
      Target_baseanalysis_Jan0910_table_test[Target_baseanalysis_Jan0910_table_test[,1] == 9, 4], UpFinalcandidates_test)

### performance 

# train
ResultMisMatching_train <- na.omit(ResultMisMatching_train)
TargetTable_train <- ResultMisMatching_train 
Target_obj_train  <- ResultMisMatching_train [,1]

missing_obj_train  <- length (Target_obj_train[Target_obj_train == 999]) 
matching_obj_train <- length (Target_obj_train[Target_obj_train != 999]) 

matching_NN_train <- sum ( as.numeric ((ResultMisMatching_train [,1]) == as.numeric (ResultMisMatching_train [,3])) &
                      as.numeric (ResultMisMatching_train [,1]) != 999)

missing_NN_train <- sum ( as.numeric (ResultMisMatching_train[,1]) == c(999))

CMVeh_train <-  matching_NN_train[1]
CVeh_train <- matching_obj_train[1]
p <- 3
MVeh_train <- sum(   (as.numeric( TargetTable_train[,p])) > 1000 )  

SIMR_train <- CMVeh_train / CVeh_train
SCMR_train <- CMVeh_train / MVeh_train

MMVeh_train <- length(  subset(TargetTable_train[,1], as.numeric( Target_obj_train ) 
                         !=  as.numeric( TargetTable_train[,p])   ))


Veh_train <- length(TargetTable_train[,1])
SER_train <- MMVeh_train / Veh_train

ResultMismatching_train<- data.frame( matching_obj_train[1], missing_obj_train[1],              
                      matching_NN_train[[1]],  missing_NN_train[[1]],
                      CMVeh_train[[1]], CVeh_train[[1]], MVeh_train[[1]],
                      SIMR_train[[1]], SCMR_train[[1]], MMVeh_train[[1]], Veh_train[[1]], SER_train[[1]] )


# test
ResultMisMatching_test <- na.omit(ResultMisMatching_test)
TargetTable_test <- ResultMisMatching_test 
Target_obj_test  <- ResultMisMatching_test [,1]

missing_obj_test  <- length (Target_obj_test[Target_obj_test == 999]) 
matching_obj_test <- length (Target_obj_test[Target_obj_test != 999]) 

matching_NN_test <- sum ( as.numeric ((ResultMisMatching_test[,1]) == as.numeric (ResultMisMatching_test[,3])) &
                            as.numeric (ResultMisMatching_test [,1]) != 999)

missing_NN_test <- sum ( as.numeric (ResultMisMatching_test [,1]) == c(999))

CMVeh_test <-  matching_NN_test[1]
CVeh_test <- matching_obj_test[1]
p <- 3
MVeh_test <- sum(   (as.numeric( TargetTable_test[,p])) > 1000 )  

SIMR_test <- CMVeh_test / CVeh_test
SCMR_test <- CMVeh_test / MVeh_test

MMVeh_test <- length(  subset(TargetTable_test[,1], as.numeric( Target_obj_test ) 
                              !=  as.numeric( TargetTable_test[,p])   ))


Veh_test <- length(TargetTable_test[,1])
SER_test <- MMVeh_test / Veh_test

ResultMismatching_test <- data.frame( matching_obj_test[1], missing_obj_test[1],              
                                     matching_NN_test[[1]],  missing_NN_test[[1]],
                                     CMVeh_test[[1]], CVeh_test[[1]], MVeh_test[[1]],
                                     SIMR_test[[1]], SCMR_test[[1]], MMVeh_test[[1]], Veh_test[[1]], SER_test[[1]] )
# rm(test)
# options(digits=6)
# test <- cbind(sub_all[,3], sub_all[,5], sub_all[,6],sub_all[,27],sub_all[,28] ,sub_all[,30])
# test <- round(test , digits=6)