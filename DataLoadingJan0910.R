################ Data Input ####################
### SOLC Jan 0910
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 
# rm(list=ls())

LC.Jan09ML3Header1=read.table("./RawData/LCJan/ML30109/84_IST0001297_CA130109212114_fileIdx.txt",  fill=T)
LC.Jan09ML3Header2=read.table("./RawData/LCJan/ML30109/84_IST0001297_CA130109233022_fileIdx.txt",  fill=T)
LC.Jan09ML3sig1=read.table("./RawData/LCJan/ML30109/IST0001297_CA130109212114.txt", fill=T)
LC.Jan09ML3sig2=read.table("./RawData/LCJan/ML30109/IST0001297_CA130109233022.txt", fill=T)

LC.Jan10ML3Header1=read.table("./RawData/LCJan/ML30110/84_IST0001297_CA130110080000_fileIdx.txt",  fill=T)
LC.Jan10ML3Header2=read.table("./RawData/LCJan/ML30110/84_IST0001297_CA130110154334_fileIdx.txt",  fill=T)
LC.Jan10ML3Header3=read.table("./RawData/LCJan/ML30110/84_IST0001297_CA130110210348_fileIdx.txt",  fill=T)
LC.Jan10ML3Header4=read.table("./RawData/LCJan/ML30110/84_IST0001297_CA130110234405_fileIdx.txt",  fill=T)
LC.Jan10ML3sig1=read.table("./RawData/LCJan/ML30110/IST0001297_CA130110080000.txt", fill=T)
LC.Jan10ML3sig2=read.table("./RawData/LCJan/ML30110/IST0001297_CA130110154334.txt", fill=T)
LC.Jan10ML3sig3=read.table("./RawData/LCJan/ML30110/IST0001297_CA130110210348.txt", fill=T)
LC.Jan10ML3sig4=read.table("./RawData/LCJan/ML30110/IST0001297_CA130110234405.txt", fill=T)

LC.Jan09ML4Header1=read.table("./RawData/LCJan/ML40109/84_IST0001210_CA130109212114_fileIdx.txt",  fill=T)
LC.Jan09ML4Header2=read.table("./RawData/LCJan/ML40109/84_IST0001210_CA130109233022_fileIdx.txt",  fill=T)
LC.Jan09ML4sig1=read.table("./RawData/LCJan/ML40109/IST0001210_CA130109212114.txt", fill=T)
LC.Jan09ML4sig2=read.table("./RawData/LCJan/ML40109/IST0001210_CA130109233022.txt", fill=T)

LC.Jan10ML4Header1=read.table("./RawData/LCJan/ML40110/84_IST0001210_CA130110080000_fileIdx.txt",  fill=T)
LC.Jan10ML4Header2=read.table("./RawData/LCJan/ML40110/84_IST0001210_CA130110154334_fileIdx.txt",  fill=T)
LC.Jan10ML4Header3=read.table("./RawData/LCJan/ML40110/84_IST0001210_CA130110210348_fileIdx.txt",  fill=T)
LC.Jan10ML4Header4=read.table("./RawData/LCJan/ML40110/84_IST0001210_CA130110234405_fileIdx.txt",  fill=T)
LC.Jan10ML4sig1=read.table("./RawData/LCJan/ML40110/IST0001210_CA130110080000.txt", fill=T)
LC.Jan10ML4sig2=read.table("./RawData/LCJan/ML40110/IST0001210_CA130110154334.txt", fill=T)
LC.Jan10ML4sig3=read.table("./RawData/LCJan/ML40110/IST0001210_CA130110210348.txt", fill=T)
LC.Jan10ML4sig4=read.table("./RawData/LCJan/ML40110/IST0001210_CA130110234405.txt", fill=T)

SO.Jan09ML3Header1=read.table("./RawData/SOJan/ML30109/810_IST0001070_CA130109195745_fileIdx.txt",  fill=T)
SO.Jan09ML3sig1=read.table("./RawData/SOJan/ML30109/IST0001070_CA130109195745.txt", fill=T)

SO.Jan10ML3Header1=read.table("./RawData/SOJan/ML30110/810_IST0001070_CA130110080000_fileIdx.txt",  fill=T)
SO.Jan10ML3Header2=read.table("./RawData/SOJan/ML30110/810_IST0001070_CA130110192108_fileIdx.txt",  fill=T)
SO.Jan10ML3sig1=read.table("./RawData/SOJan/ML30110/IST0001070_CA130110080000.txt", fill=T)
SO.Jan10ML3sig2=read.table("./RawData/SOJan/ML30110/IST0001070_CA130110192108.txt", fill=T)

SO.Jan09ML4Header1=read.table("./RawData/SOJan/ML40109/810_IST0001030_CA130109195745_fileIdx.txt",  fill=T)
SO.Jan09ML4sig1=read.table("./RawData/SOJan/ML40109/IST0001030_CA130109195745.txt", fill=T)

SO.Jan10ML4Header1=read.table("./RawData/SOJan/ML40110/810_IST0001030_CA130110080000_fileIdx.txt",  fill=T)
SO.Jan10ML4Header2=read.table("./RawData/SOJan/ML40110/810_IST0001030_CA130110192108_fileIdx.txt",  fill=T)
SO.Jan10ML4sig1=read.table("./RawData/SOJan/ML40110/IST0001030_CA130110080000.txt", fill=T)
SO.Jan10ML4sig2=read.table("./RawData/SOJan/ML40110/IST0001030_CA130110192108.txt", fill=T)



### col name for sig file
colnames(LC.Jan09ML3sig1) <- c("id", "v2", "v3","v4","v5","v6")
colnames(LC.Jan09ML3sig2) <- c("id", "v2", "v3","v4","v5","v6")
colnames(LC.Jan10ML3sig1) <- c("id", "v2", "v3","v4","v5","v6")
colnames(LC.Jan10ML3sig2) <- c("id", "v2", "v3","v4","v5","v6")
colnames(LC.Jan10ML3sig3) <- c("id", "v2", "v3","v4","v5","v6")
colnames(LC.Jan10ML3sig4) <- c("id", "v2", "v3","v4","v5","v6")

colnames(LC.Jan09ML4sig1) <- c("id", "v2", "v3","v4","v5","v6")
colnames(LC.Jan09ML4sig2) <- c("id", "v2", "v3","v4","v5","v6")
colnames(LC.Jan10ML4sig1) <- c("id", "v2", "v3","v4","v5","v6")
colnames(LC.Jan10ML4sig2) <- c("id", "v2", "v3","v4","v5","v6")
colnames(LC.Jan10ML4sig3) <- c("id", "v2", "v3","v4","v5","v6")
colnames(LC.Jan10ML4sig4) <- c("id", "v2", "v3","v4","v5","v6")

colnames(SO.Jan09ML3sig1) <- c("id", "v2", "v3","v4","v5","v6")
colnames(SO.Jan10ML3sig1) <- c("id", "v2", "v3","v4","v5","v6")
colnames(SO.Jan10ML3sig2) <- c("id", "v2", "v3","v4","v5","v6")
colnames(SO.Jan09ML4sig1) <- c("id", "v2", "v3","v4","v5","v6")
colnames(SO.Jan10ML4sig1) <- c("id", "v2", "v3","v4","v5","v6")
colnames(SO.Jan10ML4sig2) <- c("id", "v2", "v3","v4","v5","v6")


LC.Jan09ML3sig1 <- subset(LC.Jan09ML3sig1, select = c("id", "v2", "v3") )
LC.Jan09ML3sig2 <- subset(LC.Jan09ML3sig2, select = c("id", "v2", "v3") )
LC.Jan10ML3sig1 <- subset(LC.Jan10ML3sig1, select = c("id", "v2", "v3") )
LC.Jan10ML3sig2 <- subset(LC.Jan10ML3sig2, select = c("id", "v2", "v3") )
LC.Jan10ML3sig3 <- subset(LC.Jan10ML3sig3, select = c("id", "v2", "v3") )
LC.Jan10ML3sig4 <- subset(LC.Jan10ML3sig4, select = c("id", "v2", "v3") )

LC.Jan09ML4sig1 <- subset(LC.Jan09ML4sig1, select = c("id", "v2", "v3") )
LC.Jan09ML4sig2 <- subset(LC.Jan09ML4sig2, select = c("id", "v2", "v3") )
LC.Jan10ML4sig1 <- subset(LC.Jan10ML4sig1, select = c("id", "v2", "v3") )
LC.Jan10ML4sig2 <- subset(LC.Jan10ML4sig2, select = c("id", "v2", "v3") )
LC.Jan10ML4sig3 <- subset(LC.Jan10ML4sig3, select = c("id", "v2", "v3") )
LC.Jan10ML4sig4 <- subset(LC.Jan10ML4sig4, select = c("id", "v2", "v3") )

SO.Jan09ML3sig1 <- subset(SO.Jan09ML3sig1, select = c("id", "v2", "v3") )
SO.Jan10ML3sig1 <- subset(SO.Jan10ML3sig1, select = c("id", "v2", "v3") )
SO.Jan10ML3sig2 <- subset(SO.Jan10ML3sig2, select = c("id", "v2", "v3") )
SO.Jan09ML4sig1 <- subset(SO.Jan09ML4sig1, select = c("id", "v2", "v3") )
SO.Jan10ML4sig1 <- subset(SO.Jan10ML4sig1, select = c("id", "v2", "v3") )
SO.Jan10ML4sig2 <- subset(SO.Jan10ML4sig2, select = c("id", "v2", "v3") )


### Add vehid in Header file 
# LC Jan09 ML3 - header 1 & 2
uctJan09 <- 1357718400
LC.Jan09ML3Header1[,length(LC.Jan09ML3Header1)+1]<- 1000*(uctJan09 + LC.Jan09ML3Header1[,length(LC.Jan09ML3Header1)])

x<- rep(53,nrow(LC.Jan09ML3Header1))
y<- LC.Jan09ML3Header1[,ncol(LC.Jan09ML3Header1)]
len <- length (LC.Jan09ML3Header1)+1

for (i in 1:nrow(LC.Jan09ML3Header1)){
  LC.Jan09ML3Header1[i,len] <- paste (x[i],y[i], sep="")
}

colnames(LC.Jan09ML3Header1) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")


LC.Jan09ML3Header2[,length(LC.Jan09ML3Header2)+1]<- 1000*(uctJan09 + LC.Jan09ML3Header2[,length(LC.Jan09ML3Header2)])

x<- rep(53,nrow(LC.Jan09ML3Header2))
y<- LC.Jan09ML3Header2[,ncol(LC.Jan09ML3Header2)]
len <- length (LC.Jan09ML3Header2)+1

for (i in 1:nrow(LC.Jan09ML3Header2)){
  LC.Jan09ML3Header2[i,len] <- paste (x[i],y[i], sep="")
}

colnames(LC.Jan09ML3Header2) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")


# LC Jan09 ML4 - header 1 & 2

LC.Jan09ML4Header1[,length(LC.Jan09ML4Header1)+1]<- 1000*(uctJan09 + LC.Jan09ML4Header1[,length(LC.Jan09ML4Header1)])

x<- rep(54,nrow(LC.Jan09ML4Header1))
y<- LC.Jan09ML4Header1[,ncol(LC.Jan09ML4Header1)]
len <- length (LC.Jan09ML4Header1)+1

for (i in 1:nrow(LC.Jan09ML4Header1)){
  LC.Jan09ML4Header1[i,len] <- paste (x[i],y[i], sep="")
}
colnames(LC.Jan09ML4Header1) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")


LC.Jan09ML4Header2[,length(LC.Jan09ML4Header2)+1]<- 1000*(uctJan09 + LC.Jan09ML4Header2[,length(LC.Jan09ML4Header2)])

x<- rep(54,nrow(LC.Jan09ML4Header2))
y<- LC.Jan09ML4Header2[,ncol(LC.Jan09ML4Header2)]
len <- length (LC.Jan09ML4Header2)+1

for (i in 1:nrow(LC.Jan09ML4Header2)){
  LC.Jan09ML4Header2[i,len] <- paste (x[i],y[i], sep="")
}
colnames(LC.Jan09ML4Header2) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")


# LC Jan10 ML3 - header 1 & 2 & 3 & 4
uctJan10 <- 1357804800
LC.Jan10ML3Header1[,length(LC.Jan10ML3Header1)+1]<- 1000*(uctJan10 + LC.Jan10ML3Header1[,length(LC.Jan10ML3Header1)])

x<- rep(53,nrow(LC.Jan10ML3Header1))
y<- LC.Jan10ML3Header1[,ncol(LC.Jan10ML3Header1)]
len <- length (LC.Jan10ML3Header1)+1

for (i in 1:nrow(LC.Jan10ML3Header1)){
  LC.Jan10ML3Header1[i,len] <- paste (x[i],y[i], sep="")
}

colnames(LC.Jan10ML3Header1) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")


LC.Jan10ML3Header2[,length(LC.Jan10ML3Header2)+1]<- 1000*(uctJan10 + LC.Jan10ML3Header2[,length(LC.Jan10ML3Header2)])

x<- rep(53,nrow(LC.Jan10ML3Header2))
y<- LC.Jan10ML3Header2[,ncol(LC.Jan10ML3Header2)]
len <- length (LC.Jan10ML3Header2)+1

for (i in 1:nrow(LC.Jan10ML3Header2)){
  LC.Jan10ML3Header2[i,len] <- paste (x[i],y[i], sep="")
}

colnames(LC.Jan10ML3Header2) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")

LC.Jan10ML3Header3[,length(LC.Jan10ML3Header3)+1]<- 1000*(uctJan10 + LC.Jan10ML3Header3[,length(LC.Jan10ML3Header3)])

x<- rep(53,nrow(LC.Jan10ML3Header3))
y<- LC.Jan10ML3Header3[,ncol(LC.Jan10ML3Header3)]
len <- length (LC.Jan10ML3Header3)+1

for (i in 1:nrow(LC.Jan10ML3Header3)){
  LC.Jan10ML3Header3[i,len] <- paste (x[i],y[i], sep="")
}

colnames(LC.Jan10ML3Header3) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")


LC.Jan10ML3Header4[,length(LC.Jan10ML3Header4)+1]<- 1000*(uctJan10 + LC.Jan10ML3Header4[,length(LC.Jan10ML3Header4)])

x<- rep(53,nrow(LC.Jan10ML3Header4))
y<- LC.Jan10ML3Header4[,ncol(LC.Jan10ML3Header4)]
len <- length (LC.Jan10ML3Header4)+1

for (i in 1:nrow(LC.Jan10ML3Header4)){
  LC.Jan10ML3Header4[i,len] <- paste (x[i],y[i], sep="")
}

colnames(LC.Jan10ML3Header4) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")



# LC Jan10 ML4 - header 1 & 2 & 3 & 4
LC.Jan10ML4Header1[,length(LC.Jan10ML4Header1)+1]<- 1000*(uctJan10 + LC.Jan10ML4Header1[,length(LC.Jan10ML4Header1)])

x<- rep(54,nrow(LC.Jan10ML4Header1))
y<- LC.Jan10ML4Header1[,ncol(LC.Jan10ML4Header1)]
len <- length (LC.Jan10ML4Header1)+1

for (i in 1:nrow(LC.Jan10ML4Header1)){
  LC.Jan10ML4Header1[i,len] <- paste (x[i],y[i], sep="")
}

colnames(LC.Jan10ML4Header1) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")


LC.Jan10ML4Header2[,length(LC.Jan10ML4Header2)+1]<- 1000*(uctJan10 + LC.Jan10ML4Header2[,length(LC.Jan10ML4Header2)])

x<- rep(54,nrow(LC.Jan10ML4Header2))
y<- LC.Jan10ML4Header2[,ncol(LC.Jan10ML4Header2)]
len <- length (LC.Jan10ML4Header2)+1

for (i in 1:nrow(LC.Jan10ML4Header2)){
  LC.Jan10ML4Header2[i,len] <- paste (x[i],y[i], sep="")
}

colnames(LC.Jan10ML4Header2) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")

LC.Jan10ML4Header3[,length(LC.Jan10ML4Header3)+1]<- 1000*(uctJan10 + LC.Jan10ML4Header3[,length(LC.Jan10ML4Header3)])

x<- rep(54,nrow(LC.Jan10ML4Header3))
y<- LC.Jan10ML4Header3[,ncol(LC.Jan10ML4Header3)]
len <- length (LC.Jan10ML4Header3)+1

for (i in 1:nrow(LC.Jan10ML4Header3)){
  LC.Jan10ML4Header3[i,len] <- paste (x[i],y[i], sep="")
}

colnames(LC.Jan10ML4Header3) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")


LC.Jan10ML4Header4[,length(LC.Jan10ML4Header4)+1]<- 1000*(uctJan10 + LC.Jan10ML4Header4[,length(LC.Jan10ML4Header4)])

x<- rep(54,nrow(LC.Jan10ML4Header4))
y<- LC.Jan10ML4Header4[,ncol(LC.Jan10ML4Header4)]
len <- length (LC.Jan10ML4Header4)+1

for (i in 1:nrow(LC.Jan10ML4Header4)){
  LC.Jan10ML4Header4[i,len] <- paste (x[i],y[i], sep="")
}

colnames(LC.Jan10ML4Header4) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")



# SO Jan09 ML3 - header 1 

SO.Jan09ML3Header1[,length(SO.Jan09ML3Header1)+1]<- 1000*(uctJan09 + SO.Jan09ML3Header1[,length(SO.Jan09ML3Header1)])

x<- rep(93,nrow(SO.Jan09ML3Header1))
y<- SO.Jan09ML3Header1[,ncol(SO.Jan09ML3Header1)]
len <- length (SO.Jan09ML3Header1)+1

for (i in 1:nrow(SO.Jan09ML3Header1)){
  SO.Jan09ML3Header1[i,len] <- paste (x[i],y[i], sep="")
}

colnames(SO.Jan09ML3Header1) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")


# SO Jan10 ML3 - header 1 & 2

SO.Jan10ML3Header1[,length(SO.Jan10ML3Header1)+1]<- 1000*(uctJan10 + SO.Jan10ML3Header1[,length(SO.Jan10ML3Header1)])

x<- rep(93,nrow(SO.Jan10ML3Header1))
y<- SO.Jan10ML3Header1[,ncol(SO.Jan10ML3Header1)]
len <- length (SO.Jan10ML3Header1)+1

for (i in 1:nrow(SO.Jan10ML3Header1)){
  SO.Jan10ML3Header1[i,len] <- paste (x[i],y[i], sep="")
}

colnames(SO.Jan10ML3Header1) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")

SO.Jan10ML3Header2[,length(SO.Jan10ML3Header2)+1]<- 1000*(uctJan10 + SO.Jan10ML3Header2[,length(SO.Jan10ML3Header2)])

x<- rep(93,nrow(SO.Jan10ML3Header2))
y<- SO.Jan10ML3Header2[,ncol(SO.Jan10ML3Header2)]
len <- length (SO.Jan10ML3Header2)+1

for (i in 1:nrow(SO.Jan10ML3Header2)){
  SO.Jan10ML3Header2[i,len] <- paste (x[i],y[i], sep="")
}

colnames(SO.Jan10ML3Header2) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")

# SO Jan09 ML4 - header 1 

SO.Jan09ML4Header1[,length(SO.Jan09ML4Header1)+1]<- 1000*(uctJan09 + SO.Jan09ML4Header1[,length(SO.Jan09ML4Header1)])

x<- rep(94,nrow(SO.Jan09ML4Header1))
y<- SO.Jan09ML4Header1[,ncol(SO.Jan09ML4Header1)]
len <- length (SO.Jan09ML4Header1)+1

for (i in 1:nrow(SO.Jan09ML4Header1)){
  SO.Jan09ML4Header1[i,len] <- paste (x[i],y[i], sep="")
}

colnames(SO.Jan09ML4Header1) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")


# SO Jan10 ML4 - header 1 & 2

SO.Jan10ML4Header1[,length(SO.Jan10ML4Header1)+1]<- 1000*(uctJan10 + SO.Jan10ML4Header1[,length(SO.Jan10ML4Header1)])

x<- rep(94,nrow(SO.Jan10ML4Header1))
y<- SO.Jan10ML4Header1[,ncol(SO.Jan10ML4Header1)]
len <- length (SO.Jan10ML4Header1)+1

for (i in 1:nrow(SO.Jan10ML4Header1)){
  SO.Jan10ML4Header1[i,len] <- paste (x[i],y[i], sep="")
}

colnames(SO.Jan10ML4Header1) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")

SO.Jan10ML4Header2[,length(SO.Jan10ML4Header2)+1]<- 1000*(uctJan10 + SO.Jan10ML4Header2[,length(SO.Jan10ML4Header2)])

x<- rep(94,nrow(SO.Jan10ML4Header2))
y<- SO.Jan10ML4Header2[,ncol(SO.Jan10ML4Header2)]
len <- length (SO.Jan10ML4Header2)+1

for (i in 1:nrow(SO.Jan10ML4Header2)){
  SO.Jan10ML4Header2[i,len] <- paste (x[i],y[i], sep="")
}

colnames(SO.Jan10ML4Header2) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")


### Add veh id in SIG file
#LC
LC.Jan09ML3sig1["sigid"] <- NA
LC.Jan09ML3sig2["sigid"] <- NA
LC.Jan10ML3sig1["sigid"] <- NA
LC.Jan10ML3sig2["sigid"] <- NA
LC.Jan10ML3sig3["sigid"] <- NA
LC.Jan10ML3sig4["sigid"] <- NA

LC.Jan09ML4sig1["sigid"] <- NA
LC.Jan09ML4sig2["sigid"] <- NA
LC.Jan10ML4sig1["sigid"] <- NA
LC.Jan10ML4sig2["sigid"] <- NA
LC.Jan10ML4sig3["sigid"] <- NA
LC.Jan10ML4sig4["sigid"] <- NA

# ML3 - 0109
system.time({
  idx1=match(LC.Jan09ML3sig1$id, LC.Jan09ML3Header1$id)
  idx2 = which(!is.na(idx1))
  LC.Jan09ML3sig1$sigid[idx2] <- LC.Jan09ML3Header1$sigid[idx1[idx2]]
})

system.time({
  idx1=match(LC.Jan09ML3sig2$id, LC.Jan09ML3Header2$id)
  idx2 = which(!is.na(idx1))
  LC.Jan09ML3sig2$sigid[idx2] <- LC.Jan09ML3Header2$sigid[idx1[idx2]]
})

# ML4 - 0109
system.time({
  idx1=match(LC.Jan09ML4sig1$id, LC.Jan09ML4Header1$id)
  idx2 = which(!is.na(idx1))
  LC.Jan09ML4sig1$sigid[idx2] <- LC.Jan09ML4Header1$sigid[idx1[idx2]]
})

system.time({
  idx1=match(LC.Jan09ML4sig2$id, LC.Jan09ML4Header2$id)
  idx2 = which(!is.na(idx1))
  LC.Jan09ML4sig2$sigid[idx2] <- LC.Jan09ML4Header2$sigid[idx1[idx2]]
})

# ML3 - 0110
system.time({
  idx1=match(LC.Jan10ML3sig1$id, LC.Jan10ML3Header1$id)
  idx2 = which(!is.na(idx1))
  LC.Jan10ML3sig1$sigid[idx2] <- LC.Jan10ML3Header1$sigid[idx1[idx2]]
})

system.time({
  idx1=match(LC.Jan10ML3sig2$id, LC.Jan10ML3Header2$id)
  idx2 = which(!is.na(idx1))
  LC.Jan10ML3sig2$sigid[idx2] <- LC.Jan10ML3Header2$sigid[idx1[idx2]]
})

system.time({
  idx1=match(LC.Jan10ML3sig3$id, LC.Jan10ML3Header3$id)
  idx2 = which(!is.na(idx1))
  LC.Jan10ML3sig3$sigid[idx2] <- LC.Jan10ML3Header3$sigid[idx1[idx2]]
})

system.time({
  idx1=match(LC.Jan10ML3sig4$id, LC.Jan10ML3Header4$id)
  idx2 = which(!is.na(idx1))
  LC.Jan10ML3sig4$sigid[idx2] <- LC.Jan10ML3Header4$sigid[idx1[idx2]]
})


# ML4 - 0110
system.time({
  idx1=match(LC.Jan10ML4sig1$id, LC.Jan10ML4Header1$id)
  idx2 = which(!is.na(idx1))
  LC.Jan10ML4sig1$sigid[idx2] <- LC.Jan10ML4Header1$sigid[idx1[idx2]]
})

system.time({
  idx1=match(LC.Jan10ML4sig2$id, LC.Jan10ML4Header2$id)
  idx2 = which(!is.na(idx1))
  LC.Jan10ML4sig2$sigid[idx2] <- LC.Jan10ML4Header2$sigid[idx1[idx2]]
})

system.time({
  idx1=match(LC.Jan10ML4sig3$id, LC.Jan10ML4Header3$id)
  idx2 = which(!is.na(idx1))
  LC.Jan10ML4sig3$sigid[idx2] <- LC.Jan10ML4Header3$sigid[idx1[idx2]]
})

system.time({
  idx1=match(LC.Jan10ML4sig4$id, LC.Jan10ML4Header4$id)
  idx2 = which(!is.na(idx1))
  LC.Jan10ML4sig4$sigid[idx2] <- LC.Jan10ML4Header4$sigid[idx1[idx2]]
})


#SO
SO.Jan09ML3sig1["sigid"] <- NA
SO.Jan10ML3sig1["sigid"] <- NA
SO.Jan10ML3sig2["sigid"] <- NA

SO.Jan09ML4sig1["sigid"] <- NA
SO.Jan10ML4sig1["sigid"] <- NA
SO.Jan10ML4sig2["sigid"] <- NA

# ML3 0109
system.time({
  idx1=match(SO.Jan09ML3sig1$id, SO.Jan09ML3Header1$id)
  idx2 = which(!is.na(idx1))
  SO.Jan09ML3sig1$sigid[idx2] <- SO.Jan09ML3Header1$sigid[idx1[idx2]]
})


# ML3 - 0110
system.time({
  idx1=match(SO.Jan10ML3sig1$id, SO.Jan10ML3Header1$id)
  idx2 = which(!is.na(idx1))
  SO.Jan10ML3sig1$sigid[idx2] <- SO.Jan10ML3Header1$sigid[idx1[idx2]]
})

system.time({
  idx1=match(SO.Jan10ML3sig2$id, SO.Jan10ML3Header2$id)
  idx2 = which(!is.na(idx1))
  SO.Jan10ML3sig2$sigid[idx2] <- SO.Jan10ML3Header2$sigid[idx1[idx2]]
})

# ML4 0109
system.time({
  idx1=match(SO.Jan09ML4sig1$id, SO.Jan09ML4Header1$id)
  idx2 = which(!is.na(idx1))
  SO.Jan09ML4sig1$sigid[idx2] <- SO.Jan09ML4Header1$sigid[idx1[idx2]]
})


# ML4 - 0110
system.time({
  idx1=match(SO.Jan10ML4sig1$id, SO.Jan10ML4Header1$id)
  idx2 = which(!is.na(idx1))
  SO.Jan10ML4sig1$sigid[idx2] <- SO.Jan10ML4Header1$sigid[idx1[idx2]]
})

system.time({
  idx1=match(SO.Jan10ML4sig2$id, SO.Jan10ML4Header2$id)
  idx2 = which(!is.na(idx1))
  SO.Jan10ML4sig2$sigid[idx2] <- SO.Jan10ML4Header2$sigid[idx1[idx2]]
})



### Merge into one file 
#Header file

LC.Jan09Header <- rbind(LC.Jan09ML3Header1, LC.Jan09ML3Header2, LC.Jan09ML4Header1, LC.Jan09ML4Header2)
LC.Jan10Header <- rbind ( LC.Jan10ML3Header1, LC.Jan10ML3Header2, LC.Jan10ML3Header3, LC.Jan10ML3Header4,
                          LC.Jan10ML4Header1, LC.Jan10ML4Header2, LC.Jan10ML4Header3, LC.Jan10ML4Header4)
                         
LC.Jan09Header <- LC.Jan09Header[order(LC.Jan09Header[,12]),] 
LC.Jan10Header <- LC.Jan10Header[order(LC.Jan10Header[,12]),] 
LC.Jan0910Header <- rbind(LC.Jan09Header, LC.Jan10Header )
colnames(LC.Jan09Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")
colnames(LC.Jan10Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")
colnames(LC.Jan0910Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")

SO.Jan09Header <- rbind(SO.Jan09ML3Header1, SO.Jan09ML4Header1)
SO.Jan10Header <- rbind(SO.Jan10ML3Header1, SO.Jan10ML3Header2,
                        SO.Jan10ML4Header1, SO.Jan10ML4Header2)

SO.Jan09Header <- SO.Jan09Header [order(SO.Jan09Header [,12]),] 
SO.Jan10Header <- SO.Jan10Header [order(SO.Jan10Header [,12]),] 
SO.Jan0910Header <- rbind(SO.Jan09Header,SO.Jan10Header )
colnames(SO.Jan09Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")
colnames(SO.Jan10Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")
colnames(SO.Jan0910Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","sigid")

#SIG file
LC.Jan09sig <- rbind(LC.Jan09ML3sig1, LC.Jan09ML3sig2, LC.Jan09ML4sig1, LC.Jan09ML4sig2)
LC.Jan10sig <- rbind(    LC.Jan10ML3sig1, LC.Jan10ML3sig2, LC.Jan10ML3sig3, LC.Jan10ML3sig4,
                         LC.Jan10ML4sig1, LC.Jan10ML4sig2, LC.Jan10ML4sig3, LC.Jan10ML4sig4)
LC.Jan0910sig <- rbind(LC.Jan09sig, LC.Jan10sig )
colnames(LC.Jan09sig) <- c("id", "mag", "v3","sigid")
colnames(LC.Jan10sig) <- c("id", "mag", "v3","sigid")
colnames(LC.Jan0910sig) <- c("id", "mag", "v3","sigid")

SO.Jan09sig <-rbind(SO.Jan09ML3sig1, SO.Jan09ML4sig1)
SO.Jan10sig <-rbind(SO.Jan10ML3sig1, SO.Jan10ML3sig2,
                    SO.Jan10ML4sig1, SO.Jan10ML4sig2)
SO.Jan0910sig <- rbind(SO.Jan09sig, SO.Jan10sig)
colnames(SO.Jan09sig) <- c("id", "mag", "v3","sigid")
colnames(SO.Jan10sig) <- c("id", "mag", "v3","sigid")
colnames(SO.Jan0910sig) <- c("id", "mag", "v3","sigid")

# write.table(LC.Jan0910Header, "./ProcessedData/LC.Jan0910Header.txt", sep="\t")
# write.table(SO.Jan0910Header, "./ProcessedData/SO.Jan0910Header.txt", sep="\t")
# write.table(LC.Jan0910sig, "./ProcessedData/LC.Jan0910sig.txt", sep="\t")
# write.table(SO.Jan0910sig, "./ProcessedData/SO.Jan0910sig.txt", sep="\t")
# load functionbook2
save.image("./ProcessedData/Jan0910/SOLCLoadinJan0910")


