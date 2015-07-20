# the script reads the cell-count data (Reference) and significant matrix, and calculate nnlm coefficients

sigMat <- read.csv("sigMat.csv")
mixData <- read.csv("Mix.csv")

#set.seed(20+i)
#B = sigMat[,2]
#CD4T = sigMat[,3]
#CD8T = sigMat[,4]
#G = sigMat[,5]
#Mo =sigMat[,6]
#NK = sigMat[,7]
#RBC = sigMat[,8]

#print(head(sigMat))
#print(head(mixData))

for (i in 1:24){
      

      no = sprintf("%01d", i)
      no2 = sprintf("%01d", i+221)
      if (i+221 > 228){ no2 = sprintf("%01d", i+222)}
      if (i+221 >238){no2 = sprintf("%01d", i+223)}
      
      Mix = mixData[,i+1]
      print(head(Mix))
      model = nnls(as.matrix(sigMat[,2:8]), mixData[,i+1])
      

      #print(model)
      coefs <- coef(model)
      print(coefs)
      
      coefsNorm <- coefs[1:7]*100/ sum(coefs[1:7])
      #filename = paste("lmCo", no, ".csv")
      sampleName = paste("TS", no2, sep = "")
      
      if (i ==1){  
          write.table( t(as.data.frame(coefsNorm)), file = "nnlmCoefsData.csv", append=T, sep="," , row.names = sampleName, col.names=c("\",\"Mo","G","RBC", "NK", "B" , "CD4T" , "CD8T"))
      }else{
        write.table( t(as.data.frame(coefsNorm)), file = "nnlmCoefsData.csv", append=T, sep="," , row.names = sampleName, col.names= F)
        
      }
      
      
}
      