##################################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################################################################################################
# Start
#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# MuTect1 - mutation frequency plot (Chromosome-based), written by Dr Reza Rafiee
# Research Associate, Northern Institute for Cancer Research, Newcastle University
# This script gets a txt file including all mutations obtained from Mutect-1 
############################################# MuTect1 ############################################

Pathfolder <- "~/MuTect1/"  # initialise the path of the txt file
txtfilename <- "VCFsamplefilefromMuTect1Analysis.txt"
pathtxtfile <- paste(Pathfolder,txtfilename,sep = "")
Sample_MuTect1 <- read.table(pathtxtfile,header=T, sep="\t")  # 

Sample_MuTect1_Pass <- Sample_MuTect1[which(Sample_MuTect1$FILTER == "PASS"),]
Sample_MuTect1_Pass_Colourfull <-Sample_MuTect1_Pass 
Sample_MuTect1_Pass_Colourfull[,"TYPE"] <- vector()  # adding a coulmn for the type of mutation
Sample_MuTect1_Pass_Colourfull[,"Col"] <- vector()  # adding a coulmn for the colour of mutation

for (i in 1:nrow(Sample_MuTect1_Pass_Colourfull))
{
  Sample_MuTect1_Pass_Colourfull$TYPE <- paste(Sample_MuTect1_Pass_Colourfull$REF,"-->",Sample_MuTect1_Pass_Colourfull$ALT)
  Sample_MuTect1_Pass_Colourfull$Col <- ifelse(Sample_MuTect1_Pass_Colourfull$TYPE == "A --> C","pink",
                                               ifelse(Sample_MuTect1_Pass_Colourfull$TYPE == "A --> G","azure",
                                                      ifelse(Sample_MuTect1_Pass_Colourfull$TYPE == "A --> T","blue",
                                                             ifelse(Sample_MuTect1_Pass_Colourfull$TYPE == "C --> A","red",
                                                                    ifelse(Sample_MuTect1_Pass_Colourfull$TYPE == "C --> T","green",
                                                                           ifelse(Sample_MuTect1_Pass_Colourfull$TYPE == "C --> G","yellow",
                                                                                  ifelse(Sample_MuTect1_Pass_Colourfull$TYPE == "T --> A","purple",
                                                                                         ifelse(Sample_MuTect1_Pass_Colourfull$TYPE == "T --> C","grey",
                                                                                                ifelse(Sample_MuTect1_Pass_Colourfull$TYPE == "T --> G","black",
                                                                                                       ifelse(Sample_MuTect1_Pass_Colourfull$TYPE == "G --> T","darkgreen",
                                                                                                              ifelse(Sample_MuTect1_Pass_Colourfull$TYPE == "G --> C","darkgoldenrod1","darksalmon"))))))))))) # 
  
}

par(mfrow=c(2,2))
par(mar=c(5,4,4,5) + 0.1)
par(cex.axis=0.8)

for (j in 1:4)
{
  
  Chr_i <- as.integer(which(Sample_MuTect1_Pass_Colourfull$CHROM == j)[1])
  Sample_MuTect1_Pass_Colourfull_Chr_i <- Sample_MuTect1_Pass_Colourfull[which(Sample_MuTect1_Pass_Colourfull$CHROM == j),]
  #print(nrow(Sample_MuTect1_Pass_Colourfull_Chr_i))
  asfactormutect <- as.factor(Sample_MuTect1_Pass_Colourfull_Chr_i$TYPE)
  mutcolour <- as.character(Sample_MuTect1_Pass_Colourfull_Chr_i$Col)
  tb1 <- table(asfactormutect)
  heading <- paste("Chr",j) 
  col_Type <- vector()
  for (k in 1:dim(tb1[order(tb1,decreasing = TRUE)]))
    col_Type[k] <- Sample_MuTect1_Pass_Colourfull_Chr_i$Col[which(Sample_MuTect1_Pass_Colourfull_Chr_i$TYPE == names(tb1[order(tb1,decreasing = TRUE)])[k])[1]] 
  barplot(tb1[order(tb1,decreasing = TRUE)], ylab="Frequency", col= col_Type  , main=heading,xlab= "",ylim=c(0,16),las=2)
}


#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# End
##################################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################################################################################################


