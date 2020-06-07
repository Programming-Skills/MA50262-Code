###################
#
# Read program-1.R and program-2.R using the source function
#
################### 

source("program-1.R")
source("program-2.R")

# The R object digitized.file.names, given below, contains the names of these files. 
#
##############


digitized.file.names = c("san.migel.panobinostat.tsv", "san.migel.placebo.tsv")


################################
#
# The list numbers.below.figure, shown below, contains numbers at risk data. 
#
################################

numbers.below.figure = list(  
  san.migel.panobinostat = c(73, 69, 61, 55, 52, 49, 47, 45, 44, 40, 38, 38, 36, 30, 27, 27, 26, 24, 22, 21, 18, 14, 13, 11, 7, 6, 3, 2, 1,1),    
  san.migel.placebo = c(74, 70, 63, 60, 57, 52, 48, 45, 41, 37, 34, 32, 32, 29, 29, 28, 28, 26, 24, 23, 21, 21, 16, 15, 11, 9, 4, 1,1)
  
)


####################
#
# Now, specify how far along on the x-axis of each line we want to go to extract data. 
#
####################

time = list( time.san.migel.panobinostat = 0:28, 
             time.san.migel.placebo = 0:28
)



################
#
# arm indicator
#
# 1 = van-cutsem.FTD
# 2 = van-cutsem.Placebo
#
################


##############
#
# The R functions preprocess.digitized.data (Program 1) 
# and Guyot.individual.data (Program 2) are given below. 
# Read them into R first. Then execute the commands below to get individual.data.
#
##############

individual.data = NULL
for(ifile in 1:length(digitized.file.names)){
  digitized.line = read.table(digitized.file.names[ifile], header=T)
  processed.line.data = preprocess.digitized.data(digitized.line, 
                                                  numbers.below.figure[[ifile]], 
                                                  time[[ifile]])
  individual.line.data = Guyot.individual.data(processed.line.data$condensed.data.set, 
                                               processed.line.data$nrisk.data, 
                                               input.arm.id=ifile)
  individual.data = rbind(individual.data, individual.line.data)
}

treatment.type = c(
  rep("Panobinostat", length(which(individual.data[,"tmt.arm.number"] == 1))),
  rep("Placebo", length(which(individual.data[,"tmt.arm.number"] == 2))))                   


individual.data = as.data.frame(individual.data)
individual.data$treatment.type = treatment.type


############
#
# The R object "individual.data" contains the digitized data set
#
############
