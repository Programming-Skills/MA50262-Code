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


digitized.file.names = c("reck-et-al.ABCP.tsv", "reck-et-al.BCP.tsv")


################################
#
# The list numbers.below.figure, shown below, contains numbers at risk data. 
#
################################

numbers.below.figure = list(  
  reck.ABCP = c(356, 332, 311, 298, 290, 265, 232, 210, 186, 151, 124, 111, 87, 77, 58, 55, 42, 39, 27, 24, 16, 12, 4, 3, 2, 2, 2),    
  reck.BCP = c(336, 321, 292, 261, 243, 215, 179, 147, 125, 91, 69, 55, 39, 32, 21, 18, 12, 9, 7, 6, 3, 2, 1, 1)
  
)


####################
#
# Now, specify how far along on the x-axis of each line we want to go to extract data. 
#
####################

time = list( time.reck.ABCP = 0:27, 
             time.reck.BCP = 0:24
)



################
#
# arm indicator
#
# 1 = reck.ABCP
# 2 = reck.BCP
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
  rep("ABCP", length(which(individual.data[,"tmt.arm.number"] == 1))),
  rep("BCP", length(which(individual.data[,"tmt.arm.number"] == 2))))                   


individual.data = as.data.frame(individual.data)
individual.data$treatment.type = treatment.type


############
#
# The R object "individual.data" contains the digitized data set
#
############
