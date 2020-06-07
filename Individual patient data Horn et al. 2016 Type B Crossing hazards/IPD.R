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


digitized.file.names = c("horn-et-al-atezo.tsv", "horn-et-al-placebo.tsv")


################################
#
# The list numbers.below.figure, shown below, contains numbers at risk data. 
#
################################

numbers.below.figure = list(  
  horn.atezo = c(201, 191, 187, 182, 180, 174, 159, 142, 130, 121, 108, 92, 74, 58, 46, 33, 21, 11, 5, 3, 2, 10),    
  horn.placebo = c(202, 194, 189, 186, 183, 171, 160, 146, 131, 114, 96, 81, 59, 36, 27, 21, 13, 8, 3, 3, 2, 2)
  
)


####################
#
# Now, specify how far along on the x-axis of each line we want to go to extract data. 
#
####################

time = list( time.horn.atezo = 0:23, 
             time.horn.placebo = 0:23
)



################
#
# arm indicator
#
# 1 = horn.atezo
# 2 = horn.placebo
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
  rep("Atezolizumab", length(which(individual.data[,"tmt.arm.number"] == 1))),
  rep("Placebo", length(which(individual.data[,"tmt.arm.number"] == 2))))                   


individual.data = as.data.frame(individual.data)
individual.data$treatment.type = treatment.type


############
#
# The R object "individual.data" contains the digitized data set
#
############
