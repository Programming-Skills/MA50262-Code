###################
#
# Read program-1.R and program-2.R using the source function
#
################### 

source("program-1.R")
source("program-2.R")


#############
#
# First, digitize each line in Figure 2C of Mok et al (2016) 
# to obtain the (x,y) coordinates for each line that correspond to times and 
# survival probabilities. 
# 
# For each line, this will result in a matrix with 2 columns. 
# Column 1 is time and Column 2 is survival probability for that time. 
# This will be given for various time points. 
#
# This will result in a total of 2 such data files, one file per line. 
#
# The R object digitized.file.names, given below, contains the names of these files. 
#
##############


digitized.file.names = c("ferris.a.nivolumab.tsv", "ferris.standard.tsv")


################################
#
# Now, look below Figures 2C of Mok et al and write down the 
# number at risk data given for various time points for each line. 
#
# The list numbers.below.figure, shown below, contains these data. 
#
################################

numbers.below.figure = list(  
  ferris.a.nivolumab = c(88, 67, 44, 18, 6),
  ferris.standard = c(61, 42, 20, 6, 2)
  # ferris.a.nivolumab = c(88, 67, 44, 18, 6,0,0,0,0,0,0,0,0,0,0,0),
  # ferris.standard = c(61, 42, 20, 6, 2, 0,0,0,0,0,0,0,0,0,0,0)
  
)


####################
#
# Now, specify how far along on the x-axis of each line we want to go to extract data. 
# For example, in one line we may want to go up to time 15 units, 
# in another line up to time 18 units etc. 
# 
# As above, organize these times (integer values) for each line in the same order 
# as the sheets in the excel file. 
#
####################

time = list( time.ferris.a.nivolumab = 0:17, 
             time.ferris.standard  = 0:14
)



################
#
# arm indicator
#
# 1 = ferris.a.nivolumab
# 2 = ferris.standard
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
  rep("Nivolumab", length(which(individual.data[,"tmt.arm.number"] == 1))),
  rep("Standard therapy", length(which(individual.data[,"tmt.arm.number"] == 2))))                   


individual.data = as.data.frame(individual.data)
individual.data$treatment.type = treatment.type


############
#
# The R object "individual.data" contains the digitized data set
#
############
