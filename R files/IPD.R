###################
#
# Read program-1.R and program-2.R using the source function
#
################### 

source("program-1.R")
source("program-2.R")


#############
#
# First, digitize each line in Figure 1B and 1C of Larkin et al (2015, PMID: 26027431) 
# to obtain the (x,y) coordinates for each line that correspond to times and 
# survival probabilities. This is described in Step 2 of the manuscript
# 
# For each line, this will result in a matrix with 2 columns. 
# Column 1 is time and Column 2 is survival probability for that time. 
# This will be given for various time points. 
#
# This will result in a total of 6 such data files, one file per line. 
# There are 3 lines in Figure 1B and 3 in Figure 1C or Larkin et al. Hence, 6 files. 
# 
# We have named 6 these files as follows: 
# 
# pdl1-negative-nivo.txt, pdl1-negative-ipi.txt, pdl1-negative-combo.txt,
# pdl1-positive-nivo.txt, pdl1-positive-ipi.txt, pdl1-positive-combo.txt
# 
# The R object digitized.file.names, given below, contains the names of these files. 
#
##############


digitized.file.names = c("First-paper-FTD.tsv", "First-paper-Placebo.tsv")


################################
#
# Now, look below Figures 1B and 1C of Larkin et al and write down the 
# number at risk data given for various time points for each line. 
#
# The list numbers.below.figure, shown below, contains these data. 
#
################################

numbers.below.figure = list(  
  First.paper.FTD = c(534, 521, 499, 459, 406, 355, 308, 267, 
                         231, 212, 180, 156, 137, 117, 95, 74, 59, 49, 38, 29, 20, 17, 14, 12, 10, 7, 4, 1),    
  First.paper.Placebo = c(266, 259, 232, 198, 163, 137, 114, 
                          94, 71, 62, 56, 51, 43, 36, 27, 21, 16, 15, 14, 10, 8, 7, 6, 6, 4, 3, 1, 1)
  
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

time = list( time.First.paper.FTD = 0:28, 
             time.First.paper.Placebo = 0:28
)



################
#
# arm indicator
#
# 1 = pdl1-neg-nivo
# 2 = pdl1-neg-ipi
# 3 = pdl1-neg-combo
# 4 = pdl1-pos-nivo
# 5 = pdl1-pos-ipi
# 6 = pdl1-pos-combo
#
################


##############
#
# The R functions preprocess.digitized.data (Program 1) 
# and Guyot.individual.data (Program 2) are given below. 
# Read them into R first. Then execute the commands below to get individual.data.
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
  rep("FTD plus tipiracil", length(which(individual.data[,"tmt.arm.number"] == 1))),
  rep("Placebo", length(which(individual.data[,"tmt.arm.number"] == 2))))                   


individual.data = as.data.frame(individual.data)
individual.data$treatment.type = treatment.type


############
#
# The R object "individual.data" contains the digitized data set
#
############
