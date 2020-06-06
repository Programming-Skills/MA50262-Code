##################################
#
# PROGRAM 1: R Function preprocess.digitized.data
#
# R FUNCTION TO PREPROCESS THE DIGITIZED DATA BEFORE IT 
# CAN BE USED BY THE GUYOT R CODE (Program 2) TO GET INDIVIDUAL 
# LEVEL DATA
#
#
# The inputs to this function are: 
#
#       line.data = a matrix with 2 columns containing the time axis (column 1) 
#                         and survival probability (column 2) for the digitized line
#
#       num.below.fig = the numbers below the Kaplan-Meier survival curve, 
#                                 indicating the number at risk at each time point
#
#       time.points = integer values of time points of interest along the x-axis. 
#                            For example, if the figure shows x-axis up to 10, 
#                            set time.point = 0:10
#
#       max.surv.prob = maximum value of survival probability. 
#                                  If the data are digitized to get survival probability on the 0 to 100
#                                 scale, set max.surv.prob = 100. If the data are extracted in the 0 
#                                 to 1 scale of survival probability, set max.surv.prob = 1
#
#
##################################

preprocess.digitized.data = function(line.data, num.below.fig, time.points, max.surv.prob=100){

  condensed.data.set = NULL
  unique.surv.prob = unique(line.data[,2])

  ################
  # The digitized data typically will have a large number of x-axis values with the same y-axis value. 
  # The following commands handle this to give a clean data set with not too many repetitions 
  # of y-axis with different x-axis.
  ################

  for(i in 1:length(unique.surv.prob)){
    sub.data = line.data[which(line.data[,2]==unique.surv.prob[i]),]
    if(nrow(sub.data)==1){
      condensed.data.set = rbind(condensed.data.set, sub.data)
    }
    if(nrow(sub.data) > 1){
      condensed.data.set = rbind(condensed.data.set, sub.data[1,])
    }
  }
  ##############
  # create appropriate condensed.data.set so that as time increases, 
  #  survival is non-increasing
  ##############
  orig.condensed.data.set = condensed.data.set
  condensed.time = unique(orig.condensed.data.set[,1])
  new.condensed.data = NULL
  temp.max = max.surv.prob

  for(i in 1:length(condensed.time)){
    temp.time = condensed.time[i]
    temp.mat = orig.condensed.data.set[which(orig.condensed.data.set[,1] == temp.time),]
    if(nrow(temp.mat) > 1){
      temp.id = which(temp.mat[,2] <= temp.max)
      if(length(temp.id) > 0){
        temp.row = temp.mat[temp.id[1],]
        new.condensed.data = rbind(new.condensed.data, temp.row)
        temp.max = temp.row[2]
      }
    }
    else{
      temp.row = temp.mat
      if(temp.row[2] <= temp.max){
        new.condensed.data = rbind(new.condensed.data, temp.row)
        temp.max = temp.row[2]
      }
    }
  }
  condensed.data.set = new.condensed.data

  unique.condensed.time = unique(round(condensed.data.set[,1]))
  event.time = NULL
  temp.time = condensed.data.set[,1]

  ##################
  #
  # extract nrisk information
  #
  ##################
  lower <- NULL
  upper <- NULL

  for(i in 1:(length(time.points)-1)){
    my.time = time.points[i]
    my.id = which(temp.time >= my.time & temp.time < time.points[(i+1)])

    if(length(my.id) > 0){
      event.time = c(event.time, my.time)
      lower = c(lower, my.id[1])
      upper = c(upper, my.id[length(my.id)])
    }
  }

  nrisk.data = data.frame(t.risk=event.time, lower=lower, upper=upper, n.risk=num.below.fig[event.time+1])
  
  result = list(condensed.data.set=condensed.data.set, nrisk.data=nrisk.data)
  return(result)
}

