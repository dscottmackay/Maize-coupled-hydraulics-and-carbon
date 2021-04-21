#
#TREES workshop plotting routines
#  This script is used first to convert half-hourly simulation output to daily
#

#
#run these lines to create daily files for the four half-hourly simulations
#
setwd(paste("~/Documents/research/manuscripts/maizeHydraulics/",sep=""))
source("plotting_scripts/TREES_aggregation_function.R")
drivers<- read.table(paste("simulations/corn_leaf2012_V9/","LIRFcorn_2012_grow_irrigated",".txt",sep=""), header=TRUE)

drivers<- read.table(paste("simulations/corn_leaf2013_R1/","LIRFcorn_2013_grow_irrigated",".txt",sep=""), header=TRUE)

SLA <- 50.0 #specific leaf area, maize

subfolder <- "LIRF_sims/leaf2013_R5/"

subfolder <- "simulations/corn_leaf2013_R1/"
subfolder <- "simulations/corn_leaf2013_R1_v2/"
subfolder <- "simulations/corn_leaf2013_R1_SLA/"

subfolder <- "simulations/corn_leaf2013_V10/"
subfolder <- "simulations/corn_leaf2013_V9/"
subfolder <- "simulations/corn_leaf2013_V8/"
subfolder <- "simulations/corn_leaf2013_V7/"

subfolder <- "simulations/corn_leaf2012_V9/"
subfolder <- "simulations/corn_leaf2012_LT2/"


#B73 curve
fname <- "B73"
Ksat <- 4.22/(-0.3+1.5) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "B97"
Ksat <- 4.22/(-0.3+1.65) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "CML103"
Ksat <- 4.22/(-0.3+1.57) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "CML322"
Ksat <- 4.22/(-0.3+1.49) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "CML69"
Ksat <- 4.22/(-0.3+1.66) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "KI11"
Ksat <- 4.22/(-0.3+1.9) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "KY21"
Ksat <- 4.22/(-0.3+1.75) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "MO17"
Ksat <- 4.22/(-0.3+2.02) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
#MO18W curve
fname <- "MO18W"
Ksat <- 4.22/(-0.3+2.03) #whole-plant saturated hydraulic conductance, maize MO18W
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "MS71"
Ksat <- 4.22/(-0.3+1.4) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "NC350"
Ksat <- 4.22/(-0.3+1.69) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "OH7B"
Ksat <- 4.22/(-0.3+1.76) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "TX303"
Ksat <- 4.22/(-0.3+1.55) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "LIRFmonsanto"
Ksat <- 4.22/(-0.3+2.01) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "Temperate"
Ksat <- 4.22/(-0.3+1.61) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "Tropical"
Ksat <- 4.22/(-0.3+1.66) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
#
#

#mixed genotypes
subfolder <- "LIRF_sims/mixed/"

#B73 curve
fname <- "B73high"
Ksat <- 4.22/(-0.3+1.3) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "CML322low"
Ksat <- 4.22/(-0.3+1.5) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
fname <- "MO17high"
Ksat <- 4.22/(-0.3+1.8) #whole-plant saturated hydraulic conductance, maize B73
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)
#MO18W curve
fname <- "MO18Wlow"
Ksat <- 4.22/(-0.3+2.4) #whole-plant saturated hydraulic conductance, maize MO18W
simulation<-read.table(paste(subfolder,fname,".sim",sep=""),header=TRUE)
computeDaily(subfolder, fname, simulation, drivers, Ksat)

