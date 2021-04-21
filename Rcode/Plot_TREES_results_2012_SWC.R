#
#Use this script to plot predawn leaf water potential and tYMD..MPa.nspiYMD..MPa.tion
# from the pinon simulations
#
#Note: This script assumes you have already run Aggregate_to_Daily.R 
#

#
#Read in midday simulation results
#
setwd(paste("~/Documents/research/manuscripts/maizeHydraulics/",sep=""))

subfolder <- "simulations/corn_leaf2012_V9/"

B73<-read.csv(paste(subfolder," B73 _midday",".csv",sep=""),header=TRUE)
B97<-read.csv(paste(subfolder," B97 _midday",".csv",sep=""),header=TRUE)
CML103<-read.csv(paste(subfolder," CML103 _midday",".csv",sep=""),header=TRUE)
CML322<-read.csv(paste(subfolder," CML322 _midday",".csv",sep=""),header=TRUE)
CML69<-read.csv(paste(subfolder," CML69 _midday",".csv",sep=""),header=TRUE)
KI11<-read.csv(paste(subfolder," KI11 _midday",".csv",sep=""),header=TRUE)
KY21<-read.csv(paste(subfolder," KY21 _midday",".csv",sep=""),header=TRUE)
MO17<-read.csv(paste(subfolder," MO17 _midday",".csv",sep=""),header=TRUE)
MO18W<-read.csv(paste(subfolder," MO18W _midday",".csv",sep=""),header=TRUE)
MS71<-read.csv(paste(subfolder," MS71 _midday",".csv",sep=""),header=TRUE)
NC350<-read.csv(paste(subfolder," NC350 _midday",".csv",sep=""),header=TRUE)
OH7B<-read.csv(paste(subfolder," OH7B _midday",".csv",sep=""),header=TRUE)
TX303<-read.csv(paste(subfolder," TX303 _midday",".csv",sep=""),header=TRUE)
LIRFmonsanto<-read.csv(paste(subfolder," LIRFmonsanto _midday",".csv",sep=""),header=TRUE)
Temperate<-read.csv(paste(subfolder," Temperate _midday",".csv",sep=""),header=TRUE)
Tropical<-read.csv(paste(subfolder," Tropical _midday",".csv",sep=""),header=TRUE)
swc<-read.csv(paste(subfolder,"swc2012",".csv",sep=""),header=TRUE)

plotTitle <- "Maize, LIRF 2012"

pdf("plots/maize_swc_2012_V9.pdf", width = 5, height= 9, useDingbats = F)

layout(matrix(1:4, ncol = 1, nrow=4), widths = rep(1, 4), heights = c(1.2,1.2,1.2,1.2), respect = FALSE)
par(oma=c(6.1,1.1,1.75,0.1))
par(mar = c(0.0, 4.8, 0.0, 2.1))

#Axis(side=1, labels=FALSE)

theta01 <- as.matrix(0.80*B73$theta0+0.20*B73$theta1)
plot(B73$DAY+134,theta01,type="l",lwd=1, lty=1, col="white", cex.axis=1.5, cex.lab=1.5, 
     ylim=c(0.05,0.32), ylab="SWC", xaxt='n')
mtext(plotTitle,side=3,cex=1.0, adj=0, line=0.25)
text(139,0.31,"e",cex=1.5)
theta01 <- as.matrix(0.8*Temperate$theta0+0.2*Temperate$theta1)
lines(Temperate$DAY+134,theta01, lwd=1, lty=1, col="skyblue")
theta01 <- as.matrix(0.8*Tropical$theta0+0.2*Tropical$theta1)
lines(Tropical$DAY+134,theta01, lwd=1, lty=1, col="wheat")
theta01 <- as.matrix(0.8*LIRFmonsanto$theta0+0.2*LIRFmonsanto$theta1)
lines(LIRFmonsanto$DAY+134,theta01, lwd=1, lty=1, col="darkred")
points(swc$DOY,swc$theta01+0.038, pch=1,col="black",cex=1.5,lwd=1.5)
box(lwd=1.5)

Axis(side=1, labels=FALSE)
plot(B73$DAY+134,B73$theta1,type="l",lwd=1, lty=1, col="white", cex.axis=1.5, cex.lab=1.5, 
     ylim=c(0.05,0.32), ylab="SWC", xaxt='n')
text(139,0.31,"f",cex=1.5)
lines(Temperate$DAY+134,Temperate$theta1, lwd=1, lty=1, col="skyblue")
lines(Tropical$DAY+134,Tropical$theta1, lwd=1, lty=1, col="wheat")
lines(LIRFmonsanto$DAY+134,LIRFmonsanto$theta1, lwd=1, lty=1, col="darkred")
points(swc$DOY,swc$theta2+0.038, pch=1,col="black",cex=1.5,lwd=1.5)
box(lwd=1.5)

Axis(side=1, labels=FALSE)
theta01 <- as.matrix(0.5*B73$theta2+0.5*B73$theta3)
plot(B73$DAY+134,theta01,type="l",lwd=1, lty=1, col="white", cex.axis=1.5, cex.lab=1.5, 
     ylim=c(0.05,0.32), ylab="SWC", xaxt='n')
text(139,0.31,"g",cex=1.5)
theta01 <- as.matrix(0.5*Temperate$theta2+0.5*Temperate$theta3)
lines(Temperate$DAY+134,theta01, lwd=1, lty=1, col="skyblue")
theta01 <- as.matrix(0.5*Tropical$theta2+0.5*Tropical$theta3)
lines(Tropical$DAY+134,theta01, lwd=1, lty=1, col="wheat")
theta01 <- as.matrix(0.5*LIRFmonsanto$theta2+0.5*LIRFmonsanto$theta3)
lines(LIRFmonsanto$DAY+134,theta01, lwd=1, lty=1, col="darkred")
points(swc$DOY,swc$theta3+0.038, pch=1,col="black",cex=1.5,lwd=1.5)
box(lwd=1.5)

Axis(side=1, labels=FALSE)
plot(B73$DAY+134,B73$theta4,type="l",lwd=1, lty=1, col="white", cex.axis=1.5, cex.lab=1.5, 
     ylim=c(0.05,0.32), ylab="SWC", xaxt='s')
text(139,0.31,"h",cex=1.5)
lines(Temperate$DAY+134,Temperate$theta4, lwd=1, lty=1, col="skyblue")
lines(Tropical$DAY+134,Tropical$theta4, lwd=1, lty=1, col="wheat")
lines(LIRFmonsanto$DAY+134,LIRFmonsanto$theta4, lwd=1, lty=1, col="darkred")
points(swc$DOY,swc$theta4+0.038, pch=1,col="black",cex=1.5,lwd=1.5)
mtext("Day", side=1, line=3, cex=1.0)
legend("bottomleft",lty=c(1,1,1),lwd=c(1.5,1.5,1.5),col=c("darkred","skyblue","wheat"),
       c("Hybrid","Mean temperate","Mean tropical"), cex=1.0, bty="n")
box(lwd=1.5)

dev.off()


