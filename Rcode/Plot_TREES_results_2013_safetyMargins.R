#
#Use this script to plot transpiration safety margins
# 
#
#Note: This script assumes you have already run Aggregate_to_Daily.R 
#

#
#Read in midday simulation results
#
setwd(paste("~/Documents/research/manuscripts/maizeHydraulics/",sep=""))

#
#Define some y-axis labels
#
exp_list <- c(as.expression(bquote(Psi[MD]~"(MPa)" )),
              as.expression(bquote(italic(E)[Crit]~" - "~italic(E)[C]~" ("~"mmol"~m^-2 ~"gnd" ~s^-1~")" )),
              as.expression(bquote("("~italic(E)[Crit]~" - "~italic(E)[C]~") / "~italic(E)[Crit] )),
              as.expression(bquote("Fine root C (gC "~m^-2 ~")" )),
              as.expression(bquote("LAI ("~m^2 ~m^-2~")" )),
              as.expression(bquote("Root Area Index ("~m^2 ~m^-2~")" )),
              as.expression(bquote("SLA ("~m^2 ~kgC^-1~")" )),
              as.expression(bquote("Reproduction (gC "~m^-2 ~")" )),
              as.expression(bquote("Plant N (gN "~m^-2 ~")" )),
              as.expression(bquote("NSC (gC "~m^-2 ~")" )),
              as.expression(bquote(italic(E)[C]~"or"~italic(F)[Rhiz]~"("~"mmol" ~s^-1~")" )),
              as.expression(bquote(Psi[Leaf]~"(MPa)" )),
              as.expression(bquote("PLC (%)")),
              as.expression(bquote("GPP (umol "~m^-2 ~"gnd" ~s^-1 ~")" )),
              as.expression(bquote(italic(E)[C] ~" (mmol "~m^-2 ~"gnd" ~s^-1 ~")" )))
#
#Plot PLC curves, midday water potential, and GPP for 13 genotypes of maize
#
plotFile <- "plots/maize_genotypes_2013_safety.pdf"
plotTitle <- "Maize, LIRF 2013"
subfolders <- c("simulations/corn_leaf2013_V9/")
subfolders <- c("simulations/corn_leaf2013_LT2/")
plotLabels <- c("a","c","b","d")
noRefillOn <- c(175,186,182,189)

pdf(plotFile, width = 8, height= 8, useDingbats = F)
layout(matrix(1:2, ncol = 1, nrow=2), widths = rep(1, 2), heights = c(4,4), respect = FALSE)
par(oma=c(6.1, 1.0, 2.0, 1.0))

for(i in 1:length(subfolders))
{
        subfolder <- subfolders[i]
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
        
        B73$safety <- as.matrix((B73$ECRIT..mmol.s.1.)-(B73$EC..mmol.s.1.))
        B97$safety <- as.matrix((B97$ECRIT..mmol.s.1.)-(B97$EC..mmol.s.1.))
        CML103$safety <- as.matrix((CML103$ECRIT..mmol.s.1.)-(CML103$EC..mmol.s.1.))
        CML322$safety <- as.matrix((CML322$ECRIT..mmol.s.1.)-(CML322$EC..mmol.s.1.))
        CML69$safety <- as.matrix((CML69$ECRIT..mmol.s.1.)-(CML69$EC..mmol.s.1.))
        KI11$safety <- as.matrix((KI11$ECRIT..mmol.s.1.)-(KI11$EC..mmol.s.1.))
        KY21$safety <- as.matrix((KY21$ECRIT..mmol.s.1.)-(KY21$EC..mmol.s.1.))
        MO17$safety <- as.matrix((MO17$ECRIT..mmol.s.1.)-(MO17$EC..mmol.s.1.))
        MO18W$safety <- as.matrix((MO18W$ECRIT..mmol.s.1.)-(MO18W$EC..mmol.s.1.))
        MS71$safety <- as.matrix((MS71$ECRIT..mmol.s.1.)-(MS71$EC..mmol.s.1.))
        NC350$safety <- as.matrix((NC350$ECRIT..mmol.s.1.)-(NC350$EC..mmol.s.1.))
        OH7B$safety <- as.matrix((OH7B$ECRIT..mmol.s.1.)-(OH7B$EC..mmol.s.1.))
        TX303$safety <- as.matrix((TX303$ECRIT..mmol.s.1.)-(TX303$EC..mmol.s.1.))
        LIRFmonsanto$safety <- as.matrix((LIRFmonsanto$ECRIT..mmol.s.1.)-(LIRFmonsanto$EC..mmol.s.1.))
        Temperate$safety <- as.matrix((Temperate$ECRIT..mmol.s.1.)-(Temperate$EC..mmol.s.1.))
        Tropical$safety <- as.matrix((Tropical$ECRIT..mmol.s.1.)-(Tropical$EC..mmol.s.1.))
        
        B73$relsafety <- as.matrix((B73$safety)/(B73$ECRIT..mmol.s.1.))
        B97$relsafety <- as.matrix((B97$safety)/(B97$ECRIT..mmol.s.1.))
        CML103$relsafety <- as.matrix((CML103$safety)/(CML103$ECRIT..mmol.s.1.))
        CML322$relsafety <- as.matrix((CML322$safety)/(CML322$ECRIT..mmol.s.1.))
        CML69$relsafety <- as.matrix((CML69$safety)/(CML69$ECRIT..mmol.s.1.))
        KI11$relsafety <- as.matrix((KI11$safety)/(KI11$ECRIT..mmol.s.1.))
        KY21$relsafety <- as.matrix((KY21$safety)/(KY21$ECRIT..mmol.s.1.))
        MO17$relsafety <- as.matrix((MO17$safety)/(MO17$ECRIT..mmol.s.1.))
        MO18W$relsafety <- as.matrix((MO18W$safety)/(MO18W$ECRIT..mmol.s.1.))
        MS71$relsafety <- as.matrix((MS71$safety)/(MS71$ECRIT..mmol.s.1.))
        NC350$relsafety <- as.matrix((NC350$safety)/(NC350$ECRIT..mmol.s.1.))
        OH7B$relsafety <- as.matrix((OH7B$safety)/(OH7B$ECRIT..mmol.s.1.))
        TX303$relsafety <- as.matrix((TX303$safety)/(TX303$ECRIT..mmol.s.1.))
        LIRFmonsanto$relsafety <- as.matrix((LIRFmonsanto$safety)/(LIRFmonsanto$ECRIT..mmol.s.1.))
        Temperate$relsafety <- as.matrix((Temperate$safety)/(Temperate$ECRIT..mmol.s.1.))
        Tropical$relsafety <- as.matrix((Tropical$safety)/(Tropical$ECRIT..mmol.s.1.))
        
        #
        # Temperate = B73, B97, KY21, MS71, OH7B
        # Tropical = CML103, CML322, CML69, KI11, NC350
        #
        aggTemperateL <- as.matrix((1/6*B73$safety)+(1/6*B97$safety)+(1/6*KY21$safety)+
                                           (1/6*MO17$safety)+(1/6*MS71$safety)+(1/6*OH7B$safety))
        aggTropicalL <- as.matrix((0.2*CML103$safety)+(0.2*CML322$safety)+(0.2*CML69$safety)+
                                          (0.2*KI11$safety)+(0.2*NC350$safety))
        
        minL <- as.matrix(pmin((B73$safety),(B97$safety),CML103$safety,CML322$safety,
                               CML69$safety,KI11$safety,KY21$safety,MO17$safety,
                               MO18W$safety,MS71$safety,NC350$safety,OH7B$safety,
                               TX303$safety,LIRFmonsanto$safety,Temperate$safety,Tropical$safety))
        
        maxL <- as.matrix(pmax(B73$safety,B97$safety,CML103$safety,CML322$safety,
                               CML69$safety,KI11$safety,KY21$safety,MO17$safety,
                               MO18W$safety,MS71$safety,NC350$safety,OH7B$safety,
                               TX303$safety,LIRFmonsanto$safety,Temperate$safety,Tropical$safety))
        #
        ylow <- -1.0
        yhigh <- 30.0
        if (i == 1 || i == 2) par(mar = c(0.0, 4.8, 0.0, 0.0))
        else par(mar = c(0.0, 0.0, 0.0, 4.8))
        if (i == 3 || i == 4) par(yaxt="n")
        else par(yaxt="s")
        if (i == 1 || i == 3) par(xaxt="n")
        else par(xaxt="s")
        plot(B73$DAY+151,B73$safety,type="l",lwd=1.5, lty=1, col="white", cex.axis=1.25, cex.lab=1.25, 
             ylim=c(ylow,yhigh), ylab=exp_list[2], xlim=c(145,320))
        if (i==1) mtext(plotTitle,side=3,cex=1.25, adj=0, line=0.25)
        #if (i!=3) lines(c(noRefillOn[i],noRefillOn[i]),c(-1,25),lty=3, lwd=1.5, col="black")
        #else lines(c(noRefillOn[i],noRefillOn[i]),c(-1,10),lty=3, lwd=1.5, col="black")
        text(148,28,plotLabels[1], cex=1.5)
        #lines(Temperate$DAY+151,B73$safety, lwd=1.5, lty=1, col="red")
        lines(Temperate$DAY+151,Temperate$safety, lwd=1.5, lty=1, col="skyblue")
        lines(Tropical$DAY+151,Tropical$safety, lwd=1.5, lty=1, col="wheat")
        lines(Temperate$DAY+151,aggTemperateL, lwd=1.5, lty=1, col="skyblue3")
        lines(Tropical$DAY+151,aggTropicalL, lwd=1.5, lty=1, col="wheat3")
        lines(LIRFmonsanto$DAY+151,minL, lwd=1.5, lty=1, col="grey60")
        lines(LIRFmonsanto$DAY+151,maxL, lwd=1.5, lty=1, col="grey39")
        lines(LIRFmonsanto$DAY+151,LIRFmonsanto$safety, lwd=1.5, lty=1, col="darkred")
        if (i == 4)
        {
                legend("topright",lty=c(1,1,1,1,1,1,1),lwd=c(1.5,1.5,1.5,1.5,1.5,1.5,1.5),ncol=1,
                        col=c("skyblue","wheat","skyblue3","wheat3","darkred","grey60","grey39"),
                        c("Mean Temperate Inputs","Mean Tropical Inputs","Temperate Ensemble Mean",
                        "Tropical Ensemble Mean","Hybrid","Full Ensemble Minimum","Full Ensemble Maximum"), cex=0.8, bty="n")
        }
        box(lwd=1.5)
        
        for (j in 1:length(B73$DAY))
        {
                if (B73$YMD..MPa.[j]==0) B73$YMD..MPa.[j]=B73$YMD..MPa.[j-1]
                if (B97$YMD..MPa.[j]==0) B97$YMD..MPa.[j]=B97$YMD..MPa.[j-1]
                if (CML103$YMD..MPa.[j]==0) CML103$YMD..MPa.[j]=CML103$YMD..MPa.[j-1]
                if (CML322$YMD..MPa.[j]==0) CML322$YMD..MPa.[j]=CML322$YMD..MPa.[j-1]
                if (CML69$YMD..MPa.[j]==0) CML69$YMD..MPa.[j]=CML69$YMD..MPa.[j-1]
                if (KI11$YMD..MPa.[j]==0) KI11$YMD..MPa.[j]=KI11$YMD..MPa.[j-1]
                if (KY21$YMD..MPa.[j]==0) KY21$YMD..MPa.[j]=KY21$YMD..MPa.[j-1]
                if (MO17$YMD..MPa.[j]==0) MO17$YMD..MPa.[j]=MO17$YMD..MPa.[j-1]
                if (MO18W$YMD..MPa.[j]==0) MO18W$YMD..MPa.[j]=MO18W$YMD..MPa.[j-1]
                if (MS71$YMD..MPa.[j]==0) MS71$YMD..MPa.[j]=MS71$YMD..MPa.[j-1]
                if (NC350$YMD..MPa.[j]==0) NC350$YMD..MPa.[j]=NC350$YMD..MPa.[j-1]
                if (OH7B$YMD..MPa.[j]==0) OH7B$YMD..MPa.[j]=OH7B$YMD..MPa.[j-1]
                if (TX303$YMD..MPa.[j]==0) TX303$YMD..MPa.[j]=TX303$YMD..MPa.[j-1]
                if (LIRFmonsanto$YMD..MPa.[j]==0) LIRFmonsanto$YMD..MPa.[j]=LIRFmonsanto$YMD..MPa.[j-1]
                if (Temperate$YMD..MPa.[j]==0) Temperate$YMD..MPa.[j]=Temperate$YMD..MPa.[j-1]
                if (Tropical$YMD..MPa.[j]==0) Tropical$YMD..MPa.[j]=Tropical$YMD..MPa.[j-1]
        }
        
        
        #
        # Temperate = B73, B97, KY21, MS71, OH7B
        # Tropical = CML103, CML322, CML69, KI11, NC350
        #
        aggTemperateL <- as.matrix((1/6*B73$relsafety)+(1/6*B97$relsafety)+(1/6*KY21$relsafety)+
                                           (1/6*MO17$relsafety)+(1/6*MS71$relsafety)+(1/6*OH7B$relsafety))
        aggTropicalL <- as.matrix((0.2*CML103$relsafety)+(0.2*CML322$relsafety)+(0.2*CML69$relsafety)+
                                          (0.2*KI11$relsafety)+(0.2*NC350$relsafety))
        
        minL <- as.matrix(pmin((B73$relsafety),(B97$relsafety),CML103$relsafety,CML322$relsafety,
                               CML69$relsafety,KI11$relsafety,KY21$relsafety,MO17$relsafety,
                               MO18W$relsafety,MS71$relsafety,NC350$relsafety,OH7B$relsafety,
                               TX303$relsafety,LIRFmonsanto$relsafety,Temperate$relsafety,Tropical$relsafety))
        
        maxL <- as.matrix(pmax(B73$relsafety,B97$relsafety,CML103$relsafety,CML322$relsafety,
                               CML69$relsafety,KI11$relsafety,KY21$relsafety,MO17$relsafety,
                               MO18W$relsafety,MS71$relsafety,NC350$relsafety,OH7B$relsafety,
                               TX303$relsafety,LIRFmonsanto$relsafety,Temperate$relsafety,Tropical$relsafety))
        #
        ylow <- -0.1
        yhigh <- 1.1
        if (i == 1 || i == 2) par(mar = c(0.0, 4.8, 0.0, 0.0))
        else par(mar = c(0.0, 0.0, 0.0, 4.8))
        if (i == 3 || i == 4) par(yaxt="n")
        else par(yaxt="s")
        if (i == 2 || i == 3) par(xaxt="n")
        else par(xaxt="s")
        plot(B73$DAY+151,B73$relsafety,type="l",lwd=1.5, lty=1, col="white", cex.axis=1.25, cex.lab=1.25, 
             ylim=c(ylow,yhigh), ylab=exp_list[3], xlim=c(145,320))
        #if (i==1) mtext(plotTitle,side=3,cex=1.25, adj=0, line=0.25)
        #if (i<4) lines(c(noRefillOn[i],noRefillOn[i]),c(-10,70),lty=3, lwd=1.5, col="black")
        #else lines(c(noRefillOn[i],noRefillOn[i]),c(-10,20),lty=3, lwd=1.5, col="black")
        text(148,1,plotLabels[3], cex=1.5)
        #lines(Temperate$DAY+151,B73$relsafety, lwd=1.5, lty=1, col="red")
        lines(Temperate$DAY+151,Temperate$relsafety, lwd=1.5, lty=1, col="skyblue")
        lines(Tropical$DAY+151,Tropical$relsafety, lwd=1.5, lty=1, col="wheat")
        lines(Temperate$DAY+151,aggTemperateL, lwd=1.5, lty=1, col="skyblue3")
        lines(Tropical$DAY+151,aggTropicalL, lwd=1.5, lty=1, col="wheat3")
        lines(LIRFmonsanto$DAY+151,minL, lwd=1.5, lty=1, col="grey60")
        lines(LIRFmonsanto$DAY+151,maxL, lwd=1.5, lty=1, col="grey39")
        lines(LIRFmonsanto$DAY+151,LIRFmonsanto$relsafety, lwd=1.5, lty=1, col="darkred")
        
        if (i == 1)
        {
                legend("bottomright",lty=c(1,1,1,1,1,1,1),lwd=c(1.5,1.5,1.5,1.5,1.5,1.5,1.5),
                       col=c("skyblue","wheat","skyblue3","wheat3","darkred","grey60","grey39"),
                       c("Mean Temperate Parameters","Mean Tropical Parameters","Temperate Ensemble Mean",
                         "Tropical Ensemble Mean","Hybrid","Full Ensemble Minimum","Full Ensemble Maximum"), cex=0.9, bty="n")
        }
        if (i == 1 || i == 4) mtext("Yearday", side=1, line=3, cex=1.25)
        box(lwd=1.5)
}

dev.off()

LIRFmonsanto<-read.csv(paste("simulations/corn_leaf2013_V9/"," LIRFmonsanto _midday",".csv",sep=""),header=TRUE)
test <- sapflux$DOY-151
simEC <- array(data=0,dim=c(length(test),1))
for (i in 1:length(test))
{
        j <- test[i]
        simEC[i] <- LIRFmonsanto$EC..mmol.s.1.[j]
}
plot(sapflux$Flux_mmol)
lines(simEC)
cor(simEC,sapflux$Flux_mmol)
mean(sapflux$Flux_mmol)
mean(simEC)
bias <- mean(simEC)-mean(sapflux$Flux_mmol)
plot(simEC,sapflux$Flux_mmol)


