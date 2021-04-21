#
#Use this script to plot predawn leaf water potential and transpiration
# from the pinon simulations
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
              as.expression(bquote(italic(E)[C]~"or"~italic(E)[Crit]~"("~"mmol" ~s^-1~")" )),
              as.expression(bquote("Relative root area" )),
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
plotFile <- "plots/maize_genotypes_2013&2012_EC&YPD.pdf"

#subfolders <- c("simulations/corn_leaf2013_V9/")

plotLabels <- c("a","c","b","d")
noRefillOn <- c(175,186,182,189)

sapflux<-read.csv(paste("simulations/corn_leaf2013_V9/","sapflux2013_100",".csv",sep=""),header=TRUE)

subfolders <- c("simulations/corn_leaf2013_LT2/")
plotTitle <- "Maize, LIRF 2013 & 2012"
pdf(plotFile, width = 8, height= 9, useDingbats = F)
layout(matrix(1:3, ncol = 1, nrow=3), widths = rep(1, 3), heights = c(3.0,3.0,3.0), respect = FALSE)
par(oma=c(6.1, 2.0, 2.0, 1.0))

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
        
        #
        # Temperate = B73, B97, KY21, MS71, OH7B
        # Tropical = CML103, CML322, CML69, KI11, NC350
        #
        aggTemperateL <- as.matrix((1/6*B73$EC..mmol.s.1.)+(1/6*B97$EC..mmol.s.1.)+(1/6*KY21$EC..mmol.s.1.)+
                                           (1/6*MO17$EC..mmol.s.1.)+(1/6*MS71$EC..mmol.s.1.)+(1/6*OH7B$EC..mmol.s.1.))
        aggTropicalL <- as.matrix((0.2*CML103$EC..mmol.s.1.)+(0.2*CML322$EC..mmol.s.1.)+(0.2*CML69$EC..mmol.s.1.)+
                                          (0.2*KI11$EC..mmol.s.1.)+(0.2*NC350$EC..mmol.s.1.))
        
        minL <- as.matrix(pmin((B73$EC..mmol.s.1.),(B97$EC..mmol.s.1.),CML103$EC..mmol.s.1.,CML322$EC..mmol.s.1.,
                               CML69$EC..mmol.s.1.,KI11$EC..mmol.s.1.,KY21$EC..mmol.s.1.,MO17$EC..mmol.s.1.,
                               MO18W$EC..mmol.s.1.,MS71$EC..mmol.s.1.,NC350$EC..mmol.s.1.,OH7B$EC..mmol.s.1.,
                               TX303$EC..mmol.s.1.,LIRFmonsanto$EC..mmol.s.1.,Temperate$EC..mmol.s.1.,Tropical$EC..mmol.s.1.))
        
        maxL <- as.matrix(pmax(B73$EC..mmol.s.1.,B97$EC..mmol.s.1.,CML103$EC..mmol.s.1.,CML322$EC..mmol.s.1.,
                               CML69$EC..mmol.s.1.,KI11$EC..mmol.s.1.,KY21$EC..mmol.s.1.,MO17$EC..mmol.s.1.,
                               MO18W$EC..mmol.s.1.,MS71$EC..mmol.s.1.,NC350$EC..mmol.s.1.,OH7B$EC..mmol.s.1.,
                               TX303$EC..mmol.s.1.,LIRFmonsanto$EC..mmol.s.1.,Temperate$EC..mmol.s.1.,Tropical$EC..mmol.s.1.))
        #
        ylow <- -1.0
        yhigh <- 15.5
        if (i == 1 || i == 2) par(mar = c(0.0, 4.8, 0.0, 0.0))
        else par(mar = c(0.0, 0.0, 0.0, 4.8))
        if (i == 3 || i == 4) par(yaxt="n")
        else par(yaxt="s")
        if (i == 1 || i == 3) par(xaxt="n")
        else par(xaxt="s")
        plot(B73$DAY+151,B73$EC..mmol.s.1.,type="l",lwd=1.5, lty=1, col="white", cex.axis=2.0, cex.lab=2.0, 
             ylim=c(ylow,yhigh), ylab=exp_list[15], xlim=c(129,320))
        if (i==1) mtext(plotTitle,side=3,cex=1.5, adj=0, line=0.25)
        #if (i!=3) lines(c(noRefillOn[i],noRefillOn[i]),c(-1,25),lty=3, lwd=1.5, col="black")
        #else lines(c(noRefillOn[i],noRefillOn[i]),c(-1,10),lty=3, lwd=1.5, col="black")
        text(131,15,plotLabels[1], cex=2.0)
        #lines(Temperate$DAY+151,B73$EC..mmol.s.1., lwd=1.5, lty=1, col="red")
        lines(Temperate$DAY+151,Temperate$EC..mmol.s.1., lwd=1.5, lty=1, col="skyblue")
        lines(Tropical$DAY+151,Tropical$EC..mmol.s.1., lwd=1.5, lty=1, col="wheat")
        lines(Temperate$DAY+151,aggTemperateL, lwd=1.5, lty=1, col="skyblue3")
        lines(Tropical$DAY+151,aggTropicalL, lwd=1.5, lty=1, col="wheat3")
        lines(LIRFmonsanto$DAY+151,minL, lwd=1.5, lty=1, col="grey60")
        lines(LIRFmonsanto$DAY+151,maxL, lwd=1.5, lty=1, col="grey39")
        lines(LIRFmonsanto$DAY+151,LIRFmonsanto$EC..mmol.s.1., lwd=1.5, lty=1, col="darkred")
        points(sapflux$DOY,sapflux$Flux_mmol, pch=1,col="black",cex=1.25,lwd=1.25)
        if (i == 1)
        {
                legend("topright",lty=c(1,1,1,1,1,1,1),lwd=c(1.5,1.5,1.5,1.5,1.5,1.5,1.5),ncol=1,
                        col=c("skyblue","wheat","skyblue3","wheat3","darkred","grey60","grey39"),
                        c("Mean Temperate Inputs","Mean Tropical Inputs","Temperate Ensemble Mean",
                        "Tropical Ensemble Mean","Hybrid","Full Ensemble Minimum","Full Ensemble Maximum"), cex=1.0, bty="n")
        }
        text(220, 0, c("bias = -0.09; r = 0.72"), col="darkred", cex=1.5)
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
        aggTemperateL <- as.matrix((1/6*B73$YMD..MPa.)+(1/6*B97$YMD..MPa.)+(1/6*KY21$YMD..MPa.)+
                                           (1/6*MO17$YMD..MPa.)+(1/6*MS71$YMD..MPa.)+(1/6*OH7B$YMD..MPa.))
        aggTropicalL <- as.matrix((0.2*CML103$YMD..MPa.)+(0.2*CML322$YMD..MPa.)+(0.2*CML69$YMD..MPa.)+
                                          (0.2*KI11$YMD..MPa.)+(0.2*NC350$YMD..MPa.))
        
        minL <- as.matrix(pmin((B73$YMD..MPa.),(B97$YMD..MPa.),CML103$YMD..MPa.,CML322$YMD..MPa.,
                               CML69$YMD..MPa.,KI11$YMD..MPa.,KY21$YMD..MPa.,MO17$YMD..MPa.,
                               MO18W$YMD..MPa.,MS71$YMD..MPa.,NC350$YMD..MPa.,OH7B$YMD..MPa.,
                               TX303$YMD..MPa.,LIRFmonsanto$YMD..MPa.,Temperate$YMD..MPa.,Tropical$YMD..MPa.))
        
        maxL <- as.matrix(pmax(B73$YMD..MPa.,B97$YMD..MPa.,CML103$YMD..MPa.,CML322$YMD..MPa.,
                               CML69$YMD..MPa.,KI11$YMD..MPa.,KY21$YMD..MPa.,MO17$YMD..MPa.,
                               MO18W$YMD..MPa.,MS71$YMD..MPa.,NC350$YMD..MPa.,OH7B$YMD..MPa.,
                               TX303$YMD..MPa.,LIRFmonsanto$YMD..MPa.,Temperate$YMD..MPa.,Tropical$YMD..MPa.))
        #
        ylow <- -4.0
        yhigh <- 0.5
        if (i == 1 || i == 2) par(mar = c(0.0, 4.8, 0.0, 0.0))
        else par(mar = c(0.0, 0.0, 0.0, 4.8))
        if (i == 1 || i == 3) par(xaxt="n")
        else par(xaxt="s")
        plot(B73$DAY+151,B73$YMD..MPa.,type="l",lwd=1.5, lty=1, col="white", cex.axis=2.0, cex.lab=2.0, 
             ylim=c(ylow,yhigh), ylab=exp_list[1], xlim=c(129,320))
        #if (i==1) mtext(plotTitle,side=3,cex=1.25, adj=0, line=0.25)
        #if (i<4) lines(c(noRefillOn[i],noRefillOn[i]),c(-10,70),lty=3, lwd=1.5, col="black")
        #else lines(c(noRefillOn[i],noRefillOn[i]),c(-10,20),lty=3, lwd=1.5, col="black")
        text(131,0.25,plotLabels[3], cex=2.0)
        #lines(Temperate$DAY+151,B73$YMD..MPa., lwd=1.5, lty=1, col="red")
        lines(Temperate$DAY+151,Temperate$YMD..MPa., lwd=1.5, lty=1, col="skyblue")
        lines(Tropical$DAY+151,Tropical$YMD..MPa., lwd=1.5, lty=1, col="wheat")
        lines(Temperate$DAY+151,aggTemperateL, lwd=1.5, lty=1, col="skyblue3")
        lines(Tropical$DAY+151,aggTropicalL, lwd=1.5, lty=1, col="wheat3")
        lines(LIRFmonsanto$DAY+151,minL, lwd=1.5, lty=1, col="grey60")
        lines(LIRFmonsanto$DAY+151,maxL, lwd=1.5, lty=1, col="grey39")
        lines(LIRFmonsanto$DAY+151,LIRFmonsanto$YMD..MPa., lwd=1.5, lty=1, col="darkred")
        #plot C12 
        points(c(246),c(-1.63),
               pch=1,col="black",cex=1.25,lwd=1.5)
        #plot C22
        points(c(177,204,227,246),c(-1.38,-1.12,-1.29,-1.57),
               pch=1,col="black",cex=1.25,lwd=1.5)
        #plot D43
        points(c(177,204,246),c(-1.59,-1.44,-1.61),
               pch=1,col="black",cex=1.25,lwd=1.5)
        #plot D31
        points(c(204,227,246),c(-1.28,-1.21,-1.48),
               pch=1,col="black",cex=1.25,lwd=1.5)
        if (i == 4)
        {
                legend("bottomright",lty=c(1,1,1,1,1,1,1),lwd=c(1.5,1.5,1.5,1.5,1.5,1.5,1.5),
                       col=c("skyblue","wheat","skyblue3","wheat3","darkred","grey60","grey39"),
                       c("Mean Temperate Parameters","Mean Tropical Parameters","Temperate Ensemble Mean",
                         "Tropical Ensemble Mean","Hybrid","Full Ensemble Minimum","Full Ensemble Maximum"), cex=0.9, bty="n")
        }
        #if (i == 1 || i == 4) mtext("Yearday", side=1, line=3, cex=1.25)
        box(lwd=1.5)
        
        
        subfolders <- c("simulations/corn_leaf2012_LT2/")
        subfolder <- subfolders[1]
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
        #
        # Temperate = B73, B97, KY21, MS71, OH7B
        # Tropical = CML103, CML322, CML69, KI11, NC350
        #
        aggTemperateL <- as.matrix((1/6*B73$YMD..MPa.)+(1/6*B97$YMD..MPa.)+(1/6*KY21$YMD..MPa.)+
                                           (1/6*MO17$YMD..MPa.)+(1/6*MS71$YMD..MPa.)+(1/6*OH7B$YMD..MPa.))
        aggTropicalL <- as.matrix((0.2*CML103$YMD..MPa.)+(0.2*CML322$YMD..MPa.)+(0.2*CML69$YMD..MPa.)+
                                          (0.2*KI11$YMD..MPa.)+(0.2*NC350$YMD..MPa.))
        
        minL <- as.matrix(pmin((B73$YMD..MPa.),(B97$YMD..MPa.),CML103$YMD..MPa.,CML322$YMD..MPa.,
                               CML69$YMD..MPa.,KI11$YMD..MPa.,KY21$YMD..MPa.,MO17$YMD..MPa.,
                               MO18W$YMD..MPa.,MS71$YMD..MPa.,NC350$YMD..MPa.,OH7B$YMD..MPa.,
                               TX303$YMD..MPa.,LIRFmonsanto$YMD..MPa.,Temperate$YMD..MPa.,Tropical$YMD..MPa.))
        
        maxL <- as.matrix(pmax(B73$YMD..MPa.,B97$YMD..MPa.,CML103$YMD..MPa.,CML322$YMD..MPa.,
                               CML69$YMD..MPa.,KI11$YMD..MPa.,KY21$YMD..MPa.,MO17$YMD..MPa.,
                               MO18W$YMD..MPa.,MS71$YMD..MPa.,NC350$YMD..MPa.,OH7B$YMD..MPa.,
                               TX303$YMD..MPa.,LIRFmonsanto$YMD..MPa.,Temperate$YMD..MPa.,Tropical$YMD..MPa.))
        #
        ylow <- -4.0
        yhigh <- 0.5
        if (i == 1 || i == 2) par(mar = c(0.0, 4.8, 0.0, 0.0))
        else par(mar = c(0.0, 0.0, 0.0, 4.8))
        if (i == 3 || i == 4) par(yaxt="n")
        else par(yaxt="s")
        if (i == 2 || i == 3) par(xaxt="n")
        else par(xaxt="s")
        plot(B73$DAY+134,B73$YMD..MPa.,type="l",lwd=1.5, lty=1, col="white", cex.axis=2.0, cex.lab=2.0, 
             ylim=c(ylow,yhigh), ylab=exp_list[1], xlim=c(129,320))
        #if (i==1) mtext(plotTitle,side=3,cex=1.25, adj=0, line=0.25)
        #if (i<4) lines(c(noRefillOn[i],noRefillOn[i]),c(-10,70),lty=3, lwd=1.5, col="black")
        #else lines(c(noRefillOn[i],noRefillOn[i]),c(-10,20),lty=3, lwd=1.5, col="black")
        text(131,0.25,plotLabels[2], cex=2.0)
        #lines(Temperate$DAY+151,B73$YMD..MPa., lwd=1.5, lty=1, col="red")
        lines(Temperate$DAY+134,Temperate$YMD..MPa., lwd=1.5, lty=1, col="skyblue")
        lines(Tropical$DAY+134,Tropical$YMD..MPa., lwd=1.5, lty=1, col="wheat")
        lines(Temperate$DAY+134,aggTemperateL, lwd=1.5, lty=1, col="skyblue3")
        lines(Tropical$DAY+134,aggTropicalL, lwd=1.5, lty=1, col="wheat3")
        lines(LIRFmonsanto$DAY+134,minL, lwd=1.5, lty=1, col="grey60")
        lines(LIRFmonsanto$DAY+134,maxL, lwd=1.5, lty=1, col="grey39")
        lines(LIRFmonsanto$DAY+134,LIRFmonsanto$YMD..MPa., lwd=1.5, lty=1, col="darkred")
        #plot A33 
        points(c(165,184,204,222,239,247),c(-1.89,-1.73,-1.86,-1.45,-2.35,-2.34),
               pch=1,col="black",cex=1.5,lwd=1.5)
        #plot A42
        points(c(165,184,204,222,239,247),c(-1.63,-1.61,-1.85,-1.27, NA,-2.4),
               pch=1,col="black",cex=1.5,lwd=1.5)
        #plot B11
        points(c(165,184,204,222,239,247),c(-1.23,-1.29,-1.70,-1.69,NA,-2.35),
               pch=1,col="black",cex=1.5,lwd=1.5)
        #plot B23
        points(c(165,184,204,222,239,247),c(-1.7,-1.66,-1.67,-1.47,-1.52,-2.44),
               pch=1,col="black",cex=1.5,lwd=1.5)
        if (i == 4)
        {
                legend("bottomright",lty=c(1,1,1,1,1,1,1),lwd=c(1.5,1.5,1.5,1.5,1.5,1.5,1.5),
                       col=c("skyblue","wheat","skyblue3","wheat3","darkred","grey60","grey39"),
                       c("Mean Temperate Parameters","Mean Tropical Parameters","Temperate Ensemble Mean",
                         "Tropical Ensemble Mean","Hybrid","Full Ensemble Minimum","Full Ensemble Maximum"), cex=0.9, bty="n")
        }
        if (i == 1 || i == 4) mtext("Yearday", side=1, line=3, cex=1.5)
        box(lwd=1.5)
}

dev.off()

LIRFmonsanto<-read.csv(paste("simulations/corn_leaf2013_LT2/"," LIRFmonsanto _midday",".csv",sep=""),header=TRUE)
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


