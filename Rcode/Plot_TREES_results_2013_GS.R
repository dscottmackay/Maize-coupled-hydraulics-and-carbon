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
              as.expression(bquote(italic(G)[W] ~" (mmol "~m^-2  ~s^-1 ~")" )))
#
#Plot PLC curves, midday water potential, and GPP for 13 genotypes of maize
#
plotFile <- "plots/maize_genotypes_2013_GS.pdf"
plotTitle <- "Maize, LIRF 2013"
subfolders <- c("simulations/corn_leaf2013_V7/","simulations/corn_leaf2013_V9/",
                "simulations/corn_leaf2013_V8/","simulations/corn_leaf2013_V10/")
plotLabels <- c("a","c","b","d")
noRefillOn <- c(175,186,182,189)

pdf(plotFile, width = 8, height= 6, useDingbats = F)
layout(matrix(1:4, ncol = 2, nrow=2), widths = rep(1, 2), heights = c(3,3), respect = FALSE)
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
        
        #
        # Temperate = B73, B97, KY21, MS71, OH7B
        # Tropical = CML103, CML322, CML69, KI11, NC350
        #
        aggTemperateL <- as.matrix((1/6*B73$GS..mmol.m.2.s.1.)+(1/6*B97$GS..mmol.m.2.s.1.)+(1/6*KY21$GS..mmol.m.2.s.1.)+
                                           (1/6*MO17$GS..mmol.m.2.s.1.)+(1/6*MS71$GS..mmol.m.2.s.1.)+(1/6*OH7B$GS..mmol.m.2.s.1.))
        aggTropicalL <- as.matrix((0.2*CML103$GS..mmol.m.2.s.1.)+(0.2*CML322$GS..mmol.m.2.s.1.)+(0.2*CML69$GS..mmol.m.2.s.1.)+
                                          (0.2*KI11$GS..mmol.m.2.s.1.)+(0.2*NC350$GS..mmol.m.2.s.1.))
        
        minL <- as.matrix(pmin((B73$GS..mmol.m.2.s.1.),(B97$GS..mmol.m.2.s.1.),CML103$GS..mmol.m.2.s.1.,CML322$GS..mmol.m.2.s.1.,
                               CML69$GS..mmol.m.2.s.1.,KI11$GS..mmol.m.2.s.1.,KY21$GS..mmol.m.2.s.1.,MO17$GS..mmol.m.2.s.1.,
                               MO18W$GS..mmol.m.2.s.1.,MS71$GS..mmol.m.2.s.1.,NC350$GS..mmol.m.2.s.1.,OH7B$GS..mmol.m.2.s.1.,
                               TX303$GS..mmol.m.2.s.1.,LIRFmonsanto$GS..mmol.m.2.s.1.,Temperate$GS..mmol.m.2.s.1.,Tropical$GS..mmol.m.2.s.1.))
        
        maxL <- as.matrix(pmax(B73$GS..mmol.m.2.s.1.,B97$GS..mmol.m.2.s.1.,CML103$GS..mmol.m.2.s.1.,CML322$GS..mmol.m.2.s.1.,
                               CML69$GS..mmol.m.2.s.1.,KI11$GS..mmol.m.2.s.1.,KY21$GS..mmol.m.2.s.1.,MO17$GS..mmol.m.2.s.1.,
                               MO18W$GS..mmol.m.2.s.1.,MS71$GS..mmol.m.2.s.1.,NC350$GS..mmol.m.2.s.1.,OH7B$GS..mmol.m.2.s.1.,
                               TX303$GS..mmol.m.2.s.1.,LIRFmonsanto$GS..mmol.m.2.s.1.,Temperate$GS..mmol.m.2.s.1.,Tropical$GS..mmol.m.2.s.1.))
        maxL <- as.matrix(pmin(maxL,550))
        #
        ylow <- -25.0
        yhigh <- 600.0
        if (i == 1 || i == 2) par(mar = c(0.0, 4.8, 0.0, 0.0))
        else par(mar = c(0.0, 0.0, 0.0, 4.8))
        if (i == 3 || i == 4) par(yaxt="n")
        else par(yaxt="s")
        if (i == 1 || i == 3) par(xaxt="n")
        else par(xaxt="s")
        plot(B73$DAY+151,B73$GS..mmol.m.2.s.1.,type="l",lwd=1.5, lty=1, col="white", cex.axis=1.5, cex.lab=1.5, 
             ylim=c(ylow,yhigh), ylab=exp_list[15], xlim=c(145,320))
        if (i==1) mtext(plotTitle,side=3,cex=1.25, adj=0, line=0.25)
        if (i!=3) lines(c(noRefillOn[i],noRefillOn[i]),c(-100,1200),lty=3, lwd=1.5, col="black")
        else lines(c(noRefillOn[i],noRefillOn[i]),c(-100,1200),lty=3, lwd=1.5, col="black")
        text(150,575,plotLabels[i], cex=1.5)
        #lines(Temperate$DAY+151,B73$GS..mmol.m.2.s.1., lwd=1.5, lty=1, col="red")
        lines(Temperate$DAY+151,Temperate$GS..mmol.m.2.s.1., lwd=1.5, lty=1, col="skyblue")
        lines(Tropical$DAY+151,Tropical$GS..mmol.m.2.s.1., lwd=1.5, lty=1, col="wheat")
        lines(Temperate$DAY+151,aggTemperateL, lwd=1.5, lty=1, col="skyblue3")
        lines(Tropical$DAY+151,aggTropicalL, lwd=1.5, lty=1, col="wheat3")
        lines(LIRFmonsanto$DAY+151,minL, lwd=1.5, lty=1, col="grey60")
        lines(LIRFmonsanto$DAY+151,maxL, lwd=1.5, lty=1, col="grey39")
        lines(LIRFmonsanto$DAY+151,LIRFmonsanto$GS..mmol.m.2.s.1., lwd=1.5, lty=1, col="darkred")
        if (i == 2)
        {
                legend("topright",lty=c(1,1,1,1,1,1,1),lwd=c(1.5,1.5,1.5,1.5,1.5,1.5,1.5),ncol=1,
                        col=c("skyblue","wheat","skyblue3","wheat3","darkred","grey60","grey39"),
                        c("Mean Temperate Inputs","Mean Tropical Inputs","Temperate Ensemble Mean",
                        "Tropical Ensemble Mean","Hybrid","Full Ensemble Minimum","Full Ensemble Maximum"), 
                       cex=0.8, bty="n")
        }
        if (i == 2 || i == 4) mtext("Day", side=1, line=3, cex=1.25)
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
plot(simEC,sapflux$Flux_mmol)


