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
              as.expression(bquote(italic(L)[S]~" ("~m^2 ~kgC^-1~")" )),
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
plotFile <- "plots/maize_genotypes_2013_SLA.pdf"
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
        aggTemperateL <- as.matrix((1/6*B73$SLA..m.2.kgC.)+(1/6*B97$SLA..m.2.kgC.)+(1/6*KY21$SLA..m.2.kgC.)+
                                           (1/6*MO17$SLA..m.2.kgC.)+(1/6*MS71$SLA..m.2.kgC.)+(1/6*OH7B$SLA..m.2.kgC.))
        aggTropicalL <- as.matrix((0.2*CML103$SLA..m.2.kgC.)+(0.2*CML322$SLA..m.2.kgC.)+(0.2*CML69$SLA..m.2.kgC.)+
                                          (0.2*KI11$SLA..m.2.kgC.)+(0.2*NC350$SLA..m.2.kgC.))
        
        minL <- as.matrix(pmin((B73$SLA..m.2.kgC.),(B97$SLA..m.2.kgC.),CML103$SLA..m.2.kgC.,CML322$SLA..m.2.kgC.,
                               CML69$SLA..m.2.kgC.,KI11$SLA..m.2.kgC.,KY21$SLA..m.2.kgC.,MO17$SLA..m.2.kgC.,
                               MO18W$SLA..m.2.kgC.,MS71$SLA..m.2.kgC.,NC350$SLA..m.2.kgC.,OH7B$SLA..m.2.kgC.,
                               TX303$SLA..m.2.kgC.,LIRFmonsanto$SLA..m.2.kgC.,Temperate$SLA..m.2.kgC.,Tropical$SLA..m.2.kgC.))
        
        maxL <- as.matrix(pmax(B73$SLA..m.2.kgC.,B97$SLA..m.2.kgC.,CML103$SLA..m.2.kgC.,CML322$SLA..m.2.kgC.,
                               CML69$SLA..m.2.kgC.,KI11$SLA..m.2.kgC.,KY21$SLA..m.2.kgC.,MO17$SLA..m.2.kgC.,
                               MO18W$SLA..m.2.kgC.,MS71$SLA..m.2.kgC.,NC350$SLA..m.2.kgC.,OH7B$SLA..m.2.kgC.,
                               TX303$SLA..m.2.kgC.,LIRFmonsanto$SLA..m.2.kgC.,Temperate$SLA..m.2.kgC.,Tropical$SLA..m.2.kgC.))
        #
        ylow <- 32
        yhigh <- 54
        if (i == 1 || i == 2) par(mar = c(0.0, 4.8, 0.0, 0.0))
        else par(mar = c(0.0, 0.0, 0.0, 4.8))
        if (i == 3 || i == 4) par(yaxt="n")
        else par(yaxt="s")
        if (i == 1 || i == 3) par(xaxt="n")
        else par(xaxt="s")
        plot(B73$DAY+151,B73$SLA..m.2.kgC.,type="l",lwd=1.5, lty=1, col="white", cex.axis=1.5, cex.lab=1.5, 
             ylim=c(ylow,yhigh), ylab=exp_list[7], xlim=c(145,320))
        if (i==1) mtext(plotTitle,side=3,cex=1.25, adj=0, line=0.25)
        lines(c(noRefillOn[i],noRefillOn[i]),c(25,60),lty=3, lwd=1.5, col="black")
        text(153,53,plotLabels[i], cex=1.5)
        #lines(Temperate$DAY+151,B73$SLA..m.2.kgC., lwd=1.5, lty=1, col="red")
        lines(Temperate$DAY+151,Temperate$SLA..m.2.kgC., lwd=1.5, lty=1, col="skyblue")
        lines(Tropical$DAY+151,Tropical$SLA..m.2.kgC., lwd=1.5, lty=1, col="wheat")
        lines(Temperate$DAY+151,aggTemperateL, lwd=1.5, lty=1, col="skyblue3")
        lines(Tropical$DAY+151,aggTropicalL, lwd=1.5, lty=1, col="wheat3")
        lines(LIRFmonsanto$DAY+151,minL, lwd=1.5, lty=1, col="grey60")
        lines(LIRFmonsanto$DAY+151,maxL, lwd=1.5, lty=1, col="grey39")
        lines(LIRFmonsanto$DAY+151,LIRFmonsanto$SLA..m.2.kgC., lwd=1.5, lty=1, col="darkred")
        if (i == 1)
        {
                legend("topright",lty=c(1,1,1,1,1,1,1),lwd=c(1.5,1.5,1.5,1.5,1.5,1.5,1.5), ncol=1,
                        col=c("skyblue","wheat","skyblue3","wheat3","darkred","grey60","grey39"),
                        c("Mean Temperate Inputs","Mean Tropical Inputs","Temperate Ensemble Mean",
                        "Tropical Ensemble Mean","Hybrid","Full Ensemble Minimum","Full Ensemble Maximum"), cex=0.9, bty="n")
        }
        if (i == 2 || i == 4) mtext("Yearday", side=1, line=3, cex=1.25)
        box(lwd=1.5)
}

dev.off()


