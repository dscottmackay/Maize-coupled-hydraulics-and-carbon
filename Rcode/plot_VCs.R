#
#Plot vulnerability curves for maize genotypes
#Data from Sean Gleason, curves fitted August 2019 
#

setwd(paste("~/Documents/research/manuscripts/maizeHydraulics/",sep=""))
curves<-read.csv(paste("TREES-parameter-prep/plc-curves",".csv",sep=""),header=TRUE)

#
#Define some y-axis labels
#
exp_list <- c(as.expression(bquote(Psi[MD]~"(MPa)" )),
              as.expression(bquote(italic(E)[C]~"or"~italic(E)[Crit]~"("~"mmol" ~s^-1~")" )),
              as.expression(bquote("Relative root area" )),
              as.expression(bquote("Fine root C (gC "~m^-2 ~")" )),
              as.expression(bquote("LAI ("~m^2 ~m^-2~")" )),
              as.expression(bquote("Reproduction (gC "~m^-2 ~")" )),
              as.expression(bquote("Plant N (gN "~m^-2 ~")" )),
              as.expression(bquote("NSC (gC "~m^-2 ~")" )),
              as.expression(bquote(italic(E)[C]~"or"~italic(F)[Rhiz]~"("~"mmol" ~s^-1~")" )),
              as.expression(bquote(Psi[Leaf]~"(MPa)" )),
              as.expression(bquote("PLC (%)")),
              as.expression(bquote("GPP (umol "~m^-2 ~"gnd" ~s^-1 ~")" )),
              as.expression(bquote(italic(E)[C] ~" (mmol "~m^-2 ~"gnd" ~s^-1 ~")" )))

pdf("plots/maize_genotypes_VCs2.pdf", width = 9, height= 4.5, useDingbats = F)

layout(matrix(1:2, ncol = 2, nrow=1), widths = rep(1, 2), heights = c(4.5), respect = FALSE)
#par(oma=c(0.1,1.1,1,.1))
#par(mar = c(4.0, 4.8, 1.0, 2.1))
par(oma=c(6.1, 1.0, 2.0, 0.5))
par(mar = c(0.0, 4.8, 0.0, 0.0))
plot(curves$LWP,curves$B73,type="l",lwd=1.5, lty=1, col="red", cex.axis=1.25, cex.lab=1.25, 
     ylim=c(-5,105), ylab=exp_list[11], xlab="")
lines(curves$LWP,curves$B97, lwd=1.5, lty=2, col="red")
lines(curves$LWP,curves$CML103, lwd=1.5, lty=1, col="blue")
lines(curves$LWP,curves$CML322, lwd=1.5, lty=2, col="blue")
lines(curves$LWP,curves$CML69, lwd=1.5, lty=3, col="blue")
lines(curves$LWP,curves$KI11, lwd=1.5, lty=1, col="purple")
lines(curves$LWP,curves$KY21, lwd=1.5, lty=2, col="purple")
lines(curves$LWP,curves$MO17, lwd=1.5, lty=1, col="tan1")
lines(curves$LWP,curves$MO18W, lwd=1.5, lty=2, col="tan1")
lines(curves$LWP,curves$MS71, lwd=1.5, lty=1, col="darkred")
lines(curves$LWP,curves$NC350, lwd=1.5, lty=1, col="darkblue")
lines(curves$LWP,curves$OH7B, lwd=1.5, lty=1, col="purple3")
lines(curves$LWP,curves$TX303, lwd=1.5, lty=1, col="tan4")

mtext(exp_list[10], side=1, line=2.5, cex=1.25)
mtext("Maize vulnerability to cavitation",side=3,cex=1.0, adj=0, line=0.25)
legend("bottomleft",lty=c(1,2,1,2,3,1,2,1,2,1,1,1,1),
       lwd=c(1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5),
       col=c("red","red","blue","blue","blue","purple","purple","tan1","tan1","darkred","darkblue",
             "purple3","tan4"), ncol=1,
       c("B73","B97","CML103","CML322","CML69","KI11","KY21","MO17","MO18W",
         "MS71","NC350","OH7B","TX303"), 
       cex=0.8, bty="n")
box(lwd=1.5)
par(mar = c(0.0, 0.0, 0.0, 4.8))
par(yaxt="n")
plot(curves$LWP,curves$B73,type="l",lwd=1.5, lty=1, col="white", cex.axis=1.25, cex.lab=1.25, 
     ylim=c(-5,105), ylab=exp_list[11], xlab="")
lines(curves$LWP,curves$LIRF.Monsanto, lwd=1.5, lty=1, col="darkred")
lines(curves$LWP,curves$Temperate, lwd=1.5, lty=1, col="skyblue")
lines(curves$LWP,curves$Tropical, lwd=1.5, lty=1, col="wheat")
mtext(exp_list[10], side=1, line=2.5, cex=1.25)
legend("bottomleft",lty=c(1,1,1),lwd=c(1.5,1.5,1.5),col=c("darkred","skyblue","wheat"),
                c("Hybrid","Mean temperate","Mean tropical"), cex=0.8, bty="n")
box(lwd=1.5)

dev.off()

