

setwd(paste("~/Documents/research/manuscripts/maizeHydraulics/",sep=""))
subfolder <- "simulations/corn_leaf2013_V7/"
responses <-read.csv(paste(subfolder,"response_summary",".csv",sep=""),header=TRUE)

exp_list <- c(as.expression(bquote(Psi[MD]~"(MPa)" )),
              as.expression(bquote(italic(G)[W]~"("~"mmol"~m^-2 ~s^-1~")" )),
              as.expression(bquote("Relative root area" )),
              as.expression(bquote("Fine root C (gC "~m^-2 ~")" )),
              as.expression(bquote(italic(L)[T]~" ("~m^2 ~m^-2~")" )),
              as.expression(bquote("Root Area Index ("~m^2 ~m^-2~")" )),
              as.expression(bquote("SLA ("~m^2 ~kgC^-1~")" )),
              as.expression(bquote("Reproduction (gC "~m^-2 ~")" )),
              as.expression(bquote("Plant N (gN "~m^-2 ~")" )),
              as.expression(bquote("NSC (gC "~m^-2 ~")" )),
              as.expression(bquote(italic(P)[88]~" (-MPa)" )),
              as.expression(bquote(Psi[Leaf]~"(MPa)" )),
              as.expression(bquote("PLC (%)")),
              as.expression(bquote("GPP (umol "~m^-2 ~"gnd" ~s^-1 ~")" )),
              as.expression(bquote(italic(V)[cmax] ~" (umol "~m^-2 ~s^-1 ~")" )),
              as.expression(bquote(italic(V)[pmax] ~" (umol "~m^-2 ~s^-1 ~")" )))
#
plotFile <- "plots/inputs&responses.pdf"
pdf(plotFile, width = 9, height= 3, useDingbats = F)
layout(matrix(1:3, ncol = 3, nrow=1), widths = rep.int(1,3), heights = rep.int(1,1), respect = FALSE)
par(oma=c(0,6,0,6))
par(mar=c(6.1, 0.0, 2.0, 0.0))
par(yaxt="s")
plot(responses$Vpmax,responses$LT.V9.205, type="p",pch=3, col=responses$Color, cex.axis=1.5, cex.lab=1.25, 
     ylim=c(1.5,5.5), ylab=exp_list[11], xlim=c(30,105), xlab="", cex=1.5)
points(responses$Vpmax, responses$LT.V7.205, pch=19, col=responses$Color, cex=1.5)
plotTitle <- "Maize, LIRF 2013"
mtext(plotTitle,side=3,cex=1.0, adj=0, line=0.25)
mtext(exp_list[16], side=1, line=3, cex=1.0)
mtext(exp_list[5], side=2, line=3, cex=1.0)
text(33,5.45,"a", cex=1.25)
box(lwd=1.5)
par(mar=c(6.1, 0.0, 2.0, 0.0))
par(yaxt="n")
plot(responses$Vcmax,type="p", responses$LT.V9.205, pch=3, col=responses$Color, cex.axis=1.5, cex.lab=1.25, 
     ylim=c(1.5,5.5), ylab=exp_list[5], xlim=c(35,75), xlab="", cex=1.5)
points(responses$Vcmax, responses$LT.V7.205, pch=19, col=responses$Color, cex=1.5)
mtext(exp_list[15], side=1, line=3, cex=1.0)
text(37,5.45,"b", cex=1.25)
box(lwd=1.5)
par(mar=c(6.1, 0.0, 2.0, 0.0))
par(yaxt="n")
plot(responses$P88...Mpa.,responses$LT.V9.205, type="p",pch=3, col=responses$Color, cex.axis=1.5, cex.lab=1.25, 
     ylim=c(1.5,5.5), ylab="", xlim=c(1.75,3.75), xlab="", cex=1.5)
points(responses$P88...Mpa., responses$LT.V7.205, pch=19, col=responses$Color, cex=1.5)
mtext(exp_list[11], side=1, line=3, cex=1.0)
text(1.85,5.45,"c", cex=1.25)
legend("bottomright", pch=c(19,19,19), col=c("skyblue3","wheat3","darkred"), 
       c("Temperate","Tropical","Mixed"), bty="n", pt.cex=1.5, cex=1.5)
box(lwd=1.5)

dev.off()

#
#
#

plotFile <- "plots/inputs&responses_Gs.pdf"
pdf(plotFile, width = 9, height= 3, useDingbats = F)
layout(matrix(1:3, ncol = 3, nrow=1), widths = rep.int(1,3), heights = rep.int(1,1), respect = FALSE)
par(oma=c(0,6,0,6))
par(mar=c(6.1, 0.0, 2.0, 0.0))
par(yaxt="s")
plot(responses$Vpmax,responses$Gs.V7.64, type="p",pch=19, col=responses$Color, cex.axis=1.5, cex.lab=1.25, 
     ylim=c(150,210), ylab=exp_list[2], xlim=c(30,105), xlab="", cex=1.5)
#points(responses$Vpmax, responses$LT.V7.205, pch=19, col=responses$Color)
plotTitle <- "Maize, LIRF 2013"
mtext(plotTitle,side=3,cex=1.0, adj=0, line=0.25)
mtext(exp_list[16], side=1, line=3, cex=1.0)
mtext(exp_list[2], side=2, line=3, cex=1.0)
text(33,205,"a", cex=1.25)
box(lwd=1.5)
par(mar=c(6.1, 0.0, 2.0, 0.0))
par(yaxt="n")
plot(responses$Vcmax,type="p", responses$Gs.V7.64, pch=19, col=responses$Color, cex.axis=1.5, cex.lab=1.25, 
     ylim=c(150,210), ylab=exp_list[5], xlim=c(35,75), xlab="", cex=1.5)
#points(responses$Vcmax, responses$LT.V7.205, pch=19, col=responses$Color)
mtext(exp_list[15], side=1, line=3, cex=1.0)
text(37,205,"b", cex=1.25)
box(lwd=1.5)
par(mar=c(6.1, 0.0, 2.0, 0.0))
par(yaxt="n")
plot(responses$P88...Mpa.,responses$Gs.V7.64, type="p",pch=19, col=responses$Color, cex.axis=1.5, cex.lab=1.25, 
     ylim=c(150,210), ylab="", xlim=c(1.75,3.75), xlab="", cex=1.5)
#points(responses$P88...Mpa., responses$LT.V7.205, pch=19, col=responses$Color)
mtext(exp_list[11], side=1, line=3, cex=1.0)
text(1.85,205,"c", cex=1.25)
legend("bottomright", pch=c(19,19,19), col=c("skyblue3","wheat3","darkred"), 
       c("Temperate","Tropical","Mixed"), bty="n", pt.cex=1.5, cex=1.5)
box(lwd=1.5)

dev.off()

x <- responses$Vcmax
y <- responses$P88...Mpa.
test <- glm(y~x)
anova(test)
summary(test)

x <- responses$Vpmax
y <- responses$LT.V7.205
test <- glm(y~x)
summary(test)

x <- responses$Vpmax
y <- responses$LT.V9.205
test <- glm(y~x)
summary(test)

x <- responses$Vpmax
y <- responses$Gs.V7.64
test <- glm(y~x)
summary(test)

x <- responses$Vcmax
y <- responses$Gs.V7.64
test <- glm(y~x)
summary(test)

x <- responses$P88...Mpa.
y <- responses$Gs.V7.64
test <- glm(y~x)
summary(test)


x <- responses$Vcmax
y <- responses$LT.V7.205
test <- glm(y~x)
summary(test)

x <- responses$P88...Mpa.
y <- responses$LT.V7.205
test <- glm(y~x)
summary(test)

x <- responses$Vcmax
y <- responses$LT.V9.205
test <- glm(y~x)
summary(test)

x <- responses$P88...Mpa.
y <- responses$LT.V9.205
test <- glm(y~x)
summary(test)



