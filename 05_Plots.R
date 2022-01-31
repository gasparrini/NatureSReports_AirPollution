#####################################################
#
# Plots
#
#####################################################

# Results from INLA model (it takes a few minutes)
#load("Results/INLA_Stringency_results.RData")
# Results from policy specific models (it takes a few minutes)
#load("Results/INLA_policy_results.RData")

#------------------------
# Figure 1: Time series of AP-change
#------------------------

# Pollutant colors
pol      <- c("DiffNO2", "DiffO3", "DiffPM25", "DiffPM10")
cit_high <- c("Stockholm", "London", "Milan")
pol_col  <- c("red", "green", "blue", 6)
city_col <- c("#b10026", "#238b45", "#084594", "#e7298a")
no2.col  <- c("#cc4c02","#9e0142", "#d7301f")
o3.col   <- c("#008080", "#FF8C00","#808000")
pm25.col <- c("#00FFFF","#4292c6", "#023858")
pm10.col <- c("#8A2BE2", "#FF69B4", "#7a0177")


names(datatab)
difplot      <- datatab[, c("name","Date","id","country", pol)]
difplot$name <- as.character(difplot$name)
head(difplot)

# Loop to plot
for (p in seq(pol)){
  cat(pol[p], "\n")
  difplot$poldif <- difplot[, pol[p]]  
  pol.cities <- difplot[difplot$name%in%cit_high, ]
  meanpol <- aggregate(poldif ~ Date, data = difplot, mean, na.action = na.pass, na.rm = T)
  #head(meanpol)
  
  if (pol[p]%in%c("DiffNO2","DiffO3", "DiffPM25")) {
    xlabel <- xtitle <- element_blank()}
  
  if (pol[p]=="DiffPM10"){
    xlabel <- element_text(color='black', size=14)
    xtitle <- element_text(color='black', size=14, margin = margin(t = .3, unit = "cm"))}
  
  # Defining Colour ##
  if (pol[p]%in%"DiffNO2") {city_col <- no2.col}
  if (pol[p]%in%"DiffO3")  {city_col <- o3.col}  
  if (pol[p]%in%"DiffPM25"){city_col <- pm25.col}
  if (pol[p]%in%"DiffPM10"){city_col <- pm10.col}
  
  applot <- ggplot(data=difplot)+
    geom_line(data=difplot, aes(x=Date, y=poldif , group=name), color=grey(.9), size=0.5)+
    geom_line(data=meanpol, aes(x=Date, y=poldif), color=pol_col[p], size=1)+
    geom_line(data=difplot[difplot$name==cit_high[1],],aes(x=Date, y=poldif),color=city_col[1], size=0.6,linetype=1)+ 
    geom_line(data=difplot[difplot$name==cit_high[2],],aes(x=Date, y=poldif),color=city_col[2], size=0.5,linetype=6)+
    geom_line(data=difplot[difplot$name==cit_high[3],],aes(x=Date, y=poldif),color=city_col[3], size=0.3,linetype=2)+
    geom_hline(yintercept =0, lty=1, lwd=0.5, color=grey(.3))+
    
  # Include City names in all plots
  {if(!pol[p]=="DiffO3")   {annotate("text",    x = as.Date("2020-02-19"), y = c(-11, -12.5, -14) , label = cit_high)}}+
    {if(!pol[p]=="DiffO3") {annotate("segment", x = as.Date("2020-02-02"), xend = as.Date("2020-02-10"), y = -11, yend = -11,    colour =city_col[1], size=0.8, linetype=1)}}+
    {if(!pol[p]=="DiffO3") {annotate("segment", x = as.Date("2020-02-02"), xend = as.Date("2020-02-10"), y = -12.5, yend = -12.5,colour =city_col[2], size=0.8, linetype=6)}}+
    {if(!pol[p]=="DiffO3") {annotate("segment", x = as.Date("2020-02-02"), xend = as.Date("2020-02-10"), y = -14, yend = -14,    colour =city_col[3], size=0.8, linetype=2)}}+
    
    {if(pol[p]=="DiffO3") {annotate("text",    x = as.Date("2020-02-19"), y = c(-1, -2.5, -4) , label = cit_high)}}+
    {if(pol[p]=="DiffO3") {annotate("segment", x = as.Date("2020-02-02"), xend = as.Date("2020-02-10"), y = -1, yend = -1,    colour =city_col[1], size=0.8, linetype=1)}}+
    {if(pol[p]=="DiffO3") {annotate("segment", x = as.Date("2020-02-02"), xend = as.Date("2020-02-10"), y = -2.5, yend = -2.5,colour =city_col[2], size=0.8, linetype=6)}}+
    {if(pol[p]=="DiffO3") {annotate("segment", x = as.Date("2020-02-02"), xend = as.Date("2020-02-10"), y = -4, yend = -4,    colour =city_col[3], size=0.8, linetype=2)}}+
  
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
    
    # Y label according to AP
    {if(pol[p]=="DiffNO2") {ggplot2::scale_y_continuous(expression("NO"[2]*" change - "*mu*"g/m"^3),
                                                        limits = c(range(difplot$poldif)[1], range(difplot$poldif)[2]))}}+
    {if(pol[p]=="DiffO3")  {ggplot2::scale_y_continuous(expression("O"[3]*" change - "*mu*"g/m"^3),
                                                        limits = c(range(difplot$poldif)[1], range(difplot$poldif)[2]))}}+
    {if(pol[p]=="DiffPM25"){ggplot2::scale_y_continuous(expression("PM"[2.5]*" change - "*mu*"g/m"^3),
                                                        limits = c(range(difplot$poldif)[1], range(difplot$poldif)[2]))}}+
    {if(pol[p]=="DiffPM10"){ggplot2::scale_y_continuous(expression("PM"[10]*" change - "*mu*"g/m"^3),
                                                        limits = c(range(difplot$poldif)[1], range(difplot$poldif)[2]))}}+
  theme(
      panel.background = element_rect(fill = 'NA'),
      axis.text.x  = xlabel,
      axis.title.x = xtitle,
      axis.ticks   = element_line(size = 2, color=grey(0.8)),
      axis.title.y = element_text(color='black', size=12), 
      axis.text.y  = element_text(color='black', size=12, angle = 0, hjust = 0.5),
      axis.ticks.length.y = unit(.25, "cm"),
      axis.ticks.length.x = unit(.25, "cm"),
      panel.grid.major = element_line(size=0.5, linetype=2, colour = grey(0.8)),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.ontop = F
    )
  #applot
  assign(pol[p], applot)
  remove(applot)
} 

apchange <- gridExtra::grid.arrange(DiffNO2,DiffO3, DiffPM25, DiffPM10,nrow=4, ncol=1)
#ggsave("Figure1.pdf", apchange, width = 10, height = 15)
ggsave("Figure1.png", apchange, width = 10, height = 15)

#-------------------------------
# Figure 2: Association between SI score and AP-change
#-------------------------------

# High SI level for display
highSI <- 80
# Pollutant colors
pol_col <- c(2, 3, 4, 6)
# Pollutant labels
pol_lab <- c(expression(NO[2]*" change ("*mu*"g/m"^3*")"), 
             expression(O[3]*" change ("*mu*"g/m"^3*")"),
             expression(PM[2.5]*" change ("*mu*"g/m"^3*")"),
             expression(PM[10]*" change ("*mu*"g/m"^3*")"))

# Which SI grid is below the max SI level
si_below <- si_grid <= highSI

# Cities to highlight in plots
cit_high <- cit_high <- list(
    NO2  = c("Stockholm", "London", "Milan"),
    O3   = c("Stockholm", "London", "Milan"),
    PM25 = c("Stockholm", "London", "Milan"),
    PM10 = c("Stockholm", "London", "Milan")
  )

# Loop to plot
x11(width = 25, height = 70)
par(mfrow = c(4,1),mar = c(7,9,1,1))

for (p in seq_len(np)){
  # Extract city curves
  city_curves <- city_pred[si_below,,p,1]
  
  # Cities to highlight
  cit_ind <- match(cit_high[[pollutants[p]]], cities$name)
  
  # Plot all city curves  
  matplot(si_grid[si_below], city_curves, type = "l", col = grey(.7), lty = 1,
          xlab=NA, ylab=NA, ylim = range(c(-10, 4)), axes=F)
  
  title(ylab =(pol_lab[p]), line=5, cex.lab=3.5)
  if(pollutants[p]=="PM10"){
    title(xlab ="Stringency Index (in %)", line=5, cex.lab=3.5)}
  
  abline(v=seq(0, 80, by=10), lty = 3, lwd=1.5, col="light blue")
  text(seq(0, 80, by=10), labels=seq(0, 80, by=10), par("usr")[3]-0.2, srt = 0,pos = 1, xpd = TRUE, offset=-1.5, cex=3) 
  abline(h=seq(-10, 4, by=2), lty = 3, lwd=1.5, col="light blue")
  text(1, seq(-10, 4, by=2), labels=seq(-10, 4, by=2), srt = 0, pos =2, xpd = TRUE, offset=3, cex=3)
  polygon(c(si_grid[si_below], rev(si_grid[si_below])), c(fixed_pred[si_below,p,2],
                rev(fixed_pred[si_below,p,3])), col = adjustcolor(pol_col[p], .2), border = NA)
  abline(h = 0, lty = 1, lwd=1.5, col=1)
  lines(si_grid[si_below], fixed_pred[si_below,p,1], col = pol_col[p], lwd = 4)
}

dev.print(png, file ="Figure2.png", width = 25, height = 70, units = "cm", res=300)

#-------------------------------
# Figure 3: Estimated AP-change at 80% SI score
#-------------------------------

# Pollutant specific scale 
width =22 ; height=22
x11(width = width, height =height)
par(mfrow = c(2,2), mar=c(0,0,0,0))

for (p in seq_len(np)){
  # Get the value for selected SI level
  pval <- city_pred[si_grid == highSI,,p,1]
  
  # Colorscale
  cutoff <- pretty(max(abs(pval), na.rm = T) * c(-1, 1), n = 10)
  labels <- paste0(paste0(cutoff[-length(cutoff)], ",", cutoff[-1]))
  citycat<- cut(pval, cutoff, labels = labels, include.lowest = T)
  pal    <- adjustcolor(colorRampPalette(c(pol_col[p], "white", "black"))(length(labels)), .8)
  
  whichint <- cutoff[-1] >= min(pval, na.rm = T) & 
    cutoff[-length(cutoff)] <= max(pval, na.rm = T)
  
  # Plot map
  map("world", mar=c(0,0,4,0), col = grey(0.9), xlim = c(-25, 40), ylim = c(30, 70),
               myborder = 0, fill = T, border = grey(0.7), lwd = 0.3)
  map.axes(col =grey(0.7), cex.axis=0.5)
  points(cities$cities_lon, cities$cities_lat, pch = 21, cex = 2.8, col=grey(.50), bg = pal[citycat])
  if(p==4){map.scale(-24, 33.8, ratio = F, cex = 0.5, relwidth = 0.1, xpd = T, lwd = 3)}
  lg <- legend(-24, 55, labels[whichint], pt.cex = 1.5, bty = "n",pch = 21, col=grey(.40), 
               pt.bg = pal[whichint], cex = 0.9, inset = 0.02)
  
  if(pollutants[p]=="NO2") {text(lg$rect$left, lg$rect$top + 2 * strheight("("),expression("NO"[2]*" change - "*mu*"g/m"^3),adj = c(0, -0.5), cex = 0.7)}
  if(pollutants[p]=="O3")  {text(lg$rect$left, lg$rect$top + 2 * strheight("("),expression("O"[3]*" change - "*mu*"g/m"^3),adj = c(0, -0.5), cex = 0.7)}
  if(pollutants[p]=="PM25"){text(lg$rect$left, lg$rect$top + 2 * strheight("("),expression("PM"[2.5]*" change - "*mu*"g/m"^3),adj = c(0, -0.5), cex = 0.7)}
  if(pollutants[p]=="PM10"){text(lg$rect$left, lg$rect$top + 2 * strheight("("),expression("PM"[10]*" change - "*mu*"g/m"^3),adj = c(0, -0.5), cex = 0.7)}
  text(lg$rect$left, lg$rect$top + strheight("("),sprintf("SI = %i%%", highSI),adj = c(0, -0.5), cex = 0.7)
  
}
dev.print(png, file = "Figure3.png", width=width, height=height, units = "cm", res = 300)

#-------------------------------
# Figure 4: Policy effects
#-------------------------------

pol_leg<- c(expression("NO"[2]),   expression("O"[3]), 
            expression("PM"[2.5]), expression("PM"[10]))

policy_lab <- c("C1:School\nclosing", "C2:Workplace\nclosing", "C3:Cancel\npublic events", 
                "C4:Restrictions\non gatherings", "C5:Close\npublic transport", 
                "C6:Stay at\nhome requirements", "C7:Restrictions on\ninternal movement", 
                "C8:International\ntravel controls", "H1:Public information\ncampaigns")

# Reorder score_tab
score_tab <- score_tab[order(score_tab$policy, score_tab$pollutant),]

# Determine the x position of each value
atx <- 1:nrow(score_tab) + rep(1:ncomp, each = np)

# Prepare dots color
colx <- ifelse(with(score_tab, policy_low > 0 | policy_high < 0), 
               pol_col, adjustcolor(pol_col, .5))

x11(width = 15, height=7)
par(mar = c(7, 5, 1, 5))
plot(atx, score_tab[, "policy_coef"], pch = 16, xlab = "", 
     ylab = expression(paste("Pollutant change ( " , mu, "g/", m^3, ")")),
     col = NA, xaxt = "n", axes=F, ylim = range(score_tab[,c("policy_low", "policy_high")]))

axis(2, at=seq(-3, 3, by=1), labels = seq(-3, 3, by=1), col=grey(0.7), line=0)
axis(1, at = c(tapply(atx, rep(1:ncomp, each = np), min) - 1, max(atx) + 1), labels = NA, col=grey(0.7), cex=2)
text(x = tapply(atx, rep(1:ncomp, each = np), mean), y = par("usr")[3] -.1, policy_lab, srt = 315, xpd = T, adj = c(0, 1))
segments(x0 = atx, y0 = score_tab[,"policy_low"], y1 = score_tab[,"policy_high"], lwd = 2, col = pol_col)
points(atx, score_tab[, "policy_coef"], pch = 21:24, cex = 1.2, lwd = 1, col = pol_col, bg = "white")
abline(v = tapply(atx, rep(1:ncomp, each = np), min)[-1] - 1, lwd=1, lty=1, col=grey(0.6))
grid(nx =NA, ny = NULL)
abline(h = 0, col=1, lwd=1.5, lty =1)
lg <- legend(par("usr")[2], par("usr")[4], pol_leg, lwd = 2, pch = 21:24, 
             pt.bg = "white", bty = "n", xpd = T, col = pol_col)

dev.print(png, file ="Figure4.png", width = 25, height=15, units = "cm", res=200)