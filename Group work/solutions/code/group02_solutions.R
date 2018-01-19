## Group work Chapter 2 solutions
require(xtable)
require(ggplot2)

setwd(dirname(sys.frame(1)$ofile))


### Question 1 - max temps

# Point to correct max temp data
mt <- read.csv('~/Dropbox/Classes - Prof/Stat 201/Materials/Data/max_temps_dec17.csv')

# set appropriate cut points
mt.breaks <- (-1:6)*10
mt$cls <- cut(mt$max.temp, mt.breaks)

# Create table
mt.tab <- table(mt$cls)
mt.tab <- cbind(Freq=mt.tab, Rel=mt.tab/sum(mt.tab), Cum=cumsum(mt.tab))

xtable(mt.tab, digits = c(0, 0, 4, 0))

# Create histogram
g <- ggplot(mt, aes(x=max.temp))
g <- g + geom_histogram(breaks=mt.breaks, fill="cadetblue", col="black")
g <- g + theme_bw() + labs(x="Temperatures (F)", title="Max Temps for December 2017")
g
ggsave('../images/grp02_Q1_b.png', width=4, height=2.5, units = "in")


### Question 2 - air pollution

# Read data
ap <- read.csv('~/Dropbox/Classes - Prof/Stat 201/Materials/Data/max_air_pol.csv')

# set appropriate cut points
ap.breaks <- (2:10)*5
ap$cls <- cut(ap$max.fpm, ap.breaks)

# Create table
ap.tab <- table(ap$cls)
ap.tab <- cbind(Freq=ap.tab, Rel=ap.tab/sum(ap.tab), Cum=cumsum(ap.tab))

xtable(ap.tab, digits = c(0, 0, 4, 0))

# Create histogram
g <- ggplot(ap, aes(x=max.fpm))
g <- g + geom_histogram(breaks=ap.breaks, fill="cadetblue", col="black")
g <- g + theme_bw() + labs(x=expression(paste("Fine Particulate Matter (", mu, "g/", m^3,")")), 
                           title="Max Air Pollution from 2007 to 2009")
g
ggsave('../images/grp02_Q2_b.png', width=4, height=2.5, units = "in")


### Question 3 - plots

## Part a
faith <- read.csv('~/Dropbox/Classes - Prof/Stat 201/Materials/Data/faithful.csv')

# Create scatterplot
g <- ggplot(faith, aes(x=eruptions, y=waiting))
g <- g + geom_point(shape=20)
g <- g + theme_bw() + labs(x="Length of eruption (min)",
                           y="Waiting time (min)",
                           title="Old Faithful Eruption and Waiting Times")

g

ggsave('../images/grp02_Q3_a.png', width=4, height=2.5, units = "in")


## Part b

eye <- read.csv('~/Dropbox/Classes - Prof/Stat 201/Materials/Data/hair_eye.csv')
eye.tab <- sort(table(eye$Eye), decreasing = T)
eye.df <- data.frame(eye.col=factor(rownames(eye.tab), levels=rownames(eye.tab)), cnt=eye.tab)

g <- ggplot(eye.df, aes(x=eye.col, y=cnt))
g <- g + geom_bar(stat="identity", fill=c("brown", "blue", "gold3", "green"), col="black")
g <- g + theme_bw() + labs(x="Eye color", y="Count",
                           title="Eye Colors of Statistics Students")
g

ggsave('../images/grp02_Q3_b.png', width=4, height=2.5, units = "in")


## Part c

g <- ggplot(ap, aes(x=month, y=max.fpm))
g <- g + geom_line(size=0.1) + geom_point(shape=20, size=2)
g <- g + theme_bw() + scale_x_continuous(breaks=c(1,13,25), labels=c("2007","2008","2009"))
g <- g + labs(x="Month", y=expression(paste("FPM (", mu, "g/", m^3,")")),
              title="Time series of air pollution in MN")
g

ggsave('../images/grp02_Q3_ca.png', width=4, height=2.5, units = "in")
