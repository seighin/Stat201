## Group work Chapter 3 solutions
require(xtable)
require(ggplot2)

setwd(dirname(sys.frame(1)$ofile))

# Given a vector of numeric data, return vector of modes
find.mode <- function(x){
    x.tab <- table(x)
    x.modes <- rownames(x.tab[x.tab==max(x.tab)])
    if(length(x.modes)==length(x.tab)){
        x.modes <- c()
    }
    
    return(x.modes)
}
### Question 1 - max temps

# Point to correct max temp data
mt <- read.csv('~/Dropbox/Classes - Prof/Stat 201/Materials/Data/max_temps_dec17.csv')

# Find measures of center
mt.ctr <- data.frame(Mean=mean(mt$max.temp),
                     Median=median(mt$max.temp),
                     Mode=paste(find.mode(mt$max.temp), collapse=", "),
                     Midrange=mean(range(mt$max.temp)))
xt <- xtable(mt.ctr, align="ccccc")
print(xt, include.rownames=FALSE)

# Find measures of variance
mt.var <- data.frame(Range=diff(range(mt$max.temp)),
                     Variance=var(mt$max.temp),
                     SD=sd(mt$max.temp))

xt <- xtable(mt.var, align="cccc")
print(xt, include.rownames=FALSE)

# Find percentiles
# What percentile is 45 F?
ceiling((ecdf(mt$max.temp)(46))*100)
# What temp is 10th percentile?
quantile(mt$max.temp, .1)


### Question 2 - home sales

# Point to correct hame sale data
hs <- read.csv('~/Dropbox/Classes - Prof/Stat 201/Materials/Data/mpls_home_sales.csv')

# Find measures of center
hs.ctr <- data.frame(Mean=mean(hs$sale.price),
                     Median=median(hs$sale.price),
                     Mode=paste(find.mode(hs$sale.price), collapse=", "),
                     Midrange=mean(range(hs$sale.price)))
xt <- xtable(hs.ctr, align="ccccc")
print(xt, include.rownames=FALSE)

# Create histogram
g <- ggplot(hs, aes(x=sale.price))
g <- g + geom_histogram(fill="cadetblue", col="black", bins=15)
g <- g + theme_bw() + labs(x="Sale Price ($)", title="Minneapolis Home Sale Prices")
g
ggsave('../images/grp03_Q2_a.png', width=4, height=2.5, units = "in")


# Find measures of variance
hs.var <- data.frame(Range=diff(range(hs$sale.price)),
                     Variance=var(hs$sale.price),
                     SD=sd(hs$sale.price))

xt <- xtable(hs.var, align="cccc")
print(xt, include.rownames=FALSE)

# 5 number summary qnd boxplot
five.sum <- quantile(hs$sale.price, c(0, .25, .5, .75, 1))
five.sum.df <- data.frame(Min=five.sum[1],
                          Q1=five.sum[2],
                          Med=five.sum[3],
                          Q3=five.sum[4],
                          Max=five.sum[5])

xt <- xtable(five.sum.df, align="cccccc")
print(xt, include.rownames=FALSE)

g <- ggplot(hs, aes(x=1,y=sale.price))
g <- g + geom_boxplot(coef=7, fill="cadetblue") + coord_flip()
g <- g + theme_bw() + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
g <- g + labs(y="Sale Price ($)", x="",
                           title="Boxplot of Minneapolis Home Sale Prices")
g

ggsave('../images/grp03_Q2_c.png', width=4, height=2.5, units = "in")


### Question 3

# Point to correct hame sale data
h <- read.csv('~/Dropbox/Classes - Prof/Stat 201/Materials/Data/heights.csv')

h.means <- apply(h, 2, mean)
h.medians <- apply(h, 2, median)

h.df <- data.frame(Mean=h.means, Median=h.medians)

xtable(h.df, align="ccc")

h.df <- data.frame(SD=apply(h, 2, sd))

xtable(h.df, align="cc")
