require(ggplot2)

setwd(dirname(sys.frame(1)$ofile))

# Chi-square with critical value
x <- seq(0, 15, length=1000)
y <- dchisq(x, df=5)
cs <- data.frame(x=x, y=y)

cs.crit <- round(qchisq(.95, 5),2)

g.cs <- ggplot(cs, aes(x=x, y=y))
g.cs <- g.cs + geom_area(aes(ymax=y), data=cs[cs$x> cs.crit,], fill='cadetblue', alpha=.6)
g.cs <- g.cs + geom_segment(aes(x=cs.crit, y=0, xend=cs.crit, yend=dnorm(cs.crit,5)), color="cadetblue")
g.cs <- g.cs + geom_line(size=1)
g.cs <- g.cs + scale_x_continuous(breaks=c(0, 5, cs.crit, 15), 
                                  labels=expression(0, 5, chi[list(alpha, k-1)]^2, 15))
g.cs <- g.cs + annotate("text", x=13, y=.1, label="Area = α")
g.cs <- g.cs + geom_curve(aes(x=13, y=.09, xend=13.3, yend=.015), curvature = -.3,
                          arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.cs <- g.cs + theme_bw() +xlab("") + ylab("Density") + ggtitle("df = 5")
g.cs

ggsave('ch11_chi_square_crit.png', width=4.5, height=3, units = "in")
