require(ggplot2)

setwd(dirname(sys.frame(1)$ofile))

# Two sided CI
x <- seq(-3.5, 3.5, length=1000)
y <- dnorm(x)
ci.2 <- data.frame(x=x, y=y)

g.ci.2 <- ggplot(ci.2, aes(x=x, y=y))

# Upper
g.ci.2 <- g.ci.2 + geom_area(data=ci.2[ci.2$x> 1.96,], fill='cadetblue', alpha=.6)
g.ci.2 <- g.ci.2 + geom_area(data=ci.2[ci.2$x< -1.96,], fill='cadetblue', alpha=.6)
g.ci.2 <- g.ci.2 + geom_segment(aes(x=1.96, y=0, xend=1.96, yend=dnorm(1.96)), color="cadetblue")
g.ci.2 <- g.ci.2 + geom_segment(aes(x=-1.96, y=0, xend=-1.96, yend=dnorm(-1.96)), color="cadetblue")
g.ci.2 <- g.ci.2 + geom_line(size=1)
g.ci.2 <- g.ci.2 + scale_x_continuous(breaks=c( 0, 2.3), 
                                        labels=expression(bar(x), mu[0]))
g.ci.2 <- g.ci.2 + annotate("text", x=0, y=.04, label="CI (1-α)")
g.ci.2 <- g.ci.2 + geom_segment(aes(x=-.6, y=.04, xend=-1.96, yend=.04),
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="open"))
g.ci.2 <- g.ci.2 + geom_segment(aes(x=.6, y=.04, xend=1.96, yend=.04),
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="open"))
g.ci.2 <- g.ci.2 + annotate("text", x=2.2, y=.35, label="Area = α/2")
g.ci.2 <- g.ci.2 + geom_curve(aes(x=2.2, y=.3, xend=2.3, yend=.03), curvature = -.3,
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.ci.2 <- g.ci.2 + annotate("text", x=-2.2, y=.35, label="Area = α/2")
g.ci.2 <- g.ci.2 + geom_curve(aes(x=-2.2, y=.3, xend=-2.3, yend=.03), curvature = .3,
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.ci.2 <- g.ci.2 + theme_bw() +xlab("") + ylab("Density") + ggtitle(expression(H[a]: mu != mu[0]))
g.ci.2


ggsave('ch09_two_ci.png', width=4.5, height=2, units = "in")

# One sided CI
x <- seq(-3.5, 3.5, length=1000)
y <- dnorm(x)
ci.1 <- data.frame(x=x, y=y)

g.ci.1 <- ggplot(ci.2, aes(x=x, y=y))

# Upper
g.ci.1 <- g.ci.1 + geom_area(data=ci.1[ci.1$x> 1.64,], fill='cadetblue', alpha=.6)
g.ci.1 <- g.ci.1 + geom_area(data=ci.1[ci.1$x< -1.64,], fill='cadetblue', alpha=.6)
g.ci.1 <- g.ci.1 + geom_segment(aes(x=1.64, y=0, xend=1.64, yend=dnorm(1.64)), color="cadetblue")
g.ci.1 <- g.ci.1 + geom_segment(aes(x=-1.64, y=0, xend=-1.64, yend=dnorm(-1.64)), color="cadetblue")
g.ci.1 <- g.ci.1 + geom_line(size=1)
g.ci.1 <- g.ci.1 + scale_x_continuous(breaks=c( 0, 2.3), 
                                      labels=expression(bar(x), mu[0]))
g.ci.1 <- g.ci.1 + annotate("text", x=0, y=.04, label="CI (1-2α)")
g.ci.1 <- g.ci.1 + geom_segment(aes(x=-.65, y=.04, xend=-1.64, yend=.04),
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="open"))
g.ci.1 <- g.ci.1 + geom_segment(aes(x=.65, y=.04, xend=1.64, yend=.04),
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="open"))
g.ci.1 <- g.ci.1 + annotate("text", x=2.2, y=.35, label="Area = α")
g.ci.1 <- g.ci.1 + geom_curve(aes(x=2.2, y=.3, xend=2.3, yend=.03), curvature = -.3,
                              arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.ci.1 <- g.ci.1 + annotate("text", x=-2.2, y=.35, label="Area = α")
g.ci.1 <- g.ci.1 + geom_curve(aes(x=-2.2, y=.3, xend=-2.3, yend=.03), curvature = .3,
                              arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.ci.1 <- g.ci.1 + theme_bw() +xlab("") + ylab("Density") + ggtitle(expression(H[a]: mu < mu[0]))
g.ci.1


ggsave('ch09_one_ci.png', width=4.5, height=2, units = "in")
