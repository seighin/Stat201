require(ggplot2)

setwd(dirname(sys.frame(1)$ofile))

# density curve example
x <- seq(-3, 3, length=1000)
y <- dnorm(x)
dc <- data.frame(x=x, y=y)

g.dc <- ggplot(dc, aes(x=x, y=y))
g.dc <- g.dc + geom_area(aes(ymax=y), data=dc[dc$x<0,], fill='cadetblue', alpha=.6)
g.dc <- g.dc + geom_line(size=1)
g.dc <- g.dc + theme_bw() +xlab("") + ylab("Density")
g.dc <- g.dc + annotate("text", x=-2, y=.3, label="Area = P(X<0)")
g.dc <- g.dc + geom_curve(aes(x=-2, y=.27, xend=-.8, yend=.16), curvature = .2,
                          arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))

g.dc

ggsave('ch6_density_curve.png', width=4.5, height=2, units = "in")


# Uniform distribution
x <- seq(0,2,length=1000)
y <- dunif(x, 0, 2)
ud <- data.frame(x=x,y=y)

g.ud <- ggplot(ud, aes(x=x, y=y))
g.ud <- g.ud + geom_area(aes(ymax=y), data=ud[ud$x>1.3 & ud$x<1.7,], fill="plum3", alpha=.6)
g.ud <- g.ud + geom_segment(aes(x=1.3, y=0, xend=1.3, yend=.5), color="plum3") + 
    geom_segment(aes(x=1.7, y=0, xend=1.7, yend=.5), color="plum3")
g.ud <- g.ud + geom_line(size=1)
g.ud <- g.ud + theme_bw() + xlab("") + ylab("Density")
g.ud <- g.ud + scale_y_continuous(limits=c(0, .6))
g.ud <- g.ud + scale_x_continuous(breaks=c(0, .5, 1, 1.3, 1.5, 1.7, 2),
                                  labels=c("0.0", "0.5", "1.0", "u", "1.5", "v", "2.0"))

g.ud

ggsave('ch6_uniform.png', width=4, height=1.5, units = "in")

# Uniform distribution (bus)
x <- seq(0,30,length=1000)
y <- dunif(x, 0, 30)
y.c <- y[1]
bus <- data.frame(x=x,y=y)

g.bus <- ggplot(bus, aes(x=x, y=y))
g.bus <- g.bus + geom_area(aes(ymax=y), data=bus[bus$x>5 & bus$x<15,], fill="plum3", alpha=.6)
g.bus <- g.bus + geom_segment(aes(x=5, y=0, xend=5, yend=y.c), color="plum3") + 
    geom_segment(aes(x=15, y=0, xend=15, yend=y.c), color="plum3")
g.bus <- g.bus + geom_line(size=1)
g.bus <- g.bus + theme_bw() + xlab("Wait time") + ylab("Density")
g.bus <- g.bus + scale_y_continuous(limits=c(0, .04))
g.bus <- g.bus + scale_x_continuous(breaks=c(0, 5, 15, 30))

g.bus

ggsave('ch6_bus.png', width=4, height=1.5, units = "in")

# Normal distribution
x <- seq(-4, 4, length=1000)
y <- dnorm(x)
nd <- data.frame(x=x, y=y)

g.nd <- ggplot(nd, aes(x=x, y=y))
g.nd <- g.nd + geom_line(size=1)
g.nd <- g.nd + theme_bw() +xlab("") + ylab("")
g.nd <- g.nd + scale_x_continuous(breaks= -3:3,
                                  labels=expression(mu - 3*sigma, 
                                                    mu - 2*sigma,
                                                    mu - sigma,
                                                    mu,
                                                    mu + sigma,
                                                    mu + 2*sigma,
                                                    mu + 3*sigma))
g.nd <- g.nd + geom_segment(aes(x=x, y=y, xend=x, yend=rep(0,6)),
                            data=data.frame(x=c(-3:-1, 1:3), y=dnorm(c(-3:-1, 1:3))),
                            linetype="dashed")
g.nd <- g.nd + annotate("text", x=0, y=.2, label="68 %")
g.nd <- g.nd + geom_segment(aes(x=-.5, xend= -1, y=.2, yend=.2), size=.5)
g.nd <- g.nd + geom_segment(aes(x=.5, xend= 1, y=.2, yend=.2), size=.5) 
g.nd <- g.nd + annotate("text", x=0, y=.05, label="95 %")
g.nd <- g.nd + geom_segment(aes(x=-.5, xend= -2, y=.05, yend=.05), size=.5)
g.nd <- g.nd + geom_segment(aes(x=.5, xend= 2, y=.05, yend=.05), size=.5) 
g.nd <- g.nd + annotate("text", x=0, y=0, label="99.7 %")
g.nd <- g.nd + geom_segment(aes(x=-.5, xend= -3, y=0, yend=0), size=.5)
g.nd <- g.nd + geom_segment(aes(x=.5, xend= 3, y=0, yend=0), size=.5) 

g.nd

ggsave('ch6_normal.png', width=6, height=4, units = "in")

# Z  distribution
g.zd <- g.nd + xlab("Z")
g.zd <- g.zd + scale_x_continuous(breaks= -3:3)

g.zd

ggsave('ch6_zdist.png', width=6, height=4, units = "in")

# Standard normals
z <- seq(-3, 3, length=1000)
y <- dnorm(z)
sn <- data.frame(z=z, y=y)

g.norm <- ggplot(sn, aes(x=z, y=y))
g.norm <- g.norm + geom_line(size=1)
g.norm <- g.norm + theme_bw() +xlab("Z") + ylab("Density")
g.norm <- g.norm + scale_x_continuous(breaks= -3:3)

g.norm

# P(Z < .75)
g.ex01 <- g.norm + geom_area(aes(ymax=y), data=sn[sn$z<.75,], fill='cadetblue', alpha=.6)
g.ex01 <- g.ex01 + geom_segment(aes(x=.75, y=0, xend=.75, yend=dnorm(.75)), color="cadetblue")
g.ex01 <- g.ex01 + geom_line(size=1)

g.ex01

ggsave('ch6_ex01.png', width=4, height=1.5, units = "in")

# P(Z > -1.3)
g.ex02 <- g.norm + geom_area(aes(ymax=y), data=sn[sn$z>-1.3,], fill='cadetblue', alpha=.6)
g.ex02 <- g.ex02 + geom_segment(aes(x=-1.3, y=0, xend=-1.3, yend=dnorm(-1.3)), color="cadetblue")
g.ex02 <- g.ex02 + geom_line(size=1)

g.ex02

ggsave('ch6_ex02.png', width=4, height=1.5, units = "in")

# P(-0.2 < Z < 1.1)
g.ex03 <- g.norm + geom_area(aes(ymax=y), data=sn[sn$z>-.2 & sn$z< 1.1,], fill='cadetblue', alpha=.6)
g.ex03 <- g.ex03 + geom_segment(aes(x=-.2, y=0, xend=-.2, yend=dnorm(-.2)), color="cadetblue")
g.ex03 <- g.ex03 + geom_segment(aes(x=1.1, y=0, xend=1.1, yend=dnorm(1.1)), color="cadetblue")
g.ex03 <- g.ex03 + geom_line(size=1)

g.ex03

ggsave('ch6_ex03.png', width=4, height=1.5, units = "in")

# P(Z < z) = 0.35
g.ex04 <- g.norm + geom_area(aes(ymax=y), data=sn[sn$z< -.385 ,], fill='cadetblue', alpha=.6)
g.ex04 <- g.ex04 + geom_segment(aes(x=-.385, y=0, xend=-.385, yend=dnorm(-.385)), color="cadetblue")
g.ex04 <- g.ex04 + geom_line(size=1)
g.ex04 <- g.ex04 + scale_x_continuous(breaks=-.385, labels="?")
g.ex04 <- g.ex04 + annotate("text", x=-2.2, y=.35, label="Area = 0.35")
g.ex04 <- g.ex04 + geom_curve(aes(x=-2.2, y=.3, xend=-.8, yend=.12), curvature = .3,
                          arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.ex04

ggsave('ch6_ex04.png', width=4, height=1.5, units = "in")

# Alpha example
g.alpha <- g.norm + geom_area(aes(ymax=y), data=sn[sn$z> 1.645 ,], fill='cadetblue', alpha=.6)
g.alpha <- g.alpha + geom_segment(aes(x=1.645, y=0, xend=1.645, yend=dnorm(1.645)), color="cadetblue")
g.alpha <- g.alpha + geom_line(size=1)
g.alpha <- g.alpha + scale_x_continuous(breaks=1.645, labels=expression(z[alpha]))
g.alpha <- g.alpha + annotate("text", x=2.2, y=.35, label="Area = α")
g.alpha <- g.alpha + geom_curve(aes(x=2.2, y=.3, xend=1.9, yend=.03), curvature = -.3,
                              arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.alpha

ggsave('ch6_alpha.png', width=4, height=1.5, units = "in")

# Symmetry
g.sym <- g.norm + geom_area(aes(ymax=y), data=sn[sn$z> 1.5,], fill='cadetblue', alpha=.6)
g.sym <- g.sym + geom_area(aes(ymax=y), data=sn[sn$z < -1.5,], fill='cadetblue', alpha=.6)
g.sym <- g.sym + geom_segment(aes(x=-1.5, y=0, xend=-1.5, yend=dnorm(-1.5)), color="cadetblue")
g.sym <- g.sym + geom_segment(aes(x=1.5, y=0, xend=1.5, yend=dnorm(1.5)), color="cadetblue")
g.sym <- g.sym + geom_line(size=1)
g.sym <- g.sym + scale_x_continuous(breaks=c(-1.5, 1.5), labels=c("-c", "c"))
g.sym <- g.sym + annotate("text", x=2.2, y=.35, label="P(X>c)")
g.sym <- g.sym + geom_curve(aes(x=2.2, y=.3, xend=1.9, yend=.03), curvature = -.3,
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.sym <- g.sym + annotate("text", x=-2.2, y=.35, label="P(X>-c)")
g.sym <- g.sym + geom_curve(aes(x=-2.2, y=.3, xend=-1.9, yend=.03), curvature = .3,
                            arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.sym

ggsave('ch6_sym.png', width=4, height=1.5, units = "in")


# Critical value alpha
g.crit1 <- g.norm + geom_area(aes(ymax=y), data=sn[sn$z> 1.645,], fill='cadetblue', alpha=.6)
g.crit1 <- g.crit1 + geom_segment(aes(x=1.645, y=0, xend=1.645, yend=dnorm(1.645)), color="cadetblue")
g.crit1 <- g.crit1 + geom_line(size=1)
g.crit1 <- g.crit1 + scale_x_continuous(breaks=1.645, labels=expression(z[alpha]==1.645))
g.crit1 <- g.crit1 + annotate("text", x=2.2, y=.35, label="Area = 0.05")
g.crit1 <- g.crit1 + geom_curve(aes(x=2.2, y=.3, xend=2.3, yend=.03), curvature = -.3,
                               arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.crit1

ggsave('ch6_crit1.png', width=4, height=1.5, units = "in")

# Critical value alpha/2
g.crit2 <- g.norm + geom_area(aes(ymax=y), data=sn[sn$z> 1.96,], fill='cadetblue', alpha=.6)
g.crit2 <- g.crit2 + geom_area(aes(ymax=y), data=sn[sn$z < -1.96,], fill='cadetblue', alpha=.6)
g.crit2 <- g.crit2 + geom_segment(aes(x=-1.96, y=0, xend=-1.96, yend=dnorm(-1.96)), color="cadetblue")
g.crit2 <- g.crit2 + geom_segment(aes(x=1.96, y=0, xend=1.96, yend=dnorm(1.96)), color="cadetblue")
g.crit2 <- g.crit2 + geom_line(size=1)
g.crit2 <- g.crit2 + scale_x_continuous(breaks=c(-1.96, 1.96), labels=expression(-z[alpha/2]==-1.96, z[alpha/2]==1.96))
g.crit2 <- g.crit2 + annotate("text", x=2.2, y=.35, label="Area = 0.025")
g.crit2 <- g.crit2 + geom_curve(aes(x=2.2, y=.3, xend=2.3, yend=.03), curvature = -.3,
                            arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.crit2 <- g.crit2 + annotate("text", x=-2.2, y=.35, label="Area = 0.025")
g.crit2 <- g.crit2 + geom_curve(aes(x=-2.2, y=.3, xend=-2.3, yend=.03), curvature = .3,
                            arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.crit2

ggsave('ch6_crit2.png', width=4, height=1.5, units = "in")

# Sample dist histo
xb <- data.frame(x.bar= c(1, 1.5, 2, 1.5, 2, 2.5, 2, 2.5, 3))

g.xb <- ggplot(xb, aes(x=x.bar, y=..density..))
g.xb <- g.xb + geom_histogram(bins=5, color="black", fill="cadetblue")
g.xb <- g.xb + xlab(expression(bar(x))) + ylab("Density")

g.xb

ggsave('ch6_samp_dist.png', width=4, height=1, units = "in")
