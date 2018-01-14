require(ggplot2)

setwd(dirname(sys.frame(1)$ofile))

# Sig level
x <- seq(-3.5, 3.5, length=1000)
y <- dnorm(x)
pv <- data.frame(x=x, y=y)

g.pv <- ggplot(pv, aes(x=x, y=y))

# Upper
g.pv <- g.pv + geom_area(aes(ymax=y), data=pv[pv$x> 1.5,], fill='cadetblue', alpha=.6)
g.pv <- g.pv + geom_segment(aes(x=1.5, y=0, xend=1.5, yend=dnorm(1.5)), color="cadetblue")
g.pv <- g.pv + geom_line(size=1)
g.pv <- g.pv + scale_x_continuous(breaks=c(1.5), 
                                        labels=c("test statistic"))
g.pv <- g.pv + annotate("text", x=2.2, y=.35, label="Area = p-value")
g.pv <- g.pv + geom_curve(aes(x=2.2, y=.3, xend=2.3, yend=.03), curvature = -.3,
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.pv <- g.pv + theme_bw() +xlab("") + ylab("Density")
g.pv


ggsave('ch08_pvalue.png', width=4.5, height=1.5, units = "in")

# Sig level
x <- seq(-3.5, 3.5, length=1000)
y <- dnorm(x)
sig <- data.frame(x=x, y=y)

g.sig <- ggplot(sig, aes(x=x, y=y))

# Upper
g.sig.u <- g.sig + geom_area(aes(ymax=y), data=cv[cv$x> 1.645,], fill='cadetblue', alpha=.6)
g.sig.u <- g.sig.u + geom_segment(aes(x=1.645, y=0, xend=1.645, yend=dnorm(1.645)), color="cadetblue")
g.sig.u <- g.sig.u + geom_line(size=1)
g.sig.u <- g.sig.u + scale_x_continuous(breaks=c( 1.645), 
                                  labels=expression( z[alpha]))
g.sig.u <- g.sig.u + annotate("text", x=2.2, y=.35, label="Area = α")
g.sig.u <- g.sig.u + geom_curve(aes(x=2.2, y=.3, xend=2.3, yend=.03), curvature = -.3,
                          arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.sig.u <- g.sig.u + theme_bw() +xlab("") + ylab("Density") + ggtitle(expression(H[a]: mu > mu[0]))
g.sig.u


ggsave('ch08_sig_up.png', width=4.5, height=1.45, units = "in")

# Lower
g.sig.l <- g.sig + geom_area(aes(ymax=y), data=cv[cv$x< -1.645,], fill='cadetblue', alpha=.6)
g.sig.l <- g.sig.l + geom_segment(aes(x=-1.645, y=0, xend=-1.645, yend=dnorm(-1.645)), color="cadetblue")
g.sig.l <- g.sig.l + geom_line(size=1)
g.sig.l <- g.sig.l + scale_x_continuous(breaks=c(- 1.645), 
                                        labels=expression( -z[alpha]))
g.sig.l <- g.sig.l + annotate("text", x=-2.2, y=.35, label="Area = α")
g.sig.l <- g.sig.l + geom_curve(aes(x=-2.2, y=.3, xend=-2.3, yend=.03), curvature = .3,
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.sig.l <- g.sig.l + theme_bw() +xlab("") + ylab("Density") + ggtitle(expression(H[a]: mu < mu[0]))

g.sig.l

ggsave('ch08_sig_low.png', width=4.5, height=1.45 , units = "in")

# Two sided
g.sig.2 <- g.sig + geom_area(aes(ymax=y), data=cv[cv$x> 1.96,], fill='cadetblue', alpha=.6)
g.sig.2 <- g.sig.2 + geom_area(aes(ymax=y), data=cv[cv$x< -1.96,], fill='cadetblue', alpha=.6)
g.sig.2 <- g.sig.2 + geom_segment(aes(x=1.96, y=0, xend=1.96, yend=dnorm(1.96)), color="cadetblue")
g.sig.2 <- g.sig.2 + geom_segment(aes(x=-1.96, y=0, xend=-1.96, yend=dnorm(-1.96)), color="cadetblue")
g.sig.2 <- g.sig.2 + geom_line(size=1)
g.sig.2 <- g.sig.2 + scale_x_continuous(breaks=c( -1.96, 1.96), 
                                        labels=expression( -z[alpha/2], z[alpha/2]))
g.sig.2 <- g.sig.2 + annotate("text", x=2.2, y=.35, label="Area = α/2")
g.sig.2 <- g.sig.2 + geom_curve(aes(x=2.2, y=.3, xend=2.3, yend=.03), curvature = -.3,
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.sig.2 <- g.sig.2 + annotate("text", x=-2.2, y=.35, label="Area = α/2")
g.sig.2 <- g.sig.2 + geom_curve(aes(x=-2.2, y=.3, xend=-2.3, yend=.03), curvature = .3,
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.sig.2 <- g.sig.2 + theme_bw() +xlab("") + ylab("Density") + ggtitle(expression(H[a]: mu != mu[0]))
g.sig.2


ggsave('ch08_sig_2.png', width=4.5, height=2, units = "in")


# Critical values
x <- seq(-3.5, 3.5, length=1000)
y <- dnorm(x)
cv <- data.frame(x=x, y=y)

g.cv <- ggplot(cv, aes(x=x, y=y))

g.cv <- g.cv + geom_area(aes(ymax=y), data=cv[cv$x> 1.96,], fill='cadetblue', alpha=.6)
g.cv <- g.cv + geom_area(aes(ymax=y), data=cv[cv$x < -1.96,], fill='cadetblue', alpha=.6)
g.cv <- g.cv + geom_segment(aes(x=-1.96, y=0, xend=-1.96, yend=dnorm(-1.96)), color="cadetblue")
g.cv <- g.cv + geom_segment(aes(x=1.96, y=0, xend=1.96, yend=dnorm(1.96)), color="cadetblue")
g.cv <- g.cv + geom_line(size=1)
g.cv <- g.cv + scale_x_continuous(breaks=c(-2.8, -1.96, 1.96), 
                                  labels=expression(-2.8, -z[alpha/2], z[alpha/2]))
g.cv <- g.cv + annotate("text", x=2.2, y=.35, label="Area = α/2")
g.cv <- g.cv + geom_curve(aes(x=2.2, y=.3, xend=2.3, yend=.03), curvature = -.3,
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.cv <- g.cv + annotate("text", x=-2.2, y=.35, label="Area = α/2")
g.cv <- g.cv + geom_curve(aes(x=-2.2, y=.3, xend=-2.3, yend=.03), curvature = .3,
                                arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))
g.cv <- g.cv + theme_bw() +xlab("") + ylab("Density")

g.cv

ggsave('ch08_cv.png', width=4.5, height=1.75, units = "in")
