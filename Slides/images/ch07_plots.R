require(ggplot2)

setwd(dirname(sys.frame(1)$ofile))

# Critical values
x <- seq(-3, 3, length=1000)
y <- dnorm(x)
cv <- data.frame(x=x, y=y)

g.cv <- ggplot(cv, aes(x=x, y=y))
g.cv <- g.cv + geom_area(aes(ymax=y), data=cv[cv$x> -1.96 & cv$x< 1.96,], fill='cadetblue', alpha=.6)
g.cv <- g.cv + geom_segment(aes(x=-1.96, y=0, xend=-1.96, yend=dnorm(-1.96)), color="cadetblue")
g.cv <- g.cv + geom_segment(aes(x=1.96, y=0, xend=1.96, yend=dnorm(1.96)), color="cadetblue")
g.cv <- g.cv + geom_line(size=1)
g.cv <- g.cv + theme_bw() +xlab("") + ylab("Density")
g.cv <- g.cv + scale_x_continuous(breaks=c(-1.96, 1.96), labels=expression(-z[alpha/2], z[alpha/2]))
g.cv <- g.cv + annotate("text", x=-2, y=.3, label="Area = 1-α")
g.cv <- g.cv + geom_curve(aes(x=-2, y=.27, xend=-.8, yend=.16), curvature = .2,
                          arrow=arrow(angle=20, length=unit(.1,"in"), type="closed"))

g.cv

ggsave('ch7_crit_values.png', width=4.5, height=2, units = "in")


# Number line
nl <- data.frame(x=c(-1.5, 0, 1.5), y=rep(1,3))

g.nl <- ggplot(nl, aes(x=x, y=y))
#g.nl <- g.nl + geom_errorbarh(aes(xmin=-1, xmax=1, height=.1))
g.nl <- g.nl + geom_point(data=nl[nl$x==0,], shape=21, size = 5, color="black", fill="white", stroke=1)
g.nl <- g.nl + geom_point(aes(x=-1, y=1), shape=40, size=5) + 
    geom_segment(aes(x=-.1, xend=-1, y=1, yend=1)) +
    annotate("text", x=-.5, y = 1.15, label="ME")
g.nl <- g.nl + geom_point(aes(x=1, y=1), shape=41, size=5)+
    geom_segment(aes(x=.1, xend=1, y=1, yend=1)) +
    annotate("text", x=.5, y = 1.15, label="ME")
g.nl <- g.nl + scale_x_continuous(limits=c(-1.5, 1.5), breaks=c(-1,0,1),
                                  labels=expression(L[ci] == x - ME, x, U[ci] == x + ME))
g.nl <- g.nl + scale_y_continuous(limits=c(.75, 1.5))
g.nl <- g.nl + theme_void() + xlab("") + ylab("")
g.nl <- g.nl + theme(axis.text.x=element_text(color="black"), axis.ticks.x=element_line())

g.nl

ggsave('ch7_ci_numline.png', width=4.5, height=1, units = "in")


# t distribution
x <- seq(-3, 3, length=1000)
y.norm <- dnorm(x)
y.t1 <- dt(x, 3)
y.t2 <- dt(x, 7)
td <- data.frame(x=rep(x,3), y=c(y.norm, y.t1, y.t2), 
                 Distribution=c(rep("Normal",1000), rep("t, 3 df",1000), rep("t, 7 df", 1000)))

g.td <- ggplot(td, aes(x=x, y=y, color=Distribution, linetype=Distribution))
g.td <- g.td + geom_line(size=1)
g.td <- g.td + theme_bw() + xlab("") + ylab("Density")

g.td

ggsave('ch7_t_dist.png', width=4.5, height=3, units = "in")
