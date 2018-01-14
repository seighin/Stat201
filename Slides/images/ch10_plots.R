require(ggplot2)

setwd(dirname(sys.frame(1)$ofile))

set.seed(23)

# Correlation

x <- 1:10
x.db <- seq(1,10,.5
            )
# r = 1
y.pos1 <-  x
pos1 <- data.frame(x=x, y=y.pos1)

g.pos1 <- ggplot(pos1, aes(x=x, y=y))
g.pos1 <- g.pos1 + geom_point()
g.pos1 <- g.pos1 + ggtitle("r = 1") + coord_fixed()+ theme_bw()

g.pos1
ggsave('ch10_cor_pos1.png', width=2.25, height=2.5, units = "in")

x.pos2 <- 5+x
y.pos2 <- 2 * x
pos2 <- data.frame(x=x.pos2, y=y.pos2)

g.pos2 <- ggplot(pos2, aes(x=x, y=y))
g.pos2 <- g.pos2 + geom_point()
g.pos2 <- g.pos2 + ggtitle("r = 1") + coord_fixed() + theme_bw()
g.pos2 <- g.pos2 + scale_x_continuous(limits = c(1,20))

g.pos2
ggsave('ch10_cor_pos2.png', width=2.25, height=2.5, units = "in")


# r = -1
y.neg1 <- 11 - x
neg1 <- data.frame(x=x, y=y.neg1)

g.neg1 <- ggplot(neg1, aes(x=x, y=y))
g.neg1 <- g.neg1 + geom_point()
g.neg1 <- g.neg1 + ggtitle("r = -1") + coord_fixed()+ theme_bw()

g.neg1
ggsave('ch10_cor_neg1.png', width=2.25, height=2.5, units = "in")

y.neg2 <- 8 - .25 * x
neg2 <- data.frame(x=x, y=y.neg2)

g.neg2 <- ggplot(neg2, aes(x=x, y=y))
g.neg2 <- g.neg2 + geom_point()
g.neg2 <- g.neg2 + ggtitle("r = -1") + coord_fixed()+ theme_bw()
g.neg2 <- g.neg2 + scale_y_continuous(limits=c(1,10))

g.neg2
ggsave('ch10_cor_neg2.png', width=2.25, height=2.5, units = "in")

# r = 0
y.zero <- rep(5,10)
zero <- data.frame(x=x, y=y.zero)

g.zero <- ggplot(zero, aes(x=x, y=y))
g.zero <- g.zero + geom_point()
g.zero <- g.zero + ggtitle("r = 0") + coord_fixed()+ theme_bw()
g.zero <- g.zero + scale_y_continuous(limits = c(1,10))

g.zero
ggsave('ch10_cor_zero.png', width=2.25, height=2.5, units = "in")

x.zerox <- rep(5,10)
y.zerox <- 1:10
zerox <- data.frame(x=x.zerox, y=y.zerox)

g.zerox <- ggplot(zerox, aes(x=x, y=y))
g.zerox <- g.zerox + geom_point()
g.zerox <- g.zerox + ggtitle("r = 0") + coord_fixed()+ theme_bw()
g.zerox <- g.zerox + scale_x_continuous(limits = c(1,10))

g.zerox
ggsave('ch10_cor_zerox.png', width=2.25, height=2.5, units = "in")


y.para <- .41 * ((5.5-x))^2 + 1
para <- data.frame(x=x, y=y.para)
para.r <- cor(para$x, para$y)

g.para <- ggplot(para, aes(x=x, y=y))
g.para <- g.para + geom_point()
g.para <- g.para + ggtitle(paste("r =", round(para.r, 3))) + coord_fixed()+ theme_bw()
g.para <- g.para + scale_y_continuous(limits = c(1, 10))

g.para
ggsave('ch10_cor_para.png', width=2.25, height=2.5, units = "in")

y.cube <- .11 * ((x.db-5.5))^3  - 1.75 * (x.db-5.5)
cube <- data.frame(x=x.db, y=y.cube)
cube.r <- cor(cube$x, cube$y)

g.cube <- ggplot(cube, aes(x=x, y=y))
g.cube <- g.cube + geom_point()
g.cube <- g.cube + ggtitle(paste("r =", round(cube.r, 3))) + coord_fixed() + theme_bw()
g.cube <- g.cube + scale_y_continuous(limits = c(-4.5, 4.5))

g.cube
ggsave('ch10_cor_cube.png', width=2.25, height=2.5, units = "in")

y.quad <- ((x.db-5.5)*.45)^4 - ((x.db-5.5)*.8)^2 
quad <- data.frame(x=x.db, y=y.quad)
quad.r <- cor(quad$x, quad$y)

g.quad <- ggplot(quad, aes(x=x, y=y))
g.quad <- g.quad + geom_point()
g.quad <- g.quad + ggtitle(paste("r =", round(quad.r, 3))) + coord_fixed() + theme_bw()
g.quad <- g.quad + scale_y_continuous(limits = c(-4.5, 4.5))

g.quad
ggsave('ch10_cor_quad.png', width=2.25, height=2.5, units = "in")

# r approx .95
y.pos3 <- x.db + rnorm(19, 0, 1.3)
pos3 <- data.frame(x=x.db, y=y.pos3)
pos3.r = cor(pos3$x, pos3$y)

g.pos3 <- ggplot(pos3, aes(x=x, y=y))
g.pos3 <- g.pos3 + geom_point()
g.pos3 <- g.pos3 + ggtitle(paste("r =", round(pos3.r, 3))) + coord_fixed() + theme_bw()
g.pos3 <- g.pos3 + scale_y_continuous(limits = c(.5, 9.5))
g.pos3
ggsave('ch10_cor_pos3.png', width=2.25, height=2.5, units = "in")

# r approx -0.65
y.neg3 <- 10 - .5 * x.db + rnorm(19,0,3)
neg3 <- data.frame(x=x.db, y=y.neg3)
neg3.r = cor(neg3$x, neg3$y)

g.neg3 <- ggplot(neg3, aes(x=x, y=y))
g.neg3 <- g.neg3 + geom_point()
g.neg3 <- g.neg3 + ggtitle(paste("r =", round(neg3.r, 3))) + coord_fixed() + theme_bw()
g.neg3 <- g.neg3 + scale_y_continuous(limits = c(0,14)) + scale_x_continuous(limits = c(-1,13))
g.neg3
ggsave('ch10_cor_neg3.png', width=2.25, height=2.5, units = "in")

# r approx 0
set.seed(42)
temp <- rnorm(10)
y.zero2 <-  .65 * rnorm(19,5,2.5)
zero2 <- data.frame(x=x.db, y=y.zero2)
zero2.r = cor(zero2$x, zero2$y)

g.zero2 <- ggplot(zero2, aes(x=x, y=y))
g.zero2  <- g.zero2 + geom_point()
g.zero2 <- g.zero2 + ggtitle(paste("r =", round(zero2.r, 3))) + coord_fixed()+ theme_bw()
g.zero2 <- g.zero2 + scale_y_continuous(limits = c(-2, 8)) + scale_x_continuous(limits = c(0.5,10.5))

g.zero2
ggsave('ch10_cor_zero2.png', width=2.25, height=2.5, units = "in")

# r with outlier
y.out <- x + rnorm(10, 0, 1.5)
out.wo <- data.frame(x=x, y=y.out)
out.wo.r = cor(out.wo$x, out.wo$y)

g.out.wo <- ggplot(out.wo, aes(x=x, y=y))
g.out.wo <- g.out.wo + geom_point()
g.out.wo <- g.out.wo + ggtitle(paste("r =", round(out.wo.r, 3))) + coord_fixed()+ theme_bw()
g.out.wo <- g.out.wo + scale_x_continuous(limits = c(1,12)) + scale_y_continuous(limits = c(-1.5,9.5))

g.out.wo
ggsave('ch10_cor_wo_out.png', width=2.25, height=2.5, units = "in")

out.w <- rbind(out.wo, c(12,0))
out.w.r = cor(out.w$x, out.w$y)

g.out.w <- ggplot(out.w, aes(x=x, y=y))
g.out.w <- g.out.w + geom_point() + geom_point(data=out.w[11,], col="red")
g.out.w <- g.out.w + ggtitle(paste("r =", round(out.w.r, 3))) + coord_fixed()+ theme_bw()
g.out.w <- g.out.w + scale_x_continuous(limits = c(1,12)) + scale_y_continuous(limits = c(-1.5,9.5))

g.out.w
ggsave('ch10_cor_w_out.png', width=2.25, height=2.5, units = "in")

x <- seq(2,8,2)
y <- c(1, 6, 7, 12)
calcr1 <- data.frame(x=x, y=y)
calcr1.r = cor(calcr1$x, calcr1$y)

g.calcr1 <- ggplot(calcr1, aes(x=x, y=y))
g.calcr1 <- g.calcr1 + geom_point() 
g.calcr1 <- g.calcr1 + ggtitle(paste("r =", round(calcr1.r, 3))) + coord_fixed()+ theme_bw()
g.calcr1 <- g.calcr1 + scale_x_continuous(limits = c(-0.5,10.5)) + scale_y_continuous(limits = c(1,12))

g.calcr1
ggsave('ch10_cor_calcr1.png', width=2.25, height=2.5, units = "in")

u <- seq(2,8,2)
w <- c(7, 12, 1, 6)
calcr2 <- data.frame(u=u, w=w)
calcr2.r = cor(calcr2$u, calcr2$w)

g.calcr2 <- ggplot(calcr2, aes(x=u, y=w))
g.calcr2 <- g.calcr2 + geom_point() 
g.calcr2 <- g.calcr2 + ggtitle(paste("r =", round(calcr2.r, 3))) + coord_fixed()+ theme_bw()
g.calcr2 <- g.calcr2 + scale_x_continuous(limits = c(-0.5,10.5)) + scale_y_continuous(limits = c(1,12))

g.calcr2
ggsave('ch10_cor_calcr2.png', width=2.25, height=2.5, units = "in")

# Galton data
galton <- read.csv('~/Dropbox/Classes - Prof/Stat 201/Materials/MyStatLab data/Galton-father-son.csv')
galton.r <- cor(galton$father, galton$son)

g.gal.cor <- ggplot(galton, aes(x=father, y=son))
g.gal.cor <- g.gal.cor + geom_point(alpha=0.5) 
g.gal.cor <- g.gal.cor + ggtitle(paste("r =", round(galton.r, 3))) + coord_fixed()+ theme_bw()

g.gal.cor
ggsave('ch10_cor_galton.png', width=2.25, height=2.5, units = "in")

# Spurious correlation
maine.div <- c(5, 4.7, 4.6, 4.4, 4.3, 4.1, 4.2, 4.2, 4.2, 4.1)
marg.cons <- c(8.2, 7, 6.5, 5.3, 5.2, 4, 4.6, 4.5, 4.2, 3.7)
spur.cor <- data.frame(x=marg.cons, y=maine.div)
spur.cor.r <- cor(spur.cor$x, spur.cor$y)

g.spur.cor <- ggplot(spur.cor, aes(x=x, y=y))
g.spur.cor <- g.spur.cor + geom_point() 
g.spur.cor <- g.spur.cor + ggtitle(paste("r =", round(spur.cor.r, 3)))+ theme_bw()
g.spur.cor <- g.spur.cor + xlab("Margine consumption (lbs)") + ylab("Maine divorce rate (per 1000)")

g.spur.cor
ggsave('ch10_cor_spur2.png', width=2.5, height=2.5, units = "in")


### Regression

# Galton
galton.lm <- lm(son ~ father, data=galton)
galton.int <- galton.lm$coefficients[1]
galton.slp <- galton.lm$coefficients[2]

g.gal.reg <- g.gal.cor + geom_abline(intercept = galton.int,
                                     slope = galton.slp,
                                     size = 1.25, col="red")
g.gal.reg <- g.gal.reg + ggtitle(paste("y =", round(galton.int,2), "+", round(galton.slp,2),"x"))

g.gal.reg
ggsave('ch10_reg_galton.png', width=3, height=3, units = "in")

# Bears
bears <- read.csv('~/Dropbox/Classes - Prof/Stat 201/Materials/MyStatLab data/bears.csv')
bears.lm <- lm(WEIGHT ~ LENGTH, data=bears)
bears.int <- bears.lm$coefficients[1]
bears.slp <- bears.lm$coefficients[2]

g.bears <- ggplot(bears, aes(x=LENGTH, y=WEIGHT))
g.bears <- g.bears + geom_point() 
g.bears <- g.bears + theme_bw()

g.bears <- g.bears + geom_abline(intercept = bears.int,
                                     slope = bears.slp,
                                     size = 1.25, col="red")
#g.bears <- g.bears + ggtitle(paste("y =", round(bears.int,2), "+", round(bears.slp,2),"x"))
g.bears <- g.bears + xlab("Height (in)") + ylab("Weight (lbs)")

g.bears
ggsave('ch10_reg_bears.png', width=2.5, height=2.5, units = "in")

