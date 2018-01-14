require(ggplot2)

setwd(dirname(sys.frame(1)$ofile))

# Sunlight histogram
sun <- read.csv('data/sunlight data.csv', 
                header=TRUE, stringsAsFactors = FALSE)

sunlight <- sun$Avg.Daily.Sunlight

png('ch02_sunlight_hist.png',
    width=475, height=300)

hist(sunlight, breaks = 10,
     main = "Avg sunlight in MN from 2007 to 2009",
     xlab = "Sunlight (KJ/m^2)",
     col="red")

dev.off()


# Scatterplot and correlation
x <- runif(20, 0, 10)

y.pos <- x + rnorm(20)
y.none <- runif(20, 0, 10)
y.neg <- 10 - x + rnorm(20)

sp.df <- data.frame(x = rep(x,3), 
                    y = c(y.pos, y.none, y.neg), 
                    Correlation = factor(c(rep("Positive",20), rep("None",20), rep("Negative",20) )))

g.sp <- ggplot(sp.df, aes(x=x, y=y, color=Correlation))
g.sp <- g.sp + geom_point(shape=19)
g.sp <- g.sp + facet_grid(. ~ Correlation)
g.sp <- g.sp + theme_bw() + guides(color=FALSE)

g.sp
ggsave('ch02_scatter_cor.png', width=4.5, height=2.5, units = "in")
