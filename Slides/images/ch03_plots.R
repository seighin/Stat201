require(ggplot2)

# Set working dir to script dir
setwd(dirname(sys.frame(1)$ofile))

# Boxplot of age data
age <- c(22, 32, 46, 50, 33, 38, 20, 24)


png('ch03_boxplot.png',
    width=300, height=200)
mar.old <-par(mar = c(3,1,1,1) + 0.1)

boxplot(age, horizontal = TRUE, col="cadetblue")
 
par(mar=mar.old)
dev.off()

# Var comparison
set.seed(42)
x.1 <- rnorm(2000, 0, 1)
x.2 <- rnorm(2000, 0, 3)

x.var <- data.frame(x=c(x.1, x.2), lab=c(rep("SD = 1",2000), rep("SD = 3",2000)))

g.var <- ggplot(x.var, aes(x=x, y=..density..))
g.var <- g.var + geom_histogram(,binwidth=0.5,color="black", fill="cadetblue")
g.var <- g.var + facet_grid(lab ~ .)
g.var <- g.var + theme_bw() +xlab("") + ylab("")
g.var

ggsave('ch03_var_diff.png', width=4.25, height=2.75, units = "in")
