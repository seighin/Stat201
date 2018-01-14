# Set working dir to script dir
setwd(dirname(sys.frame(1)$ofile))

# Local Climatological Data from https://www.ncdc.noaa.gov/cdo-web/datatools/lcd
#   {Select by zip 55107, add to cart, go to cart, lcd csv, select date range (last month)}
#   should be saved as lcd.csv
lcd <- read.csv('../source_data/lcd.csv')

t <- lcd[!is.na(lcd$HOURLYDRYBULBTEMPF), c('DATE', 'HOURLYDRYBULBTEMPF')]
t$just.date <- as.integer(substr(as.character(t$DATE),9,10))

t.list <- tapply(t$HOURLYDRYBULBTEMPF , factor(t$just.date), max)

temp <- data.frame(day = dimnames(t.list)[[1]], max.temp = t.list, row.names = NULL)

###
### Update file name with correct month
write.csv(temp, '../max_temps_dec17.csv', row.names = FALSE)


