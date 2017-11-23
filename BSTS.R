library(xts)
library(bsts)

csvfile = "BeijingPM20100101_20151231.csv"

file = read.csv(csvfile)

PM25 = na.contiguous(file$PM_US.Post)

file = file[50609:51972,]

TEMP = file$TEMP
TEMP[is.na(TEMP)] <- 20

file = t(rbind(PM25, TEMP))
file = data.frame(file)
file$PM25 = log(file$PM25)

ss <- AddLocalLinearTrend(list(), file$PM25)

model <- bsts(file$PM25 ~ ., state.specification = ss, data = file, niter = 1000)

plot(model, "components")