source("Solution_1.R")

csvfile1 = "ShenyangPM20100101_20151231.csv"
csvfile2 = "ChengduPM20100101_20151231.csv"
csvfile3 = "BeijingPM20100101_20151231.csv"
csvfile4 = "GuangzhouPM20100101_20151231.csv"
csvfile5 = "ShanghaiPM20100101_20151231.csv"

DataClean(csvfile1, csvfile2, csvfile3, csvfile4, csvfile5)

predictpm25(25,"Beijing")
