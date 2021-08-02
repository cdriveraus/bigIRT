svSocket::startSocketServer(8888);
require(bigIRT);
setwd("/home/driver/bigIRT/testing/")
items <- read.csv(file = 'items.csv')
persons = read.csv(file='persons.csv')
while (!exists("done")) Sys.sleep(1)
