svSocket::startSocketServer(8888);
require(bigIRT);
setwd( "C:/Users/Driver/Seafile/mpib/bigIRT/testing")
items <- read.csv(file = 'items.csv')
persons = read.csv(file='persons.csv')
while (!exists("done")) Sys.sleep(1)
