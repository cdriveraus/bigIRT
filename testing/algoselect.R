#
# con <- socketConnection(host = "localhost", port = 8888, #connect to socket server
#   blocking = FALSE, timeout = 30)

# itemcode <-  svSocket::evalServer(con,'
# print(ls())
if(!exists('adat2')) adat2<-list()
itemcode <- bigIRT:::selectItem(items[Scale %in% scale & !Item %in% adat2$item,],
  ability = unlist(persons[person,scale,with=FALSE]),
  targetease = 0.1,samplesize = 1)
  # ')

