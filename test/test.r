library(jsonlite)

content <- fromJSON(txt = "{\"tval\":1067385600.000000, \"ext\": 60.000000, \"iaga\": \"VIC\", \"N\": {\"nez\": -32.032387, \"geo\": -31.587579}, \"E\": {\"nez\": 3.914707, \"geo\": -6.604813}, \"Z\": {\"nez\": 30.236118, \"geo\": 30.236118}}")
content2 <- fromJSON(txt = "{\"tval\":1067385660.000000, \"ext\": 60.000000, \"iaga\": \"VIC\", \"N\": {\"nez\": -29.978230, \"geo\": -29.734411}, \"E\": {\"nez\": 4.199440, \"geo\": -5.674006}, \"Z\": {\"nez\": 31.153442, \"geo\": 31.153442}}")
# content3 <- fromJSON(txt = '{"tval":1067385720.000000, "ext": 60.000000, "iaga": "VIC", "N": {"nez": -26.973866, "geo": -26.878542}, "E": {"nez": 4.163980, "geo": -4.740480}, "Z": {"nez": 32.070774, "geo": 32.070774}}')
contents <- list(content, content2)
print(contents)
a <- Map(c,unlist(content),unlist(content2))
print(a)

# a <- data.frame("A"="A1","B"="B1","C"="C1")
# b <- data.frame("A"="A2","B"="B2","C"="C2")
# c <- data.frame("A"="A3","B"="B3","C"="C3")
# # c <- as.data.frame(c)
# all <- list(a,b,c)
# print(all)
# result = Reduce(merge, all)
# print(result)

# df1 <- data.frame("names" = "John", "age" = 21)
# df2 <- data.frame("names" = "H", "score" = 22)
# df3 <- data.frame("names" = "John", "country" = "US")
# print(list(df1,df2,df3))
# print(Reduce(merge, list(df1,df2,df3)))