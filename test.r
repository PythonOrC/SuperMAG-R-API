# httpgd::hgd()
# httpgd::hgd_browse()


# VIC_csvdata_1029 <- read.csv("D:/Personal/Coding/Space Weather/20031029-supermag.csv", header = TRUE, sep = ",", fill = TRUE, comment.char = "")

# # plot original data

# op <- par(
#       mfrow = c(3, 1),
#       oma = c(3, 1, 0, 0) + 0.1,
#       mar = c(0, 3, 1, 1) + 0.1,
#       mgp = c(2, 1, 0),
#       xpd = NA
# )

# plot(VIC_csvdata_1029$dbn_nez, type = "l", , xlab = "", ylab = "nT")
# axis(side = 2)
# title("dBn 20031029")


# plot(VIC_csvdata_1029$dbe_nez, type = "l", , xlab = "", ylab = "nT")
# axis(side = 2)
# title("dBe 20031029")


# plot(VIC_csvdata_1029$dbz_nez, type = "l", , xlab = "", ylab = "nT")
# axis(side = 2)
# title("dBz 20031029")


# print(range(VIC_csvdata_1029$dbe_nez))
# print(range(VIC_csvdata_1029$dbn_nez))
# print(range(VIC_csvdata_1029$dbz_nez))

all_csvdata_1029 <- read.csv("D:\\Personal\\Coding\\Space Weather\\20031029-supermag-6-stations.csv", header = TRUE, sep = ",", fill = TRUE, comment.char = "")
BOU_csvdata_1029 <- seperate(all_csvdata_1029, "BOU")

print(ncol(BOU_csvdata_1029))
print(nrow(BOU_csvdata_1029))