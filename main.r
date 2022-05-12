httpgd::hgd()
httpgd::hgd_browse()
library(waveslim)
VIC_csvdata_1029 <- read.csv("D:/Personal/Coding/Space Weather/20031029-supermag.csv", header = TRUE, sep = ",", fill = TRUE, comment.char = "")
# wavelet analysis
# wavelet parameters
wf <- "la8"
boundary <- "reflection"
N <- 1439
levels <- floor(log(N, 2))
# Wavelet MRA

op <- par(
    mfrow = c(12, 1),
    oma = c(3, 1, 0, 0) + 0.1,
    mar = c(0, 3, 1, 1) + 0.1,
    mgp = c(2, 1, 0),
    xpd = NA
)


VIC_wavelet_tmp <- VIC_csvdata_1029$dbn_nez
VIC_wavelet.wt <- modwt(VIC_wavelet_tmp, wf = wf, n.levels = levels, boundary = boundary)
VIC_wavelet_mra_1029dbn <- mra.wt(VIC_wavelet.wt)
for (j in 1:levels) {
    plot(VIC_wavelet_mra_1029dbn[[j]], type = "l", , xlab = "", ylab = "nT")
    axis(side = 2)
    title(paste("dBN 20031029, level ", j, sep = ""))
    print(diff(range(VIC_wavelet_mra_1029dbn[[j]])))
}
plot(VIC_csvdata_1029$dbn_nez, type = "l", , xlab = "", ylab = "nT")
title("dBN 20031029 (raw data)")
# BOU, NEW, BSL, FRD, FRN, TUC