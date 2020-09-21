#Assignment_4
df <-  read.table("../../Data/ITS_mapping.csv", header = TRUE, sep = "\t")
png (filename = "./silly_boxplot.png")
plot (x=(as.factor(df$Ecosystem)), y = df$Lat, xlab = "Ecosystem", ylab = "Latitude", cex.lab=1, cex.axis=0.5, cex.main=1, cex.sub=1) 
dev.off()
?plot()
