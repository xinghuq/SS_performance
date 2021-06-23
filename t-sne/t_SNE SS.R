install.packages("devtools")
devtools::install_github("jlmelville/smallvis/smallvis")


library(smallvis)
load(".RData")
set.seed(99)
colors = rainbow(length(unique(Model)))
names(colors) = unique(Model)
SSq0_wtsne <- smallvis(SSq0, k=10,scale = TRUE, perplexity = 40,Y_init = "pca", method = "wtsne", ret_extra = c("dx", "dy"),eta=50,exaggeration_factor=10,tol = 1e-30,max_iter = 2000)

png("Ar_wt_SNE.png")
plot(SSq0_wtsne$Y,t="p",xlab="WT-SNE 1",ylab="WT_SNE 2",col = colors[Model])
points(SSq0_wtsne$Y, type="p", pch=19, col=colors[Model], bg=NA, cex=1)
legend("topright", legend=levels(Model),col=colors,pch=19)
dev.off() 
SSq0_wtsne$itercosts

SSq1_wtsne <- smallvis(SSq1,k=10,scale = TRUE,perplexity = 40,method = "wtsne", Y_init = "pca", ret_extra = c("dx", "dy"),eta=50,exaggeration_factor=10,tol = 1e-30,max_iter = 2000)
plot(SSq1_wtsne$Y,t="p",xlab="WT-SNE 1",ylab="WT_SNE 2",col = colors[Model])
points(SSq1_wtsne$Y, type="p", pch=19, col=colors[Model], bg=NA, cex=1)
legend("topright", legend=levels(Model),col=colors,pch=19)
SSq1_wtsne$itercosts

SSq2_wtsne <- smallvis(SSq2, k=10,scale = TRUE, perplexity = 40,Y_init = "pca", method = "wtsne", ret_extra = c("dx", "dy"),eta=50,exaggeration_factor=10,tol = 1e-30,max_iter = 2000)

plot(SSq2_wtsne$Y,t="p",xlab="WT-SNE 1",ylab="WT_SNE 2",col = colors[Model])
points(SSq2_wtsne$Y, type="p", pch=19, col=colors[Model], bg=NA, cex=1)
legend("topright", legend=levels(Model),col=colors,pch=19)
SSq2_wtsne$itercosts

SS_Shannon_wtsne <- smallvis(Shannon_all, k=10,scale = TRUE, perplexity = 40,Y_init = "pca", method = "wtsne", ret_extra = c("dx", "dy"),eta=50,exaggeration_factor=10,tol = 1e-30,max_iter = 2000)
plot(SS_Shannon_wtsne$Y,t="p",xlab="WT-SNE 1",ylab="WT_SNE 2",col = colors[Model])
points(SS_Shannon_wtsne$Y, type="p", pch=19, col=colors[Model], bg=NA, cex=1)
legend("topright", legend=levels(Model),col=colors,pch=19)
SS_Shannon_wtsne$itercosts

SSq0_q2_D_wtsne <- smallvis(SSall, k=10,scale = TRUE, perplexity = 40,Y_init = "pca", method = "wtsne", ret_extra = c("dx", "dy"),eta=50,exaggeration_factor=10,tol = 1e-30,max_iter = 2000)

plot(SSq0_q2_D_wtsne$Y,t="p",xlab="WT-SNE 1",ylab="WT_SNE 2",col = colors[Model])
points(SSq0_q2_D_wtsne$Y, type="p", pch=19, col=colors[Model], bg=NA, cex=1)
legend("topright", legend=levels(Model),col=colors,pch=19)
SSq0_q2_D_wtsne$itercosts

SSSh_D_wtsne <- smallvis(SSq1_Shannnon,k=10, scale = TRUE, perplexity = 40,Y_init = "pca", method = "wtsne", ret_extra = c("dx", "dy"),eta=50,exaggeration_factor=10,tol = 1e-30,max_iter = 2000)

plot(SSSh_D_wtsne$Y,t="p",xlab="WT-SNE 1",ylab="WT_SNE 2",col = colors[Model])
points(SSSh_D_wtsne$Y, type="p", pch=19, col=colors[Model], bg=NA, cex=1)
legend("topright", legend=levels(Model),col=colors,pch=19)
SSSh_D_wtsne$itercosts

SSAr_H_He_wtsne <- smallvis(SSq0_q2_Shannon, k=10,scale = TRUE, perplexity = 40,Y_init = "pca", method = "wtsne", ret_extra = c("dx", "dy"),eta=50,exaggeration_factor=10,tol = 1e-30,max_iter = 2000)

plot(SSAr_H_He_wtsne$Y,t="p",xlab="WT-SNE 1",ylab="WT_SNE 2",col = colors[Model])
points(SSAr_H_He_wtsne$Y, type="p", pch=19, col=colors[Model], bg=NA, cex=1)
legend("topright", legend=levels(Model),col=colors,pch=19)
SSAr_H_He_wtsne$itercosts

SSq0_q2_wtsne <- smallvis(SS_tra, k=10,scale = TRUE, perplexity = 40,Y_init = "pca", method = "wtsne", ret_extra = c("dx", "dy"),eta=50,exaggeration_factor=10,tol = 1e-30,max_iter = 2000)

plot(SSq0_q2_wtsne$Y,t="p",xlab="WT-SNE 1",ylab="WT_SNE 2",col = colors[Model])
points(SSq0_q2_wtsne$Y, type="p", pch=19, col=colors[Model], bg=NA, cex=1)
legend("topright", legend=levels(Model),col=colors,pch=19)
SSq0_q2_wtsne$itercosts

SSAr_H_He_D_wtsne <- smallvis(SSq0_q2_Sh_D, k=10,scale = TRUE, perplexity = 40,Y_init = "pca", method = "wtsne", ret_extra = c("dx", "dy"),eta=50,exaggeration_factor=10,tol = 1e-30,max_iter = 2000)
plot(SSAr_H_He_D_wtsne$Y,t="p",xlab="WT-SNE 1",ylab="WT_SNE 2",col = colors[Model])
points(SSAr_H_He_D_wtsne$Y, type="p", pch=19, col=colors[Model], bg=NA, cex=1)
legend("topright", legend=levels(Model),col=colors,pch=19)
SSAr_H_He_D_wtsne$itercosts


save.image(file = "WT_SNE.RData")


