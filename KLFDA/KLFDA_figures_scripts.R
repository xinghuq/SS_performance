#### now we can do test and plot the subspace into 3D plot
###  now we found klfda is better than lfda, thus we used klfda
library(kernlab)
library(lfda)
library(MASS)
load("~/KLFDA_updated_KernelMatrix.RData")

klfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=20,knn=50,kernel=rbfdot(sigma = 0.1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SSq1=klfda_1(as.matrix(SSq1_scaled),as.factor(Model),r=20,knn=50,kernel=rbfdot(sigma = 0.1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SSq2=klfda_1(as.matrix(SSq2_scaled),as.factor(Model),r=20,knn=68,kernel=rbfdot(sigma = 0.1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SS_sh=klfda_1(as.matrix(Shannon_all_scaled),as.factor(Model),r=20,knn=50,kernel=rbfdot(sigma = 0.05),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SS_trad=klfda_1(as.matrix(SS_trad_scaled),as.factor(Model),r=20,knn=50,kernel=rbfdot(sigma = 0.05),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
#### not ssall is SSq0+SSq1+SSq2, not include SSshannon
klfda_SS_q0_1D_q2=klfda_1(as.matrix(SSall_scaled),as.factor(Model),r=20,knn=50,kernel=rbfdot(sigma = 0.05),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")

klfda_Sh_1D_scaled=klfda_1(as.matrix(Sh_1D_scaled),as.factor(Model),r=20,knn=50,kernel=rbfdot(sigma = 0.05),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SS_q0_Sh_q2=klfda_1(as.matrix(SSq0_q2_Shannon_scaled),as.factor(Model),r=20,knn=50,kernel=rbfdot(sigma = 0.01),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SS_q0_q2_Sh_1D=klfda_1(as.matrix(SSq0_q2_Sh_D_scaled),as.factor(Model),r=20,knn=55,kernel=rbfdot(sigma = 0.01),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")

library(caret)
confusionMatrix(klfda_SSq1$bayes_assigment$class,Model)


Z=klfda_SSq0$Z
Z=klfda_SSq1$Z
Z=klfda_SSq2$Z
Z=klfda_SS_sh$Z
Z=klfda_SS_trad$Z
Z=klfda_SS_q0_1D_q2$Z
Z=klfda_Sh_1D_scaled$Z
Z=klfda_SS_q0_Sh_q2$Z
Z=klfda_SS_q0_q2_Sh_1D$Z




library(plotly)
cols2=rainbow(length(unique(Model)))
p_SSq0 <- plot_ly(as.data.frame(Z), x =Z[,1], y =Z[,2],z=Z[,3],  color = Model,colors=cols2) %>% 
  layout(autosize = TRUE)%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LDA1'),
                      yaxis = list(title = 'LDA2'),
                      zaxis = list(title = 'LDA3')))

p_SSq0

p_SSq1 <- plot_ly(as.data.frame(Z), x =Z[,1], y =Z[,2],z=Z[,3],  color = Model,colors=cols2) %>% 
  layout(autosize = TRUE)%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LDA1'),
                      yaxis = list(title = 'LDA2'),
                      zaxis = list(title = 'LDA3')))

p_SSq1

p_SSq0 <- plot_ly(as.data.frame(Z), x =Z[,1], y =Z[,2],z=Z[,3],  color = Model,colors=cols2) %>% 
  layout(autosize = TRUE)%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LDA1'),
                      yaxis = list(title = 'LDA2'),
                      zaxis = list(title = 'LDA3')))

p_SSq2

p_SSq2 <- plot_ly(as.data.frame(Z), x =Z[,1], y =Z[,2],z=Z[,3],  color = Model,colors=cols2) %>% 
  layout(autosize = TRUE)%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LDA1'),
                      yaxis = list(title = 'LDA2'),
                      zaxis = list(title = 'LDA3')))

p_SSq

p_SSq0 <- plot_ly(as.data.frame(Z), x =Z[,1], y =Z[,2],z=Z[,3],  color = Model,colors=cols2) %>% 
  layout(autosize = TRUE)%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LDA1'),
                      yaxis = list(title = 'LDA2'),
                      zaxis = list(title = 'LDA3')))

p_SSq0

p_SSq0 <- plot_ly(as.data.frame(Z), x =Z[,1], y =Z[,2],z=Z[,3],  color = Model,colors=cols2) %>% 
  layout(autosize = TRUE)%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LDA1'),
                      yaxis = list(title = 'LDA2'),
                      zaxis = list(title = 'LDA3')))

p_SSq0

p_SSq0 <- plot_ly(as.data.frame(Z), x =Z[,1], y =Z[,2],z=Z[,3],  color = Model,colors=cols2) %>% 
  layout(autosize = TRUE)%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LDA1'),
                      yaxis = list(title = 'LDA2'),
                      zaxis = list(title = 'LDA3')))


save(klfda_SSq0,
     klfda_SSq1,
     klfda_SSq2,
     klfda_SS_sh,
     klfda_SS_trad,
     klfda_SS_q0_1D_q2,
     klfda_Sh_1D_scaled,
     klfda_SS_q0_Sh_q2,
     klfda_SS_q0_q2_Sh_1D,file = "KLFDA_figure_mutiple_kernel_gaussian.RData")
