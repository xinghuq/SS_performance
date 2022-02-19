library(lfda)
library(kernlab)
library(MASS)
library(caret)

#### scripts to plot the projection data to subspace
SSq0_scaled=scale(SSq0[,-1])
SSq1_scaled=scale(SSq1[,-1])

#### calculate the variance of the components

allklfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=44, knn=10,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")

allklfda_SSq1=klfda_1(as.matrix(SSq1_scaled),as.factor(Model),r=48, knn=7,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
allklfda_SSq2=klfda_1(as.matrix(SSq2_scaled),as.factor(Model),r=44,knn=39,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
allklfda_SS_sh=klfda_1(as.matrix(Shannon_all_scaled),as.factor(Model),r=46,knn=76,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
allklfda_SS_trad=klfda_1(as.matrix(SS_trad_scaled),as.factor(Model),knn=7,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
#### not ssall is SSq0+SSq1+SSq2, not include SSshannon
allklfda_SS_q0_1D_q2=klfda_1(as.matrix(SSall_scaled),as.factor(Model),knn=8,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")

allklfda_Sh_1D_scaled=klfda_1(as.matrix(Sh_1D_scaled),as.factor(Model),knn=4,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
allklfda_SS_q0_Sh_q2=klfda_1(as.matrix(SSq0_q2_Shannon_scaled),as.factor(Model),knn=15,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
allklfda_SS_q0_q2_Sh_1D=klfda_1(as.matrix(SSq0_q2_Sh_D_scaled),as.factor(Model),knn=9,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")

Z_allklfda_SSq1=allklfda_SSq1$T
vars <- apply(Z_allklfda_SSq1, 2, sd)  
props <- vars^2 / sum(vars^2)
cumsum(props)

confusionMatrix(allklfda_SSq1$posteriors.classZ,Model)
#### The first step is tuning knn, to find a best knn for the model, number of k-nearest neighbours
knn=1:99L
find_knn_SSq0=list()
T_kfda_SSq0=list()
for (i in 1:length(knn)){
  find_knn_SSq0[[i]]=list()
  T_kfda_SSq0[[i]]=list()
T_kfda_SSq0[[i]]=klfda_1(as.matrix(SSq0[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = 0.5),tol=1e-900,knn=knn[[i]],prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")

#kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
find_knn_SSq0[[i]]=confusionMatrix(T_kfda_SSq0[[i]]$bayes_assigment$class,Model)$overall[[1]]
}
find_klfda_SSq0_value=do.call(rbind,lapply(find_knn_SSq0, data.frame))

knn_klfda_SSq0=cbind(knn,find_klfda_SSq0_value)
write.csv(knn_klfda_SSq0,file = "find_knn_SSq0_not_scaled_accuracy,r_15_gamma_0.5.csv")
#### find the best r SSq0
r=2:44L
find_r_SSq0=list()
T_kfda_r_SSq0=list()
for (i in 1:length(r)){
  find_r_SSq0[[i]]=list()
  T_kfda_r_SSq0[[i]]=list()
  T_kfda_r_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=r[[i]],kernel=rbfdot(sigma = 1),tol=1e-9000,knn=10,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_r_SSq0[[i]]=confusionMatrix(T_kfda_r_SSq0$bayes_assigment$class,Model)$overall[[1]]
}

find_r_klfda_SSq0_value=do.call(rbind,lapply(find_r_SSq0, data.frame))

knn_r_klfda_SSq0=cbind(r,find_r_klfda_SSq0_value)

write.csv(knn_r_klfda_SSq0,file = "r_accuracy_klfda_SSq0_scaled_scaled_knn=10_update_kernelmatrix.csv")


knn=1:99L
find_knn_SSq0=list()
T_kfda_SSq0=list()
for (i in 1:length(knn)){
  find_knn_SSq0[[i]]=list()
  T_kfda_SSq0[[i]]=list()
  T_kfda_SSq0[[i]]=klfda_1(as.matrix(SSq0[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = 0.5),tol=1e-900,knn=knn[[i]],prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_knn_SSq0[[i]]=confusionMatrix(T_kfda_SSq0[[i]]$bayes_assigment$class,Model)$overall[[1]]
}
find_klfda_SSq0_value=do.call(rbind,lapply(find_knn_SSq0, data.frame))

knn_klfda_SSq0=cbind(knn,find_klfda_SSq0_value)
write.csv(knn_klfda_SSq0,file = "find_knn_SSq0_not_scaled_accuracy,r_15_gamma_0.5.csv")
gamma=c(0.001,0.005,0.01,0.05,0.1,0.5,1)

SSq1_tune=tune(klfda_1, train.x=as.matrix(SSq1[,-1]), train.y = as.factor(Model), validation.x= NULL, validation.y = NULL, kernel=rbfdot(sigma = 0.001:1),r=20,knn=7, tol=1e-90,prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain",predict.func = predict.kfda)


sigma=c(0.001,0.005,0.01,0.05,0.1,0.5,1,5,10)
knn=1:99L
find_sigma_knn_SSq0=list()
#sigma_knn_kfda_SSq0=list()
sigma_knn_SSq0=matrix(data=NA,ncol = 2,nrow =900)
for (i in 1:length(sigma)){
  find_sigma_knn_SSq0[[i]]=list()
  #sigma_knn_kfda_SSq0[[i]]=list()
  # sigma_knn_kfda_SSq0[[i]]=list()
  for(j in 1:length(knn)) {
    find_sigma_knn_SSq0[[i]][[j]]=list()
    #sigma_knn_kfda_SSq0[[i]][[j]]=list()
    sigma_knn_kfda_SSq0=klfda_1(as.matrix(SSq0[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = sigma[i]),tol=1e-30,knn=knn[j],prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    
    #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    find_sigma_knn_SSq0[[i]][[j]]=confusionMatrix(sigma_knn_kfda_SSq0$bayes_assigment$class,Model)$overall[[1]]
    sigma_knn_SSq0[,1]=sigma[i]
    sigma_knn_SSq0[,2]=knn[j]
  }
}
find_sigma_knn_SSq0_value=do.call(rbind,lapply(find_sigma_knn_SSq0, data.frame))

sigma_knn_klfda_SSq0_results=cbind(sigma_knn_SSq0,find_sigma_knn_SSq0_value)

write.csv(sigma_knn_klfda_SSq0_results,file = "Train_accuracy_sigma_knn_klfda_SSq0_results_not_scaled_r_20.csv")



sigma=c(0.001,0.005,0.01,0.05,0.1,0.5,1,5,10)
knn=1:99L
find_sigma_knn_SSq1=list()
sigma_knn_kfda_SSq1=list()
sigma_knn_SSq1=list()
  matrix(data=NA,ncol = 2,nrow =891)
for (i in 1:length(sigma)){
  find_sigma_knn_SSq1[[i]]=list()
  sigma_knn_kfda_SSq1[[i]]=list()
 # sigma_knn_kfda_SSq1[[i]]=list()
  for(j in 1:length(knn)) {
  find_sigma_knn_SSq1[[i]][[j]]=list()
  sigma_knn_kfda_SSq1[[i]][[j]]=list()
  sigma_knn_kfda_SSq1[[i]][[j]]=klfda_1(as.matrix(SSq1[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = sigma[i]),tol=1e-30,knn=knn[j],prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_sigma_knn_SSq1[[i]][[j]]=confusionMatrix(sigma_knn_kfda_SSq1[[i]][[j]]$bayes_assigment$class,Model)$overall[[1]]
  sigma_knn_SSq1[[i]][[j]]=matrix(data=NA,ncol = 2,nrow = 99)
  sigma_knn_SSq1[[i]][[j]]=sigma_knn_SSq1[[i]][[j]][i,j]
  }
}
  sigma=c(0.001,0.005,0.01,0.05,0.1,0.5,1,5,10)
  knn=1:99L

  sigma_knn_SSq1=list()
  
  for (i in 1:length(sigma)){
  sigma_knn_SSq1[[i]]=matrix(data=NA,ncol = 2,nrow = 99)
  for(j in 1:length(knn)) {
    sigma_knn_SSq1[[i]][j,1]=sigma[i]
  sigma_knn_SSq1[[i]][j,2]=knn[j]
  }
  }
  
  sigma_knn_SSq1=do.call(rbind, lapply(sigma_knn_SSq1,as.data.frame))
find_sigma_knn_SSq1_value=as.data.frame(unlist(find_sigma_knn_SSq1))
# convert to matrix 
#find_sigma_knn_SSq1_value=as.data.frame(matrix(unlist(find_sigma_knn_SSq1), nrow=length(unlist(find_sigma_knn_SSq1[1]))))

sigma_knn_klfda_SSq1_results=cbind(sigma_knn_SSq1[-(892:900),],find_sigma_knn_SSq1_value)

write.csv(sigma_knn_klfda_SSq1_results,file = "Train_accuracy_sigma_knn_klfda_SSq1_results_not_scaled_r_20.csv")
save.image()

sigma=c(0.001,0.005,0.01,0.05,0.1,0.5,1,5,10)
knn=1:99L
find_sigma_knn_SSq2=list()
#sigma_knn_kfda_SSq2=list()
sigma_knn_SSq2=matrix(data=NA,ncol = 2,nrow =900)
for (i in 1:length(sigma)){
  find_sigma_knn_SSq2[[i]]=list()
  #sigma_knn_kfda_SSq2[[i]]=list()
  # sigma_knn_kfda_SSq2[[i]]=list()
  for(j in 1:length(knn)) {
    find_sigma_knn_SSq2[[i]][[j]]=list()
    #sigma_knn_kfda_SSq2[[i]][[j]]=list()
    sigma_knn_kfda_SSq2=klfda_1(as.matrix(SSq2[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = sigma[i]),tol=1e-30,knn=knn[j],prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    
    #kfda_SSq2=klfda_1(as.matrix(SSq2_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    find_sigma_knn_SSq2[[i]][[j]]=confusionMatrix(sigma_knn_kfda_SSq2$bayes_assigment$class,Model)$overall[[1]]
    sigma_knn_SSq2[,1]=sigma[i]
    sigma_knn_SSq2[,2]=knn[j]
  }
}
find_sigma_knn_SSq2_value=do.call(rbind,lapply(find_sigma_knn_SSq2, data.frame))

sigma_knn_klfda_SSq2_results=cbind(sigma_knn_SSq2,find_sigma_knn_SSq2_value)

write.csv(sigma_knn_klfda_SSq2_results,file = "Train_accuracy_sigma_knn_klfda_SSq2_results_not_scaled_r_20.csv")

sigma=c(0.001,0.005,0.01,0.05,0.1,0.5,1,5,10)
knn=1:99L
find_sigma_knn_SS_trad=list()
#sigma_knn_kfda_SS_trad=list()
sigma_knn_SS_trad=matrix(data=NA,ncol = 2,nrow =900)
for (i in 1:length(sigma)){
  find_sigma_knn_SS_trad[[i]]=list()
  #sigma_knn_kfda_SS_trad[[i]]=list()
  # sigma_knn_kfda_SS_trad[[i]]=list()
  for(j in 1:length(knn)) {
    find_sigma_knn_SS_trad[[i]][[j]]=list()
    #sigma_knn_kfda_SS_trad[[i]][[j]]=list()
    sigma_knn_kfda_SS_trad=klfda_1(as.matrix(SS_tra[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = sigma[i]),tol=1e-30,knn=knn[j],prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    
    #kfda_SS_trad=klfda_1(as.matrix(SS_trad_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    find_sigma_knn_SS_trad[[i]][[j]]=confusionMatrix(sigma_knn_kfda_SS_trad$bayes_assigment$class,Model)$overall[[1]]
    sigma_knn_SS_trad[,1]=sigma[i]
    sigma_knn_SS_trad[,2]=knn[j]
  }
}
find_sigma_knn_SS_trad_value=do.call(rbind,lapply(find_sigma_knn_SS_trad, data.frame))

sigma_knn_klfda_SS_trad_results=cbind(sigma_knn_SS_trad,find_sigma_knn_SS_trad_value)

write.csv(sigma_knn_klfda_SS_trad_results,file = "Train_accuracy_sigma_knn_klfda_SS_trad_results_not_scaled_r_20.csv")

sigma=c(0.001,0.005,0.01,0.05,0.1,0.5,1,5,10)
knn=1:99L
find_sigma_knn_SS_q0_q2_D1=list()
#sigma_knn_kfda_SS_q0_q2_D1=list()
sigma_knn_SS_q0_q2_D1=matrix(data=NA,ncol = 2,nrow =900)
for (i in 1:length(sigma)){
  find_sigma_knn_SS_q0_q2_D1[[i]]=list()
  #sigma_knn_kfda_SS_q0_q2_D1[[i]]=list()
  # sigma_knn_kfda_SS_q0_q2_D1[[i]]=list()
  for(j in 1:length(knn)) {
    find_sigma_knn_SS_q0_q2_D1[[i]][[j]]=list()
    #sigma_knn_kfda_SS_q0_q2_D1[[i]][[j]]=list()
    sigma_knn_kfda_SS_q0_q2_D1=klfda_1(as.matrix(SSall[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = sigma[i]),tol=1e-30,knn=knn[j],prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    
    #kfda_SS_q0_q2_D1=klfda_1(as.matrix(SS_q0_q2_D1_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    find_sigma_knn_SS_q0_q2_D1[[i]][[j]]=confusionMatrix(sigma_knn_kfda_SS_q0_q2_D1$bayes_assigment$class,Model)$overall[[1]]
    sigma_knn_SS_q0_q2_D1[,1]=sigma[i]
    sigma_knn_SS_q0_q2_D1[,2]=knn[j]
  }
}
find_sigma_knn_SS_q0_q2_D1_value=do.call(rbind,lapply(find_sigma_knn_SS_q0_q2_D1, data.frame))

sigma_knn_klfda_SS_q0_q2_D1_results=cbind(sigma_knn_SS_q0_q2_D1,find_sigma_knn_SS_q0_q2_D1_value)

write.csv(sigma_knn_klfda_SS_q0_q2_D1_results,file = "Train_accuracy_sigma_knn_klfda_SS_q0_q2_D1_results_not_scaled_r_20.csv")

sigma=c(0.001,0.005,0.01,0.05,0.1,0.5,1,5,10)
knn=1:99L
find_sigma_knn_SS_Shannon=list()
#sigma_knn_kfda_SS_Shannon=list()
sigma_knn_SS_Shannon=matrix(data=NA,ncol = 2,nrow =900)
for (i in 1:length(sigma)){
  find_sigma_knn_SS_Shannon[[i]]=list()
  #sigma_knn_kfda_SS_Shannon[[i]]=list()
  # sigma_knn_kfda_SS_Shannon[[i]]=list()
  for(j in 1:length(knn)) {
    find_sigma_knn_SS_Shannon[[i]][[j]]=list()
    #sigma_knn_kfda_SS_Shannon[[i]][[j]]=list()
    sigma_knn_kfda_SS_Shannon=klfda_1(as.matrix(Shannon_all[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = sigma[i]),tol=1e-30,knn=knn[j],prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    
    #kfda_SS_Shannon=klfda_1(as.matrix(SS_Shannon_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    find_sigma_knn_SS_Shannon[[i]][[j]]=confusionMatrix(sigma_knn_kfda_SS_Shannon$bayes_assigment$class,Model)$overall[[1]]
    sigma_knn_SS_Shannon[,1]=sigma[i]
    sigma_knn_SS_Shannon[,2]=knn[j]
  }
}
find_sigma_knn_SS_Shannon_value=do.call(rbind,lapply(find_sigma_knn_SS_Shannon, data.frame))

sigma_knn_klfda_SS_Shannon_results=cbind(sigma_knn_SS_Shannon,find_sigma_knn_SS_Shannon_value)

write.csv(sigma_knn_klfda_SS_Shannon_results,file = "Train_accuracy_sigma_knn_klfda_SS_Shannon_results_not_scaled_r_20.csv")


sigma=c(0.001,0.005,0.01,0.05,0.1,0.5,1,5,10)
knn=1:99L
find_sigma_knn_SS_Sh_D=list()
#sigma_knn_kfda_SS_Sh_D=list()
sigma_knn_SS_Sh_D=matrix(data=NA,ncol = 2,nrow =900)
for (i in 1:length(sigma)){
  find_sigma_knn_SS_Sh_D[[i]]=list()
  #sigma_knn_kfda_SS_Sh_D[[i]]=list()
  # sigma_knn_kfda_SS_Sh_D[[i]]=list()
  for(j in 1:length(knn)) {
    find_sigma_knn_SS_Sh_D[[i]][[j]]=list()
    #sigma_knn_kfda_SS_Sh_D[[i]][[j]]=list()
    sigma_knn_kfda_SS_Sh_D=klfda_1(as.matrix(SSq1_Shannnon[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = sigma[i]),tol=1e-30,knn=knn[j],prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    
    #kfda_SS_Sh_D=klfda_1(as.matrix(SS_Sh_D_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    find_sigma_knn_SS_Sh_D[[i]][[j]]=confusionMatrix(sigma_knn_kfda_SS_Sh_D$bayes_assigment$class,Model)$overall[[1]]
    sigma_knn_SS_Sh_D[,1]=sigma[i]
    sigma_knn_SS_Sh_D[,2]=knn[j]
  }
}
find_sigma_knn_SS_Sh_D_value=do.call(rbind,lapply(find_sigma_knn_SS_Sh_D, data.frame))

sigma_knn_klfda_SS_Sh_D_results=cbind(sigma_knn_SS_Sh_D,find_sigma_knn_SS_Sh_D_value)

write.csv(sigma_knn_klfda_SS_Sh_D_results,file = "Train_accuracy_sigma_knn_klfda_SS_Sh_D_results_not_scaled_r_20.csv")

sigma=c(0.001,0.005,0.01,0.05,0.1,0.5,1,5,10)
knn=1:99L
find_sigma_knn_SS_q0_q2_Shannon=list()
#sigma_knn_kfda_SS_q0_q2_Shannon=list()
sigma_knn_SS_q0_q2_Shannon=matrix(data=NA,ncol = 2,nrow =900)
for (i in 1:length(sigma)){
  find_sigma_knn_SS_q0_q2_Shannon[[i]]=list()
  #sigma_knn_kfda_SS_q0_q2_Shannon[[i]]=list()
  # sigma_knn_kfda_SS_q0_q2_Shannon[[i]]=list()
  for(j in 1:length(knn)) {
    find_sigma_knn_SS_q0_q2_Shannon[[i]][[j]]=list()
    #sigma_knn_kfda_SS_q0_q2_Shannon[[i]][[j]]=list()
    sigma_knn_kfda_SS_q0_q2_Shannon=klfda_1(as.matrix(SSq0_q2_Shannon[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = sigma[i]),tol=1e-30,knn=knn[j],prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    
    #kfda_SS_q0_q2_Shannon=klfda_1(as.matrix(SS_q0_q2_Shannon_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    find_sigma_knn_SS_q0_q2_Shannon[[i]][[j]]=confusionMatrix(sigma_knn_kfda_SS_q0_q2_Shannon$bayes_assigment$class,Model)$overall[[1]]
    sigma_knn_SS_q0_q2_Shannon[,1]=sigma[i]
    sigma_knn_SS_q0_q2_Shannon[,2]=knn[j]
  }
}
find_sigma_knn_SS_q0_q2_Shannon_value=do.call(rbind,lapply(find_sigma_knn_SS_q0_q2_Shannon, data.frame))

sigma_knn_klfda_SS_q0_q2_Shannon_results=cbind(sigma_knn_SS_q0_q2_Shannon,find_sigma_knn_SS_q0_q2_Shannon_value)

write.csv(sigma_knn_klfda_SS_q0_q2_Shannon_results,file = "Train_accuracy_sigma_knn_klfda_SS_q0_q2_Shannon_results_not_scaled_r_20.csv")

sigma=c(0.001,0.005,0.01,0.05,0.1,0.5,1,5,10)
knn=1:99L
find_sigma_knn_SS_q0_q2_Sh_D=list()
#sigma_knn_kfda_SS_q0_q2_Sh_D=list()
sigma_knn_SS_q0_q2_Sh_D=matrix(data=NA,ncol = 2,nrow =900)
for (i in 1:length(sigma)){
  find_sigma_knn_SS_q0_q2_Sh_D[[i]]=list()
  #sigma_knn_kfda_SS_q0_q2_Sh_D[[i]]=list()
  # sigma_knn_kfda_SS_q0_q2_Sh_D[[i]]=list()
  for(j in 1:length(knn)) {
    find_sigma_knn_SS_q0_q2_Sh_D[[i]][[j]]=list()
    #sigma_knn_kfda_SS_q0_q2_Sh_D[[i]][[j]]=list()
    sigma_knn_kfda_SS_q0_q2_Sh_D=klfda_1(as.matrix(SSq0_q2_Sh_D[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = sigma[i]),tol=1e-30,knn=knn[j],prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    
    #kfda_SS_q0_q2_Sh_D=klfda_1(as.matrix(SS_q0_q2_Sh_D_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
    find_sigma_knn_SS_q0_q2_Sh_D[[i]][[j]]=confusionMatrix(sigma_knn_kfda_SS_q0_q2_Sh_D$bayes_assigment$class,Model)$overall[[1]]
    sigma_knn_SS_q0_q2_Sh_D[,1]=sigma[i]
    sigma_knn_SS_q0_q2_Sh_D[,2]=knn[j]
  }
}
find_sigma_knn_SS_q0_q2_Sh_D_value=do.call(rbind,lapply(find_sigma_knn_SS_q0_q2_Sh_D, data.frame))

sigma_knn_klfda_SS_q0_q2_Sh_D_results=cbind(sigma_knn_SS_q0_q2_Sh_D,find_sigma_knn_SS_q0_q2_Sh_D_value)

write.csv(sigma_knn_klfda_SS_q0_q2_Sh_D_results,file = "Train_accuracy_sigma_knn_klfda_SS_q0_q2_Sh_D_results_not_scaled_r_20.csv")

### the best r is 20, and knn=16 for non-scaled data
klfda_SSq0=klfda_1(as.matrix(SSq0[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = 1),tol=1e-90,knn=16,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
confusionMatrix(klfda_SSq0$bayes_judgement$post_class,Model)
Z=klfda_SSq0$Z
### the best r=15-20, knn=12 for scaled data, ac=1

klfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=20,kernel=rbfdot(sigma = 1),tol=1e-90,knn=12,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
confusionMatrix(klfda_SSq0$bayes_assigment$class,Model)
Z=klfda_SSq0$Z

#### test for lfda

knn=1:99L
find_knn_SSq0=list()
T_kfda_SSq0=list()
for (i in 1:length(knn)){
  find_knn_SSq0[[i]]=list()
  T_kfda_SSq0[[i]]=list()
  T_kfda_SSq0[[i]]=lfda_1(as.matrix(SSq0[,-1]),Model,r=20,knn=knn[[i]],CV=FALSE,usekernel = FALSE,tol=1e-30, fL = 0.5,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_knn_SSq0[[i]]=confusionMatrix(T_kfda_SSq0[[i]]$bayes_assigment$class,Model)$overall[[1]]
}
find_klfda_SSq0_value=do.call(rbind,lapply(find_knn_SSq0, data.frame))

knn_klfda_SSq0=cbind(knn,find_klfda_SSq0_value)

write.csv(knn_klfda_SSq0,file = "lfda_SSq0_not_scaled_r_20.csv")

lfda_SSq0=lfda_1(as.matrix(SSq0_scaled),Model,r=20,tol=1e-30,CV=FALSE,usekernel = FALSE, fL = 0.5,knn = 34)

Z=lfda_SSq0$Z
confusionMatrix(lfda_SSq0$bayes_assigment$class,Model)


#### now test for SSq1
### 
library(e1071)
gamma=c(0.001,0.005,0.01,0.05,0.1,0.5,1)

SSq1_tune=tune(klfda_1, train.x=as.matrix(SSq1[,-1]), train.y = as.factor(Model), validation.x= NULL, validation.y = NULL, kernel=rbfdot(sigma = 0.001:1),r=20,knn=7, tol=1e-90,prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain",predict.func = predict.kfda)

find_gamma_SSq1=list()
gamma_kfda_SSq1=list()
for (i in 1:length(gamma)){
  find_gamma_SSq1[[i]]=list()
  gamma_kfda_SSq1[[i]]=list()
  gamma_kfda_SSq1[[i]]=klfda_1(as.matrix(SSq1_scaled),as.factor(Model),r=20,kernel=rbfdot(sigma = gamma[i]),tol=1e-30,knn=56,prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_gamma_SSq1[[i]]=confusionMatrix(gamma_kfda_SSq1[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_gamma_klfda_SSq1_value=do.call(rbind,lapply(find_gamma_SSq1, data.frame))

gamma_klfda_SSq1=cbind(gamma,find_gamma_klfda_SSq1_value)
write.csv(gamma_klfda_SSq1,file = "klfda_SSq1_gamma_accuracy_r_20_update_final.csv")




knn=1:99L
find_knn_SSq1=list()
T_kfda_SSq1=list()
for (i in 1:length(knn)){
  find_knn_SSq1[[i]]=list()
  T_kfda_SSq1[[i]]=list()
  T_kfda_SSq1[[i]]=klfda_1(as.matrix(SSq1[,-1]),as.factor(Model),r=20,kernel=rbfdot(sigma = 0.5),tol=1e-30,knn=knn[[i]],prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_knn_SSq1[[i]]=confusionMatrix(T_kfda_SSq1[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_klfda_SSq1_value=do.call(rbind,lapply(find_knn_SSq1, data.frame))

knn_klfda_SSq1=cbind(knn,find_klfda_SSq1_value)
write.csv(knn_klfda_SSq1,file = "klfda_SSq1_not_scaled_Knn_accuracy_r_20_update_final_sigma=0.5.csv")
### klfda find the best r SSq1

r=2:48L
find_r_SSq1=list()
T_kfda_r_SSq1=list()
for (i in 1:length(r)){
  find_r_SSq1[[i]]=list()
  T_kfda_r_SSq1[[i]]=list()
  T_kfda_r_SSq1=klfda_1(as.matrix(SSq1_scaled),as.factor(Model),r=r[[i]],kernel=rbfdot(sigma = 1),tol=1e-9000,knn=10,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq1=klfda_1(as.matrix(SSq1_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_r_SSq1[[i]]=confusionMatrix(T_kfda_r_SSq1$bayes_assigment$class,Model)$overall[[1]]
}

find_r_klfda_SSq1_value=do.call(rbind,lapply(find_r_SSq1, data.frame))

knn_r_klfda_SSq1=cbind(r,find_r_klfda_SSq1_value)

write.csv(knn_r_klfda_SSq1,file = "r_accuracy_klfda_SSq1_scaled_scaled_knn=10_update_kernelmatrix.csv")


knn=1:99L
lfda_find_knn_SSq1=list()
T_lfda_SSq1=list()
for (i in 1:length(knn)){
  lfda_find_knn_SSq1[[i]]=list()
  T_lfda_SSq1[[i]]=list()
  T_lfda_SSq1[[i]]=lfda_1(as.matrix(SSq1_scaled),as.factor(Model),r=20,knn=knn[[i]],prior=c(1,1,1,1,1)/5,tol=1e-30,CV=FALSE,usekernel = FALSE, fL = 0.5)
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  lfda_find_knn_SSq1[[i]]=confusionMatrix(T_lfda_SSq1[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_lfda_SSq1_value=do.call(rbind,lapply(lfda_find_knn_SSq1, data.frame))

knn_lfda_SSq1=cbind(knn,find_lfda_SSq1_value)
write.csv(knn_lfda_SSq1,file = "lfda_SSq1_Knn_accuracy_r_20.csv")


### Now test for scaled SSq2
SSq2_scaled=scale(SSq2[,-1])

knn=1:99L
find_knn_SSq2=list()
T_kfda_SSq2=list()
for (i in 1:length(knn)){
  find_knn_SSq2[[i]]=list()
  T_kfda_SSq2[[i]]=list()
  T_kfda_SSq2[[i]]=klfda_1(as.matrix(SSq2_scaled),as.factor(Model),r=20,kernel=rbfdot(sigma = 1),tol=1e-90,knn=knn[[i]],prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_knn_SSq2[[i]]=confusionMatrix(T_kfda_SSq2[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_klfda_SSq2_value=do.call(rbind,lapply(find_knn_SSq2, data.frame))

knn_klfda_SSq2=cbind(knn,find_klfda_SSq2_value)
write.csv(knn_klfda_SSq2,file = "klfda_SSq2_scaled_Knn_accuracy_r_20_update.csv")
### klfda find r

r=2:44L
find_r_SSq2=list()
T_kfda_r_SSq2=list()
for (i in 1:length(r)){
  find_r_SSq2[[i]]=list()
  T_kfda_r_SSq2[[i]]=list()
  T_kfda_r_SSq2=klfda_1(as.matrix(SSq2_scaled),as.factor(Model),r=r[[i]],kernel=rbfdot(sigma = 1),tol=1e-9000,knn=39,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq2=klfda_1(as.matrix(SSq2_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_r_SSq2[[i]]=confusionMatrix(T_kfda_r_SSq2$bayes_assigment$class,Model)$overall[[1]]
}

find_r_klfda_SSq2_value=do.call(rbind,lapply(find_r_SSq2, data.frame))

knn_r_klfda_SSq2=cbind(r,find_r_klfda_SSq2_value)

write.csv(knn_r_klfda_SSq2,file = "r_accuracy_klfda_SSq2_scaled_scaled_knn=39_update_kernelmatrix.csv")



###lfda find knn
knn=1:99L
lfda_find_knn_SSq2=list()
T_lfda_SSq2=list()
for (i in 1:length(knn)){
  lfda_find_knn_SSq2[[i]]=list()
  T_lfda_SSq2[[i]]=list()
  T_lfda_SSq2[[i]]=lfda_1(as.matrix(SSq2_scaled),as.factor(Model),r=20,knn=knn[[i]],prior=c(1,1,1,1,1)/5,tol=1e-30,CV=FALSE,usekernel = FALSE, fL = 0.5)
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  lfda_find_knn_SSq2[[i]]=confusionMatrix(T_lfda_SSq2[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_lfda_SSq2_value=do.call(rbind,lapply(lfda_find_knn_SSq2, data.frame))

knn_lfda_SSq2=cbind(knn,find_lfda_SSq2_value)
write.csv(knn_lfda_SSq2,file = "lfda_SSq2_scaled_Knn_accuracy_r_20.csv")


#### test for SS_Shannon
Shannon_all_scaled=scale(Shannon_all[,-1])

knn=1:99L
find_knn_SSSh=list()
T_kfda_SSSh=list()
for (i in 1:length(knn)){
  find_knn_SSSh[[i]]=list()
  T_kfda_SSSh[[i]]=list()
  T_kfda_SSSh[[i]]=klfda_1(as.matrix(Shannon_all_scaled),as.factor(Model),r=20,kernel=rbfdot(sigma = 1),tol=1e-90,knn=knn[[i]],prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_knn_SSSh[[i]]=confusionMatrix(T_kfda_SSSh[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_klfda_SSSh_value=do.call(rbind,lapply(find_knn_SSSh, data.frame))

knn_klfda_SSSh=cbind(knn,find_klfda_SSSh_value)
write.csv(knn_klfda_SSSh,file = "klfda_SSSh_scaled_Knn_accuracy_r_20.csv")

### klfda find r

r=2:48L
find_r_SS_Sh=list()
T_kfda_r_SS_Sh=list()
for (i in 1:length(r)){
  find_r_SS_Sh[[i]]=list()
  T_kfda_r_SS_Sh[[i]]=list()
  T_kfda_r_SS_Sh=klfda_1(as.matrix(Shannon_all_scaled),as.factor(Model),r=r[[i]],kernel=rbfdot(sigma = 1),tol=1e-9000,knn=76,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SS_Sh=klfda_1(as.matrix(SS_Sh_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_r_SS_Sh[[i]]=confusionMatrix(T_kfda_r_SS_Sh$bayes_assigment$class,Model)$overall[[1]]
}

find_r_klfda_SS_Sh_value=do.call(rbind,lapply(find_r_SS_Sh, data.frame))

knn_r_klfda_SS_Sh=cbind(r,find_r_klfda_SS_Sh_value)

write.csv(knn_r_klfda_SS_Sh,file = "r_accuracy_klfda_SS_Sh_scaled_scaled_knn=39_update_kernelmatrix.csv")






knn=1:99L
lfda_find_knn_SSSh=list()
T_lfda_SSSh=list()
for (i in 1:length(knn)){
  lfda_find_knn_SSSh[[i]]=list()
  T_lfda_SSSh[[i]]=list()
  T_lfda_SSSh[[i]]=lfda_1(as.matrix(Shannon_all_scaled),as.factor(Model),r=20,knn=knn[[i]],prior=c(1,1,1,1,1)/5,tol=1e-30,CV=FALSE,usekernel = FALSE, fL = 0.5)
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  lfda_find_knn_SSSh[[i]]=confusionMatrix(T_lfda_SSSh[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_lfda_SSSh_value=do.call(rbind,lapply(lfda_find_knn_SSSh, data.frame))

knn_lfda_SSSh=cbind(knn,find_lfda_SSSh_value)
write.csv(knn_lfda_SSSh,file = "lfda_SSSh_scaled_Knn_accuracy_r_20.csv")



### test for SS trad (SSq0+SSq2)
SS_trad_scaled=scale(SS_tra[,-1])

knn=1:99L
find_knn_SS_trad=list()
T_kfda_SS_trad=list()
for (i in 1:length(knn)){
  find_knn_SS_trad[[i]]=list()
  T_kfda_SS_trad[[i]]=list()
  T_kfda_SS_trad[[i]]=klfda_1(as.matrix(SS_trad_scaled),as.factor(Model),r=20,kernel=rbfdot(sigma = 1),tol=1e-90,knn=knn[[i]],prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_knn_SS_trad[[i]]=confusionMatrix(T_kfda_SS_trad[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_klfda_SS_trad_value=do.call(rbind,lapply(find_knn_SS_trad, data.frame))

knn_klfda_SS_trad=cbind(knn,find_klfda_SS_trad_value)
write.csv(knn_klfda_SS_trad,file = "klfda_SS_trad_scaled_Knn_accuracy_r_20.csv")

##### find r for SS trad

r=2:88L
find_r_SS_trad=list()
T_kfda_r_SS_trad=list()
for (i in 1:length(r)){
  find_r_SS_trad[[i]]=list()
  T_kfda_r_SS_trad[[i]]=list()
  T_kfda_r_SS_trad=klfda_1(as.matrix(SS_trad_scaled),as.factor(Model),r=r[[i]],kernel=rbfdot(sigma = 1),tol=1e-9000,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SS_trad=klfda_1(as.matrix(SS_trad_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_r_SS_trad[[i]]=confusionMatrix(T_kfda_r_SS_trad$bayes_assigment$class,Model)$overall[[1]]
}

find_r_klfda_SS_trad_value=do.call(rbind,lapply(find_r_SS_trad, data.frame))

knn_r_klfda_SS_trad=cbind(r,find_r_klfda_SS_trad_value)

write.csv(knn_r_klfda_SS_trad,file = "r_accuracy_klfda_SS_trad_scaled_scaled_knn=7_update_kernelmatrix.csv")



knn=1:99L
lfda_find_knn_SS_trad=list()
T_lfda_SS_trad=list()
for (i in 1:length(knn)){
  lfda_find_knn_SS_trad[[i]]=list()
  T_lfda_SS_trad[[i]]=list()
  T_lfda_SS_trad[[i]]=lfda_1(as.matrix(SS_trad_scaled),as.factor(Model),r=20,knn=knn[[i]],prior=c(1,1,1,1,1)/5,tol=1e-30,CV=FALSE,usekernel = FALSE, fL = 0.5)
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  lfda_find_knn_SS_trad[[i]]=confusionMatrix(T_lfda_SS_trad[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_lfda_SS_trad_value=do.call(rbind,lapply(lfda_find_knn_SS_trad, data.frame))

knn_lfda_SS_trad=cbind(knn,find_lfda_SS_trad_value)
write.csv(knn_lfda_SS_trad,file = "lfda_SS_trad_scaled_Knn_accuracy_r_20.csv")



### Now test for scaled SSall :SSq0_SSq2+1D
SSall_scaled=scale(SSall[,-1])

knn=1:99L
find_knn_SSall=list()
T_kfda_SSall=list()
for (i in 1:length(knn)){
  find_knn_SSall[[i]]=list()
  T_kfda_SSall[[i]]=list()
  T_kfda_SSall[[i]]=klfda_1(as.matrix(SSall_scaled),as.factor(Model),r=20,kernel=rbfdot(sigma = 1),tol=1e-90,knn=knn[[i]],prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_knn_SSall[[i]]=confusionMatrix(T_kfda_SSall[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_klfda_SSall_value=do.call(rbind,lapply(find_knn_SSall, data.frame))

knn_klfda_SSall=cbind(knn,find_klfda_SSall_value)
write.csv(knn_klfda_SSall,file = "klfda_SSall_scaled_Knn_accuracy_r_20.csv")

### find r for SSall (q0+q2)


r=2:136L
find_r_SS_q0_q2=list()
T_kfda_r_SS_q0_q2=list()
for (i in 1:length(r)){
  find_r_SS_q0_q2[[i]]=list()
  T_kfda_r_SS_q0_q2[[i]]=list()
  T_kfda_r_SS_q0_q2=klfda_1(as.matrix(SSall_scaled),as.factor(Model),r=r[[i]],kernel=rbfdot(sigma = 1),tol=1e-9000,knn=8,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SS_q0_q2=klfda_1(as.matrix(SS_q0_q2_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_r_SS_q0_q2[[i]]=confusionMatrix(T_kfda_r_SS_q0_q2$bayes_assigment$class,Model)$overall[[1]]
}

find_r_klfda_SS_q0_q2_value=do.call(rbind,lapply(find_r_SS_q0_q2, data.frame))

knn_r_klfda_SS_q0_q2=cbind(r[1:98],find_r_klfda_SS_q0_q2_value)

write.csv(knn_r_klfda_SS_q0_q2,file = "r_accuracy_klfda_SS_q0_q2_scaled_scaled_knn=8_update_kernelmatrix.csv")



knn=1:99L
lfda_find_knn_SSall=list()
T_lfda_SSall=list()
for (i in 1:length(knn)){
  lfda_find_knn_SSall[[i]]=list()
  T_lfda_SSall[[i]]=list()
  T_lfda_SSall[[i]]=lfda_1(as.matrix(SSall_scaled),as.factor(Model),r=20,knn=knn[[i]],prior=c(1,1,1,1,1)/5,tol=1e-30,CV=FALSE,usekernel = FALSE, fL = 0.5)
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  lfda_find_knn_SSall[[i]]=confusionMatrix(T_lfda_SSall[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_lfda_SSall_value=do.call(rbind,lapply(lfda_find_knn_SSall, data.frame))

knn_lfda_SSall=cbind(knn,find_lfda_SSall_value)
write.csv(knn_lfda_SSall,file = "lfda_SSall_scaled_Knn_accuracy_r_20.csv")


##### Test for SSq0_Shannon_q2
SSq0_q2_Shannon_scaled=scale(SSq0_q2_Shannon[,-1])
knn=1:99L
find_knn_SSq0_q2_Shannon=list()
T_kfda_SSq0_q2_Shannon=list()
for (i in 1:length(knn)){
  find_knn_SSq0_q2_Shannon[[i]]=list()
  T_kfda_SSq0_q2_Shannon[[i]]=list()
  T_kfda_SSq0_q2_Shannon[[i]]=klfda_1(as.matrix(SSq0_q2_Shannon_scaled),as.factor(Model),r=20,kernel=rbfdot(sigma = 1),tol=1e-90,knn=knn[[i]],prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_knn_SSq0_q2_Shannon[[i]]=confusionMatrix(T_kfda_SSq0_q2_Shannon[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_klfda_SSq0_q2_Shannon_value=do.call(rbind,lapply(find_knn_SSq0_q2_Shannon, data.frame))

knn_klfda_SSq0_q2_Shannon=cbind(knn,find_klfda_SSq0_q2_Shannon_value)
write.csv(knn_klfda_SSq0_q2_Shannon,file = "klfda_SSq0_q2_Shannon_scaled_Knn_accuracy_r_20.csv")

##### find r for q0_shannon_q2


r=2:136L
find_r_SS_q0_Shannon_q2=list()
T_kfda_r_SS_q0_Shannon_q2=list()
for (i in 1:length(r)){
  find_r_SS_q0_Shannon_q2[[i]]=list()
  T_kfda_r_SS_q0_Shannon_q2[[i]]=list()
  T_kfda_r_SS_q0_Shannon_q2=klfda_1(as.matrix(SSq0_q2_Shannon_scaled),as.factor(Model),r=r[[i]],kernel=rbfdot(sigma = 1),tol=1e-9000,knn=15,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  #kfda_SS_q0_Shannon_q2=klfda_1(as.matrix(SS_q0_Shannon_q2_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_r_SS_q0_Shannon_q2[[i]]=confusionMatrix(T_kfda_r_SS_q0_Shannon_q2$bayes_assigment$class,Model)$overall[[1]]
}

find_r_klfda_SS_q0_Shannon_q2_value=do.call(rbind,lapply(find_r_SS_q0_Shannon_q2, data.frame))

knn_r_klfda_SS_q0_Shannon_q2=cbind(r[1:98],find_r_klfda_SS_q0_Shannon_q2_value)

write.csv(knn_r_klfda_SS_q0_Shannon_q2,file = "r_accuracy_klfda_SS_q0_Shannon_q2_scaled_scaled_knn=15_update_kernelmatrix.csv")



knn=1:99L
lfda_find_knn_SSq0_q2_Shannon=list()
T_lfda_SSq0_q2_Shannon=list()
for (i in 1:length(knn)){
  lfda_find_knn_SSq0_q2_Shannon[[i]]=list()
  T_lfda_SSq0_q2_Shannon[[i]]=list()
  T_lfda_SSq0_q2_Shannon[[i]]=lfda_1(as.matrix(SSq0_q2_Shannon_scaled),as.factor(Model),r=20,knn=knn[[i]],prior=c(1,1,1,1,1)/5,tol=1e-30,CV=FALSE,usekernel = FALSE, fL = 0.5)
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  lfda_find_knn_SSq0_q2_Shannon[[i]]=confusionMatrix(T_lfda_SSq0_q2_Shannon[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_lfda_SSq0_q2_Shannon_value=do.call(rbind,lapply(lfda_find_knn_SSq0_q2_Shannon, data.frame))

knn_lfda_SSq0_q2_Shannon=cbind(knn,find_lfda_SSq0_q2_Shannon_value)
write.csv(knn_lfda_SSq0_q2_Shannon,file = "lfda_SSq0_q2_Shannon_scaled_Knn_accuracy_r_20.csv")

### tets for SS_q1_Shannon

Sh_1D_scaled=scale(SSq1_Shannnon[,-1])
knn=1:99L
find_knn_SS_Sh_1D=list()
T_kfda_SS_Sh_1D=list()
for (i in 1:length(knn)){
  find_knn_SS_Sh_1D[[i]]=list()
  T_kfda_SS_Sh_1D[[i]]=list()
  T_kfda_SS_Sh_1D[[i]]=klfda_1(as.matrix(Sh_1D_scaled),as.factor(Model),r=20,kernel=rbfdot(sigma = 1),tol=1e-90,knn=knn[[i]],prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_knn_SS_Sh_1D[[i]]=confusionMatrix(T_kfda_SS_Sh_1D[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_klfda_SS_Sh_1D_value=do.call(rbind,lapply(find_knn_SS_Sh_1D, data.frame))

knn_klfda_SS_Sh_1D=cbind(knn,find_klfda_SS_Sh_1D_value)
write.csv(knn_klfda_SS_Sh_1D,file = "klfda_SS_Sh_1D_scaled_Knn_accuracy_r_20.csv")

#### find r for SSq1_shannon

r=2:90L
find_r_SS_q1_shannon=list()
T_kfda_r_SS_q1_shannon=list()
for (i in 1:length(r)){
  find_r_SS_q1_shannon[[i]]=list()
  T_kfda_r_SS_q1_shannon[[i]]=list()
  T_kfda_r_SS_q1_shannon=klfda_1(as.matrix(Sh_1D_scaled),as.factor(Model),r=r[[i]],kernel=rbfdot(sigma = 1),tol=1e-9000,knn=4,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  #kfda_SS_q1_shannon=klfda_1(as.matrix(SS_q1_shannon_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_r_SS_q1_shannon[[i]]=confusionMatrix(T_kfda_r_SS_q1_shannon$bayes_assigment$class,Model)$overall[[1]]
}

find_r_klfda_SS_q1_shannon_value=do.call(rbind,lapply(find_r_SS_q1_shannon, data.frame))

knn_r_klfda_SS_q1_shannon=cbind(r,find_r_klfda_SS_q1_shannon_value)

write.csv(knn_r_klfda_SS_q1_shannon,file = "r_accuracy_klfda_SS_q1_shannon_scaled_scaled_knn=4_update_kernelmatrix.csv")



knn=1:99L
lfda_find_knn_SS_Sh_1D=list()
T_lfda_SS_Sh_1D=list()
for (i in 1:length(knn)){
  lfda_find_knn_SS_Sh_1D[[i]]=list()
  T_lfda_SS_Sh_1D[[i]]=list()
  T_lfda_SS_Sh_1D[[i]]=lfda_1(as.matrix(Sh_1D_scaled),as.factor(Model),r=20,knn=knn[[i]],prior=c(1,1,1,1,1)/5,tol=1e-30,CV=FALSE,usekernel = FALSE, fL = 0.5)
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  lfda_find_knn_SS_Sh_1D[[i]]=confusionMatrix(T_lfda_SS_Sh_1D[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_lfda_SS_Sh_1D_value=do.call(rbind,lapply(lfda_find_knn_SS_Sh_1D, data.frame))

knn_lfda_SS_Sh_1D=cbind(knn,find_lfda_SS_Sh_1D_value)
write.csv(knn_lfda_SS_Sh_1D,file = "lfda_SS_Sh_1D_scaled_Knn_accuracy_r_20.csv")

##### Test for SSq0_shannon_q1_1D_all
SSq0_q2_Sh_D_scaled=scale(SSq0_q2_Sh_D[,-1])
knn=1:99L
find_knn_SSq0_q2_Sh_D=list()
T_kfda_SSq0_q2_Sh_D=list()
for (i in 1:length(knn)){
  find_knn_SSq0_q2_Sh_D[[i]]=list()
  T_kfda_SSq0_q2_Sh_D[[i]]=list()
  T_kfda_SSq0_q2_Sh_D[[i]]=klfda_1(as.matrix(SSq0_q2_Sh_D_scaled),as.factor(Model),r=20,kernel=rbfdot(sigma = 1),tol=1e-90,knn=knn[[i]],prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_knn_SSq0_q2_Sh_D[[i]]=confusionMatrix(T_kfda_SSq0_q2_Sh_D[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_klfda_SSq0_q2_Sh_D_value=do.call(rbind,lapply(find_knn_SSq0_q2_Sh_D, data.frame))

knn_klfda_SSq0_q2_Sh_D=cbind(knn,find_klfda_SSq0_q2_Sh_D_value)
write.csv(knn_klfda_SSq0_q2_Sh_D,file = "klfda_SSq0_q2_Sh_D_all_scaled_Knn_accuracy_r_20.csv")

### find r klfda for SSq0_q2_Sh_D


r=2:178L
find_r_SS_q0_q2_Sh_D=list()
T_kfda_r_SS_q0_q2_Sh_D=list()
for (i in 1:length(r)){
  find_r_SS_q0_q2_Sh_D[[i]]=list()
  T_kfda_r_SS_q0_q2_Sh_D[[i]]=list()
  T_kfda_r_SS_q0_q2_Sh_D=klfda_1(as.matrix(SSq0_q2_Sh_D_scaled),as.factor(Model),r=r[[i]],kernel=rbfdot(sigma = 1),tol=1e-9000,knn=9,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  #kfda_SS_q0_q2_Sh_D=klfda_1(as.matrix(SS_q0_q2_Sh_D_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  find_r_SS_q0_q2_Sh_D[[i]]=confusionMatrix(T_kfda_r_SS_q0_q2_Sh_D$bayes_assigment$class,Model)$overall[[1]]
}

find_r_klfda_SS_q0_q2_Sh_D_value=do.call(rbind,lapply(find_r_SS_q0_q2_Sh_D, data.frame))

knn_r_klfda_SS_q0_q2_Sh_D=cbind(r[1:98],find_r_klfda_SS_q0_q2_Sh_D_value)

write.csv(knn_r_klfda_SS_q0_q2_Sh_D,file = "r_accuracy_klfda_SS_q0_q2_Sh_D_scaled_scaled_knn=9_update_kernelmatrix.csv")



knn=1:99L
lfda_find_knn_SSq0_q2_Sh_D=list()
T_lfda_SSq0_q2_Sh_D=list()
for (i in 1:length(knn)){
  lfda_find_knn_SSq0_q2_Sh_D[[i]]=list()
  T_lfda_SSq0_q2_Sh_D[[i]]=list()
  T_lfda_SSq0_q2_Sh_D[[i]]=lfda_1(as.matrix(SSq0_q2_Sh_D_scaled),as.factor(Model),r=20,knn=knn[[i]],prior=c(1,1,1,1,1)/5,tol=1e-30,CV=FALSE,usekernel = FALSE, fL = 0.5)
  
  #kfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=10,kernel=rbfdot(sigma = 1),tol=1e-90,knn=7,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  lfda_find_knn_SSq0_q2_Sh_D[[i]]=confusionMatrix(T_lfda_SSq0_q2_Sh_D[[i]]$bayes_assigment$class,Model)$overall[[1]]
}

find_lfda_SSq0_q2_Sh_D_value=do.call(rbind,lapply(lfda_find_knn_SSq0_q2_Sh_D, data.frame))

knn_lfda_SSq0_q2_Sh_D=cbind(knn,find_lfda_SSq0_q2_Sh_D_value)
write.csv(knn_lfda_SSq0_q2_Sh_D,file = "lfda_SSq0_q2_Sh_D_scaled_Knn_accuracy_r_20.csv")


#### now we can do test and plot the subspace into 3D plot
###  now we found klfda is better than lfda, thus we used klfda

klfda_SSq0=klfda_1(as.matrix(SSq0_scaled),as.factor(Model),r=20,knn=10,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SSq1=klfda_1(as.matrix(SSq1_scaled),as.factor(Model),r=20,knn=7,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SSq2=klfda_1(as.matrix(SSq2_scaled),as.factor(Model),r=20,knn=39,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SS_sh=klfda_1(as.matrix(Shannon_all_scaled),as.factor(Model),r=20,knn=76,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SS_trad=klfda_1(as.matrix(SS_trad_scaled),as.factor(Model),r=20,knn=7,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
#### not ssall is SSq0+SSq1+SSq2, not include SSshannon
klfda_SS_q0_1D_q2=klfda_1(as.matrix(SSall_scaled),as.factor(Model),r=20,knn=8,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")

klfda_Sh_1D_scaled=klfda_1(as.matrix(Sh_1D_scaled),as.factor(Model),r=20,knn=4,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SS_q0_Sh_q2=klfda_1(as.matrix(SSq0_q2_Shannon_scaled),as.factor(Model),r=20,knn=15,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
klfda_SS_q0_q2_Sh_1D=klfda_1(as.matrix(SSq0_q2_Sh_D_scaled),as.factor(Model),r=20,knn=9,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")


confusionMatrix(klfda_SSq1$bayes_assigment$class,Model)
Z_0=klfda_SSq0$Z
Z_1=klfda_SSq1$Z
Z_2=klfda_SSq2$Z
Z_sh=klfda_SS_sh$Z
Z_trad=klfda_SS_trad$Z
Z_q0_1D_q2=klfda_SS_q0_1D_q2$Z
Z=klfda_Sh_1D_scaled$Z
Z=klfda_SS_q0_Sh_q2$Z
Z=klfda_SS_q0_q2_Sh_1D$Z

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

p_SSq0

p_SSq0 <- plot_ly(as.data.frame(Z), x =Z[,1], y =Z[,2],z=Z[,3],  color = Model,colors=cols2) %>% 
  layout(autosize = TRUE)%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LDA1'),
                      yaxis = list(title = 'LDA2'),
                      zaxis = list(title = 'LDA3')))

p_SSq0
####  cross validation to test the performance
## SShanon

Sh_lfda=list()
n=500
Sh_lfda_model=list()
Sh_pred_lfd=list()
Sh_pred_lfd0=list()
Sh_pred_lfd1=list()
Sh_pred_lfd2=list()
for(i in 1:n) {
  Sh_lfda[[i]]=list()
  Sh_pred_lfd[[i]]=list()
  Sh_pred_lfd0[[i]]=list()
  Sh_pred_lfd1[[i]]=list()
  Sh_pred_lfd2[[i]]=list()
  #predict1[[i]]=list()
  Sh_lfda[[i]] = lfda_1(as.matrix(Shannon_all_scaled[-i,]),as.data.frame(Model)[-i,],r=20,tol=1e-30,CV=FALSE,usekernel = FALSE, fL = 0.5,knn = 34)
  Sh_pred_lfd[[i]]=predict.lfda(Sh_lfda[[i]],as.matrix(as.data.frame(SSq0_scaled)[i,]),prior = NULL)
  Sh_pred_lfd0[[i]]=Sh_pred_lfd[[i]]$class
  Sh_pred_lfd1[[i]]=Sh_pred_lfd[[i]]$bayes_judgement$post_class
  Sh_pred_lfd2[[i]]=Sh_pred_lfd[[i]]$bayes_assigment$class
  # predicSh = function(i) predict.lfda(lfda[[i]],Shannon[i,])[1] 
  
  Sh_lfda_model[[i]]=Model[i]
}

Sh_class_pred_lfda0=do.call(rbind,lapply(Sh_pred_lfd0, data.frame))
Sh_class_pred_lfda1=do.call(rbind,lapply(Sh_pred_lfd1, data.frame))
Sh_class_pred_lfda2=do.call(rbind,lapply(Sh_pred_lfd2, data.frame))


Sh_lfda_model_class=do.call(rbind,lapply(Sh_lfda_model, data.frame))

confusionMatrix(Sh_class_pred_lfda2$X..i..,Sh_lfda_model_class$X..i..)






lfda_micro = lfda_1(as.matrix(t11),as.matrix(t22),r=10,tol=1e-90,CV=FALSE,usekernel = TRUE, fL = 0.5,knn = 6)

confusionMatrix(t22,lfda_micro$bayes_assigment$class)


lfda=list()
n=500
lfda_model=list()
pred_lfd=list()
pred_lfd0=list()
pred_lfd1=list()
pred_lfd2=list()
for(i in 1:n) {
  lfda[[i]]=list()
  pred_lfd[[i]]=list()
  pred_lfd0[[i]]=list()
  pred_lfd1[[i]]=list()
  pred_lfd2[[i]]=list()
  #predict1[[i]]=list()
  lfda[[i]] = lfda_1(t11[-i,],as.data.frame(t22)[-i,],r=10,tol=1e-30,CV=FALSE,usekernel = FALSE, fL = 0.5,knn = 26)
  pred_lfd[[i]]=predict.lfda(lfda[[i]],as.matrix(as.data.frame(t11)[i,]),prior = NULL)
  pred_lfd0[[i]]=pred_lfd[[i]]$class
  pred_lfd1[[i]]=pred_lfd[[i]]$bayes_judgement$post_class
  pred_lfd2[[i]]=pred_lfd[[i]]$bayes_assigment$class
  # predict1 = function(i) predict.lfda(lfda[[i]],Shannon[i,])[1] 
  
  lfda_model[[i]]=t22[i]
}

class_pred_lfda0=do.call(rbind,lapply(pred_lfd0, data.frame))
class_pred_lfda1=do.call(rbind,lapply(pred_lfd1, data.frame))
class_pred_lfda2=do.call(rbind,lapply(pred_lfd2, data.frame))


lfda_model_class=do.call(rbind,lapply(lfda_model, data.frame))

confusionMatrix(lfda_model_class$X..i..,class_pred_lfda2$X..i..)




KFDA_SSq1=KFDA(as.matrix(SSq1[,-1]),as.matrix(Model),r=10,order=2,kernel="gaussian", usekernel=TRUE,fL=0.5,regParam=0.25, priors=NULL,tol=1e-90,reg=0.01,metric =  'plain',plotFigures=FALSE,verbose=TRUE)

Pred_KFDA=predict.KFDA(KFDA_SSq1,as.matrix(SSq1[c(20,120,220,320,420),-1]))

Pred_KFDA$posteriors.class1

kfda_SSq1=klfda_1(as.matrix(SSq1_scaled),as.factor(Model),r=10,knn=7,kernel=rbfdot(sigma = 1),tol=1e-90,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
pred_kfda= predict.kfda(kfda_SSq1,as.matrix(SSq1_scaled[c(20,120,220,320,420),]),dkernel=rbfdot(sigma = 1),prior = NULL)
pred_kfda$bayes_assig_pred$class

gaussian



kfda=klfda_1(as.matrix(SSq1[-i,-1]),as.factor(Model)[-i],r=20,knn=10,kernel=rbfdot(sigma = 1),tol=1e-90,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
pred_kfda= predict.kfda(kfda,as.matrix(SSq1[i,-1]),prior = NULL)
pred_kfda$bayes_jud_pred$post_class
confusionMatrix(kfda$bayes_assigment$class,Model[-i])

##### CV klfda

## SSq0

klfdaq0=list()
n=500
klfdaq0_model=list()
pred_klfdaq0=list()

pred_klfdaq02=list()
pred_klfdaq0p=list()
pred_klfdaq0Z=list()
for(i in 1:n) {
  klfdaq0[[i]]=list()
  pred_klfdaq0[[i]]=list()
  
  pred_klfdaq02[[i]]=list()
  pred_klfdaq0p[[i]]=list()
  pred_klfdaq0Z[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  klfdaq0=klfda_1(as.matrix(SSq0_scaled[-i,]),Model[-i],r=20,prior=NULL,knn=10,kernel=rbfdot(sigma = 0.5),tol=1e-30,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  pred_klfdaq0[[i]]= predict.kfda(klfdaq0,as.matrix(as.data.frame(SSq0_scaled)[i,]), prior = NULL)
  # pred_klfdaq00[[i]]=pred_klfdaq0[[i]]$posteriors.class
  # pred_klfdaq01[[i]]=pred_klfdaq0[[i]]$posteriors.class0
  pred_klfdaq02[[i]]=pred_klfdaq0[[i]]$posteriors.class1
  pred_klfdaq0p[[i]]=pred_klfdaq0[[i]]$bayes_jud_pred$post_class
  pred_klfdaq0Z[[i]]=pred_klfdaq0[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfdaq0a(klfdaq0a[[i]],Shannon[i,])[1] 
  
  klfdaq0_model[[i]]=Model[i]
}

class_pred_klfdaq02=do.call(rbind,lapply(pred_klfdaq02, data.frame))
class_pred_klfdaq0p=do.call(rbind,lapply(pred_klfdaq0p, data.frame))
class_pred_klfdaq0Z=do.call(rbind,lapply(pred_klfdaq0Z, data.frame))

klfdaq0_model_class=do.call(rbind,lapply(klfdaq0_model, data.frame))

confusionMatrix(class_pred_klfdaq0Z$X..i..,klfdaq0_model_class$X..i..)


##### not scaled

Sklfdaq0=list()
n=500
Sklfdaq0_model=list()
Spred_klfdaq0=list()

Spred_klfdaq02=list()
Spred_klfdaq0p=list()
Spred_klfdaq0Z=list()
for(i in 1:n) {
  Sklfdaq0[[i]]=list()
  Spred_klfdaq0[[i]]=list()
  
  Spred_klfdaq02[[i]]=list()
  Spred_klfdaq0p[[i]]=list()
  Spred_klfdaq0Z[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  Sklfdaq0=klfda_1(as.matrix(SSq0[-i,-1]),Model[-i],r=20,prior=NULL,knn=14,kernel=rbfdot(sigma = 0.5),tol=1e-30,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  Spred_klfdaq0[[i]]= predict.kfda(Sklfdaq0,as.matrix(as.data.frame(SSq0)[i,-1]), prior = NULL)
  # pred_klfdaq00[[i]]=pred_klfdaq0[[i]]$posteriors.class
  # pred_klfdaq01[[i]]=pred_klfdaq0[[i]]$posteriors.class0
  Spred_klfdaq02[[i]]=Spred_klfdaq0[[i]]$posteriors.class1
  Spred_klfdaq0p[[i]]=Spred_klfdaq0[[i]]$bayes_jud_pred$post_class
  Spred_klfdaq0Z[[i]]=Spred_klfdaq0[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfdaq0a(klfdaq0a[[i]],Shannon[i,])[1] 
  
  Sklfdaq0_model[[i]]=Model[i]
}

Sclass_pred_klfdaq02=do.call(rbind,lapply(Spred_klfdaq02, data.frame))
Sclass_pred_klfdaq0p=do.call(rbind,lapply(Spred_klfdaq0p, data.frame))
Sclass_pred_klfdaq0Z=do.call(rbind,lapply(Spred_klfdaq0Z, data.frame))

Sklfdaq0_model_class=do.call(rbind,lapply(Sklfdaq0_model, data.frame))

confusionMatrix(Sclass_pred_klfdaq02$X..i..,Sklfdaq0_model_class$X..i..)

#### find the best gamma parameters first
library(e1071)
gamma=c(0.001,0.005,0.01,0.05,0.1,0.5,1)

SSq1_tune=tune(klfda_1, train.x=as.matrix(SSq1[,-1]), train.y = as.factor(Model), validation.x= NULL, validation.y = NULL, kernel=rbfdot(sigma = 0.001:1),r=20,knn=7, tol=1e-90,prior=NULL,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain",predict.func = predict.kfda)



##SSq1
SSq1_scaled0=as.data.frame(SSq1_scaled)
klfdaq1=list()
m=500
klfdaq1_model=list()
pred_klfdaq1=list()
pred_klfdaq10=list()
pred_klfdaq11=list()
pred_klfdaq12=list()
pred_klfdaq1p=list()
pred_klfdaq1Z=list()
for(i in 1:m) {
  klfdaq1[[i]]=list()
  pred_klfdaq1[[i]]=list()
  pred_klfdaq10[[i]]=list()
  pred_klfdaq11[[i]]=list()
  pred_klfdaq12[[i]]=list()
  pred_klfdaq1p[[i]]=list()
  pred_klfdaq1Z[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  klfdaq1=klfda_1(as.matrix(SSq1[-i,-1]),Model[-i],r=10,kernel=rbfdot(sigma = 0.5),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  pred_klfdaq1[[i]]= predict.kfda(klfdaq1,as.matrix(as.data.frame(SSq1)[i,-1]),prior = NULL)
  # pred_klfdaq10[[i]]=pred_klfdaq1[[i]]$posteriors.class
  # pred_klfdaq11[[i]]=pred_klfdaq1[[i]]$posteriors.class0
  pred_klfdaq12[[i]]=pred_klfdaq1[[i]]$posteriors.class1
  pred_klfdaq1p[[i]]=pred_klfdaq1[[i]]$bayes_jud_pred$cl2
  pred_klfdaq1Z[[i]]=pred_klfdaq1[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfdaq1a(klfdaq1a[[i]],Shannon[i,])[1] 
  
  klfdaq1_model[[i]]=Model[i]
}
#### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
class_pred_klfdaq12=do.call(rbind,lapply(pred_klfdaq12, data.frame))
class_pred_klfdaq1p=do.call(rbind,lapply(pred_klfdaq1p, data.frame))
class_pred_klfdaq1Z=do.call(rbind,lapply(pred_klfdaq1Z, data.frame))

klfdaq1_model_class=do.call(rbind,lapply(klfdaq1_model, data.frame))

confusionMatrix(class_pred_klfdaq1Z$X..i..,klfdaq1_model_class$X..i..)



capture.output(confusionMatrix(class_pred_klfdaq1Z$X..i..,klfdaq1_model_class$X..i..),file = "Confusionmatrix_1D_klfda_VC.txt")

lapply(a0, function(x) write.table(data.frame(x), 'confusionmatrixklfdaSSq1.csv'  , append= T, sep=',' ))
### this a little change for data
Sklfdaq1=list()
m=500
Sklfdaq1_model=list()
Spred_klfdaq1=list()
Spred_klfdaq10=list()
Spred_klfdaq11=list()
Spred_klfdaq12=list()
Spred_klfdaq1p=list()
Spred_klfdaq1Z=list()
for(i in 1:m) {
  Sklfdaq1[[i]]=list()
  Spred_klfdaq1[[i]]=list()
  Spred_klfdaq10[[i]]=list()
  Spred_klfdaq11[[i]]=list()
  Spred_klfdaq12[[i]]=list()
  Spred_klfdaq1p[[i]]=list()
  Spred_klfdaq1Z[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  Sklfdaq1=klfda_1(as.matrix(SSq1_scaled[-i,]),Model[-i],r=20,kernel=rbfdot(sigma = 0.5),knn=7,tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  Spred_klfdaq1[[i]]= predict.kfda(Sklfdaq1,as.matrix(as.data.frame(SSq1_scaled)[i,]),prior = NULL)
  # pred_klfdaq10[[i]]=pred_klfdaq1[[i]]$posteriors.class
  # pred_klfdaq11[[i]]=pred_klfdaq1[[i]]$posteriors.class0
  Spred_klfdaq12[[i]]=Spred_klfdaq1[[i]]$posteriors.class1
  Spred_klfdaq1p[[i]]=Spred_klfdaq1[[i]]$bayes_jud_pred$cl2
  Spred_klfdaq1Z[[i]]=Spred_klfdaq1[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfdaq1a(klfdaq1a[[i]],Shannon[i,])[1] 
  
  Sklfdaq1_model[[i]]=Model[i]
}
#### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
Sclass_pred_klfdaq12=do.call(rbind,lapply(Spred_klfdaq12, data.frame))
Sclass_pred_klfdaq1p=do.call(rbind,lapply(Spred_klfdaq1p, data.frame))
Sclass_pred_klfdaq1Z=do.call(rbind,lapply(Spred_klfdaq1Z, data.frame))

Sklfdaq1_model_class=do.call(rbind,lapply(Sklfdaq1_model, data.frame))

confusionMatrix(Sclass_pred_klfdaq1Z$X..i..,Sklfdaq1_model_class$X..i..)
##SSq2

klfdaq2=list()
n=500
klfdaq2_model=list()
pred_klfdaq2=list()
pred_klfdaq20=list()
pred_klfdaq21=list()
pred_klfdaq22=list()
pred_klfdaq2p=list()
pred_klfdaq2Z=list()
for(i in 1:n) {
  klfdaq2[[i]]=list()
  pred_klfdaq2[[i]]=list()
  pred_klfdaq20[[i]]=list()
  pred_klfdaq21[[i]]=list()
  pred_klfdaq22[[i]]=list()
  pred_klfdaq2p[[i]]=list()
  pred_klfdaq2Z[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  klfdaq2[[i]]=klfda_1(as.matrix(SSq2_scaled[-i,]),as.factor(Model)[-i],r=20,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=39,metric="plain")
  pred_klfdaq2[[i]]= predict.kfda(klfdaq2[[i]],as.matrix(as.data.frame(SSq2_scaled)[i,]),dkernel=rbfdot(sigma = 1),prior = NULL)
  # pred_klfdaq20[[i]]=pred_klfdaq2[[i]]$posteriors.class
  # pred_klfdaq21[[i]]=pred_klfdaq2[[i]]$posteriors.class0
  pred_klfdaq22[[i]]=pred_klfdaq2[[i]]$posteriors.class1
  pred_klfdaq2p[[i]]=pred_klfdaq2[[i]]$bayes_jud_pred$post_class
  pred_klfdaq2Z[[i]]=pred_klfdaq2[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfdaq2a(klfdaq2a[[i]],Shannon[i,])[1] 
  
  klfdaq2_model[[i]]=Model[i]
}
#### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
class_pred_klfdaq22 = do.call(rbind,lapply(pred_klfdaq22, data.frame))
class_pred_klfdaq2p =do.call(rbind,lapply(pred_klfdaq2p, data.frame))
class_pred_klfdaq2Z =do.call(rbind,lapply(pred_klfdaq2Z, data.frame))

klfdaq2_model_class=do.call(rbind,lapply(klfdaq2_model, data.frame))

confusionMatrix(class_pred_klfdaq2Z$X..i..,klfdaq2_model_class$X..i..)


##

##SS_shannon

klfdash=list()
n=500
klfdash_model=list()
pred_klfdash=list()
pred_klfdash0=list()
pred_klfdash1=list()
pred_klfdash2=list()
pred_klfdashp=list()
pred_klfdashZ=list()
for(i in 1:n) {
  klfdash[[i]]=list()
  pred_klfdash[[i]]=list()
  pred_klfdash0[[i]]=list()
  pred_klfdash1[[i]]=list()
  pred_klfdash2[[i]]=list()
  pred_klfdashp[[i]]=list()
  pred_klfdashZ[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  klfdash[[i]]=klfda_1(as.matrix(Shannon_all_scaled[-i,]),as.factor(Model)[-i],r=20,tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=76,metric="plain")
  pred_klfdash[[i]]= predict.kfda(klfdash[[i]],as.matrix(as.data.frame(Shannon_all_scaled)[i,]),prior = NULL)
  # pred_klfdash0[[i]]=pred_klfdash[[i]]$posteriors.class
  # pred_klfdash1[[i]]=pred_klfdash[[i]]$posteriors.class0
  pred_klfdash2[[i]]=pred_klfdash[[i]]$posteriors.class1
  pred_klfdashp[[i]]=pred_klfdash[[i]]$bayes_jud_pred$post_class
  pred_klfdashZ[[i]]=pred_klfdash[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfdasha(klfdasha[[i]],Shannon[i,])[1] 
  
  klfdash_model[[i]]=Model[i]
}
#### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
class_pred_klfdash2=do.call(rbind,lapply(pred_klfdash2, data.frame))
class_pred_klfdashp=do.call(rbind,lapply(pred_klfdashp, data.frame))
class_pred_klfdashZ=do.call(rbind,lapply(pred_klfdashZ, data.frame))

klfdash_model_class=do.call(rbind,lapply(klfdash_model, data.frame))

confusionMatrix(class_pred_klfdashZ$X..i..,klfdash_model_class$X..i..)

#####SS_trad

klfda_trad=list()
m=500
klfda_trad_model=list()
pred_klfda_trad=list()
pred_klfda_trad0=list()
pred_klfda_trad1=list()
pred_klfda_trad2=list()
pred_klfda_tradp=list()
pred_klfda_tradZ=list()
for(i in 1:m) {
  klfda_trad[[i]]=list()
  pred_klfda_trad[[i]]=list()
  pred_klfda_trad0[[i]]=list()
  pred_klfda_trad1[[i]]=list()
  pred_klfda_trad2[[i]]=list()
  pred_klfda_tradp[[i]]=list()
  pred_klfda_tradZ[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  klfda_trad[[i]]=klfda_1(as.matrix(SS_trad_scaled[-i,]),Model[-i],r=20,tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=7,metric="plain")
  pred_klfda_trad[[i]]= predict.kfda(klfda_trad[[i]],as.matrix(as.data.frame(SS_trad_scaled)[i,]),prior = NULL)
  # pred_klfda_trad0[[i]]=pred_klfda_trad[[i]]$posteriors.class
  # pred_klfda_trad1[[i]]=pred_klfda_trad[[i]]$posteriors.class0
  pred_klfda_trad2[[i]]=pred_klfda_trad[[i]]$posteriors.class1
  pred_klfda_tradp[[i]]=pred_klfda_trad[[i]]$bayes_jud_pred$post_class
  pred_klfda_tradZ[[i]]=pred_klfda_trad[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfda_trada(klfda_trada[[i]],Shannon[i,])[1] 
  
  klfda_trad_model[[i]]=Model[i]
}
#### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
class_pred_klfda_trad2=do.call(rbind,lapply(pred_klfda_trad2, data.frame))
class_pred_klfda_tradp=do.call(rbind,lapply(pred_klfda_tradp, data.frame))
class_pred_klfda_tradZ=do.call(rbind,lapply(pred_klfda_tradZ, data.frame))

klfda_trad_model_class=do.call(rbind,lapply(klfda_trad_model, data.frame))

confusionMatrix(class_pred_klfda_trad2$X..i..,klfda_trad_model_class$X..i..)

## SS_q0_q2_1D

klfda_q0_1D_q2=list()
n=500
klfda_q0_1D_q2_model=list()
pred_klfda_q0_1D_q2=list()
pred_klfda_q0_1D_q20=list()
pred_klfda_q0_1D_q21=list()
pred_klfda_q0_1D_q22=list()
pred_klfda_q0_1D_q2p=list()
pred_klfda_q0_1D_q2Z=list()
for(i in 1:n) {
  klfda_q0_1D_q2[[i]]=list()
  pred_klfda_q0_1D_q2[[i]]=list()
  pred_klfda_q0_1D_q20[[i]]=list()
  pred_klfda_q0_1D_q21[[i]]=list()
  pred_klfda_q0_1D_q22[[i]]=list()
  pred_klfda_q0_1D_q2p[[i]]=list()
  pred_klfda_q0_1D_q2Z[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  klfda_q0_1D_q2[[i]]=klfda_1(as.matrix(SSall[-i,-1]),as.factor(Model)[-i],r=20,kernel=rbfdot(sigma = 1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=8,metric="plain")
  pred_klfda_q0_1D_q2[[i]]= predict.kfda(klfda_q0_1D_q2[[i]],as.matrix(as.data.frame(SSall)[i,-1]),prior = NULL)
  # pred_klfda_q0_1D_q20[[i]]=pred_klfda_q0_1D_q2[[i]]$posteriors.class
  # pred_klfda_q0_1D_q21[[i]]=pred_klfda_q0_1D_q2[[i]]$posteriors.class0
  pred_klfda_q0_1D_q22[[i]]=pred_klfda_q0_1D_q2[[i]]$posteriors.class1
  pred_klfda_q0_1D_q2p[[i]]=pred_klfda_q0_1D_q2[[i]]$bayes_jud_pred$post_class
  pred_klfda_q0_1D_q2Z[[i]]=pred_klfda_q0_1D_q2[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfda_q0_1D_q2a(klfda_q0_1D_q2a[[i]],Shannon[i,])[1] 
  
  klfda_q0_1D_q2_model[[i]]=Model[i]
}
#### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
class_pred_klfda_q0_1D_q22=do.call(rbind,lapply(pred_klfda_q0_1D_q22, data.frame))
class_pred_klfda_q0_1D_q2p=do.call(rbind,lapply(pred_klfda_q0_1D_q2p, data.frame))
class_pred_klfda_q0_1D_q2Z=do.call(rbind,lapply(pred_klfda_q0_1D_q2Z, data.frame))

klfda_q0_1D_q2_model_class=do.call(rbind,lapply(klfda_q0_1D_q2_model, data.frame))

confusionMatrix(class_pred_klfda_q0_1D_q22$X..i..,klfda_q0_1D_q2_model_class$X..i..)



klfda_Sh_D=list()
n=500
klfda_Sh_D_model=list()
pred_klfda_Sh_D=list()
pred_klfda_Sh_D0=list()
pred_klfda_Sh_D1=list()
pred_klfda_Sh_D2=list()
pred_klfda_Sh_Dp=list()
pred_klfda_Sh_DZ=list()
for(i in 1:n) {
  klfda_Sh_D[[i]]=list()
  pred_klfda_Sh_D[[i]]=list()
  pred_klfda_Sh_D0[[i]]=list()
  pred_klfda_Sh_D1[[i]]=list()
  pred_klfda_Sh_D2[[i]]=list()
  pred_klfda_Sh_Dp[[i]]=list()
  pred_klfda_Sh_DZ[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  klfda_Sh_D[[i]]=klfda_1(as.matrix(Sh_1D_scaled[-i,]),as.factor(Model)[-i],r=20,kernel=rbfdot(sigma = 0.05),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=50,metric="plain")
  pred_klfda_Sh_D[[i]]= predict.kfda(klfda_Sh_D[[i]],as.matrix(as.data.frame(Sh_1D_scaled)[i,]),prior = NULL)
  # pred_klfda_Sh_D0[[i]]=pred_klfda_Sh_D[[i]]$posteriors.class
  # pred_klfda_Sh_D1[[i]]=pred_klfda_Sh_D[[i]]$posteriors.class0
  pred_klfda_Sh_D2[[i]]=pred_klfda_Sh_D[[i]]$posteriors.class1
  pred_klfda_Sh_Dp[[i]]=pred_klfda_Sh_D[[i]]$bayes_jud_pred$post_class
  pred_klfda_Sh_DZ[[i]]=pred_klfda_Sh_D[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfda_Sh_Da(klfda_Sh_Da[[i]],Shannon[i,])[1] 
  
  klfda_Sh_D_model[[i]]=Model[i]
}
#### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
class_pred_klfda_Sh_D2=do.call(rbind,lapply(pred_klfda_Sh_D2, data.frame))
class_pred_klfda_Sh_Dp=do.call(rbind,lapply(pred_klfda_Sh_Dp, data.frame))
class_pred_klfda_Sh_DZ=do.call(rbind,lapply(pred_klfda_Sh_DZ, data.frame))

klfda_Sh_D_model_class=do.call(rbind,lapply(klfda_Sh_D_model, data.frame))

confusionMatrix(class_pred_klfda_Sh_DZ$X..i..,klfda_Sh_D_model_class$X..i..)


klfda_Ar_H_He=list()
n=500
klfda_Ar_H_He_model=list()
pred_klfda_Ar_H_He=list()
pred_klfda_Ar_H_He0=list()
pred_klfda_Ar_H_He1=list()
pred_klfda_Ar_H_He2=list()
pred_klfda_Ar_H_Hep=list()
pred_klfda_Ar_H_HeZ=list()
for(i in 1:n) {
  klfda_Ar_H_He[[i]]=list()
  pred_klfda_Ar_H_He[[i]]=list()
  pred_klfda_Ar_H_He0[[i]]=list()
  pred_klfda_Ar_H_He1[[i]]=list()
  pred_klfda_Ar_H_He2[[i]]=list()
  pred_klfda_Ar_H_Hep[[i]]=list()
  pred_klfda_Ar_H_HeZ[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  klfda_Ar_H_He[[i]]=klfda_1(as.matrix(SSq0_q2_Shannon_scaled[-i,]),as.factor(Model)[-i],r=20,kernel=rbfdot(sigma = 0.05),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=50,metric="plain")
  pred_klfda_Ar_H_He[[i]]= predict.kfda(klfda_Ar_H_He[[i]],as.matrix(as.data.frame(SSq0_q2_Shannon_scaled)[i,]),prior = NULL)
  # pred_klfda_Ar_H_He0[[i]]=pred_klfda_Ar_H_He[[i]]$posteriors.class
  # pred_klfda_Ar_H_He1[[i]]=pred_klfda_Ar_H_He[[i]]$posteriors.class0
  pred_klfda_Ar_H_He2[[i]]=pred_klfda_Ar_H_He[[i]]$posteriors.class1
  pred_klfda_Ar_H_Hep[[i]]=pred_klfda_Ar_H_He[[i]]$bayes_jud_pred$post_class
  pred_klfda_Ar_H_HeZ[[i]]=pred_klfda_Ar_H_He[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfda_Ar_H_Hea(klfda_Ar_H_Hea[[i]],Shannon[i,])[1] 
  
  klfda_Ar_H_He_model[[i]]=Model[i]
}
#### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
class_pred_klfda_Ar_H_He2=do.call(rbind,lapply(pred_klfda_Ar_H_He2, data.frame))
class_pred_klfda_Ar_H_Hep=do.call(rbind,lapply(pred_klfda_Ar_H_Hep, data.frame))
class_pred_klfda_Ar_H_HeZ=do.call(rbind,lapply(pred_klfda_Ar_H_HeZ, data.frame))

klfda_Ar_H_He_model_class=do.call(rbind,lapply(klfda_Ar_H_He_model, data.frame))

confusionMatrix(class_pred_klfda_Ar_H_HeZ$X..i..,klfda_Ar_H_He_model_class$X..i..)


klfda_Ar_H_He_D_D=list()
n=500
klfda_Ar_H_He_D_model=list()
pred_klfda_Ar_H_He_D=list()
pred_klfda_Ar_H_He_D0=list()
pred_klfda_Ar_H_He_D1=list()
pred_klfda_Ar_H_He_D2=list()
pred_klfda_Ar_H_He_Dp=list()
pred_klfda_Ar_H_He_DZ=list()
for(i in 1:n) {
  klfda_Ar_H_He_D[[i]]=list()
  pred_klfda_Ar_H_He_D[[i]]=list()
  pred_klfda_Ar_H_He_D0[[i]]=list()
  pred_klfda_Ar_H_He_D1[[i]]=list()
  pred_klfda_Ar_H_He_D2[[i]]=list()
  pred_klfda_Ar_H_He_Dp[[i]]=list()
  pred_klfda_Ar_H_He_DZ[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  klfda_Ar_H_He_D[[i]]=klfda_1(as.matrix(SSq0_q2_Sh_D_scaled[-i,]),as.factor(Model)[-i],r=20,kernel=rbfdot(sigma = 0.05),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=50,metric="plain")
  pred_klfda_Ar_H_He_D[[i]]= predict.kfda(klfda_Ar_H_He_D[[i]],as.matrix(as.data.frame(SSq0_q2_Sh_D_scaled)[i,]),prior = NULL)
  # pred_klfda_Ar_H_He_D0[[i]]=pred_klfda_Ar_H_He_D[[i]]$posteriors.class
  # pred_klfda_Ar_H_He_D1[[i]]=pred_klfda_Ar_H_He_D[[i]]$posteriors.class0
  pred_klfda_Ar_H_He_D2[[i]]=pred_klfda_Ar_H_He_D[[i]]$posteriors.class1
  pred_klfda_Ar_H_He_Dp[[i]]=pred_klfda_Ar_H_He_D[[i]]$bayes_jud_pred$post_class
  pred_klfda_Ar_H_He_DZ[[i]]=pred_klfda_Ar_H_He_D[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfda_Ar_H_He_Da(klfda_Ar_H_He_Da[[i]],Shannon[i,])[1] 
  
  klfda_Ar_H_He_D_model[[i]]=Model[i]
}
#### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
class_pred_klfda_Ar_H_He_D2=do.call(rbind,lapply(pred_klfda_Ar_H_He_D2, data.frame))
class_pred_klfda_Ar_H_He_Dp=do.call(rbind,lapply(pred_klfda_Ar_H_He_Dp, data.frame))
class_pred_klfda_Ar_H_He_DZ=do.call(rbind,lapply(pred_klfda_Ar_H_He_DZ, data.frame))

klfda_Ar_H_He_D_model_class=do.call(rbind,lapply(klfda_Ar_H_He_D_model, data.frame))

confusionMatrix(class_pred_klfda_Ar_H_He_DZ$X..i..,klfda_Ar_H_He_D_model_class$X..i..)








kfda_umap=list()
n=500
kfda_umap_model=list()
pred_kfda_umap=list()
pred_kfda_umap0=list()
pred_kfda_umap1=list()
pred_kfda_umap2=list()
pred_kfda_umapp=list()
pred_kfda_umapZ=list()
for(i in 1:n) {
  kfda_umap[[i]]=list()
  pred_kfda_umap[[i]]=list()
  pred_kfda_umap0[[i]]=list()
  pred_kfda_umap1[[i]]=list()
  pred_kfda_umap2[[i]]=list()
  pred_kfda_umapp[[i]]=list()
  pred_kfda_umapZ[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  kfda_umap[[i]]=klfda_1(as.matrix(t11_umap$layout[-i,]),as.factor(t22)[-i],kernel=rbfdot(sigma = 1),r=10,prior=NULL,tol=1e-90,usekernel = TRUE,CV=FALSE,fL=0.25,knn=7,metric="plain")
  pred_kfda_umap[[i]]= predict.kfda(kfda_umap[[i]],as.matrix(as.data.frame(t11_umap$layout)[i,]))
  # pred_kfda0[[i]]=pred_kfda[[i]]$posteriors.class
  # pred_kfda1[[i]]=pred_kfda[[i]]$posteriors.class0
  pred_kfda_umap2[[i]]=pred_kfda_umap[[i]]$posteriors.class1
  pred_kfda_umapp[[i]]=pred_kfda_umap[[i]]$bayes_jud_pred$post_class
  pred_kfda_umapZ[[i]]=pred_kfda_umap[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.kfdaa(kfdaa[[i]],Shannon[i,])[1] 
  
  kfda_umap_model[[i]]=t22[i]
}

class_pred_kfda_umap2=do.call(rbind,lapply(pred_kfda_umap2, data.frame))
class_pred_kfda_umapp=do.call(rbind,lapply(pred_kfda_umapp, data.frame))
class_pred_kfda_umapZ=do.call(rbind,lapply(pred_kfda_umapZ, data.frame))

kfda_umap_model_class=do.call(rbind,lapply(kfda_umap_model, data.frame))

confusionMatrix(kfda_umap_model_class$X..i..,class_pred_kfda_umapZ$X..i..)


encodertest=klfda_1(as.matrix(X.output$X.output),as.factor(Model),kernel=rbfdot(sigma = 1),r=10,prior=NULL,tol=1e-90,usekernel = TRUE,CV=FALSE,fL=0.25,knn=7,metric="plain")

confusionMatrix(Model,encodertest$bayes_assigment$class)


#####

### now using randomforest 
###In random forests, there is no need for cross-validation or a separate test set toget an unbiased estimate of the test set error. It is estimated internally, during therun, as follows: 

#Each tree is constructed using a different bootstrap sample from the original data.About one-third of the cases are left out of the bootstrap sample and not used in theconstruction of the kth tree.
### because RF use of of bag to do validation we set the seed here to allow the repreductividity of this test
### Note: Scaling is done to Normalize data so that priority is not given to a particular feature. Role of Scaling is mostly important in algorithms that are distance based and require Euclidean Distance.
# Random Forest is a tree-based model and hence does not require feature scaling.
# This algorithm requires partitioning, even if you apply Normalization then also> the result would be the same.
#Any algorithm based on recursive partitioning, such as decision trees, and regression trees does not require inputs (features) to be normalized, since it is invariant to monotonic transformations of the features (just think about how the splits are done at each node). Since random forests (as well as gbm) are just a collection of trees, there is no need to normalize.


set.seed(999)
library(randomForest)
RF_SSq0=rfcv(SSq0[,-1], Model, cv.fold=10)
mean(RF_SSq0$error.cv)
RF_SSq0$predicted$`44`
TRF_SSq0<- randomForest(SSq0[,-1], Model, importance=TRUE,proximity=TRUE)
print(TRF_SSq0)
confusionMatrix(TRF_SSq0$predicted,Model)
TRF_SSq0

RF_SSq1=rfcv(SSq1[,-1], Model, cv.fold=10)
mean(RF_SSq1$error.cv)
RF_SSq1$error.cv
TRF_SSq1<- randomForest(SSq1[,-1], Model, importance=TRUE,proximity=TRUE)
print(TRF_SSq1)
confusionMatrix(TRF_SSq1$predicted,Model)
TRF_SSq1$confusion

RF_SSq2=rfcv(SSq2[,-1], Model, cv.fold=10)
mean(RF_SSq2$error.cv)
TRF_SSq2<- randomForest(SSq2[,-1], Model, importance=TRUE,proximity=TRUE)
print(TRF_SSq2)
confusionMatrix(TRF_SSq2$predicted,Model)


RF_SS_trad=rfcv(SS_trad[,-1], Model, cv.fold=10)
mean(RF_SS_trad$error.cv)

TRF_SS_trad<- randomForest(SS_trad[,-1], Model, importance=TRUE,proximity=TRUE)
confusionMatrix(TRF_SS_trad$predicted,Model)

RF_SS_all=rfcv(SSall_0[,-1], Model, cv.fold=10)
mean(RF_SS_all$error.cv)

TRF_SS_all<- randomForest(SSall_0[,-1], Model, importance=TRUE,proximity=TRUE)
confusionMatrix(TRF_SS_all$predicted,Model)
varImpPlot(TRF_SS_all)


library(caret)
library(caretEnsemble)
set.seed(999)
options(warn=-1)
library(e1071)
econtrol <- trainControl(method="LOOCV")
model_list <- train(Model ~., data=newshannon,
                    method=c("svmPoly", "nnet", "C5.0", "nb","avNNet","mxnet"),
                    preProcess=c("scale","center"),
                    trControl = econtrol)

model_svmSSq0 <- train(Model ~., data=SSq0,
                   method=c("svmPoly"),
                   preProcess=c("scale","center"),
                   trControl = econtrol)
model_svmSSq1 <- train(Model ~., data=SSq1,
                       method=c("svmPoly"),
                       preProcess=c("scale","center"),
                       trControl = econtrol)

model_svmSSq0$finalModel

model_svmSSq1$finalModel
print(model_svmSSq0)
print(model_svmSSq1)
model_svmSSq0$results$Accuracy

# Compute the confusion matrix
confusionMatrix(reference = model_svmSSq0$pred$obs, data = model_svmSSq0$pred$pred, mode='everything', positive='MM')
varimp_SSq0 <- varImp(model_svmSSq0)
plot(varimp_SSq0, main="Variable Importance with SSq0")

model_nnet_SSq0 <- train(Model ~., data=SSq0,
                    method=c("nnet"),
                    preProcess=c("scale","center"),
                    trControl = econtrol)
model_nnet_SSq1 <- train(Model ~., data=SSq1,
                         method=c("nnet"),
                         preProcess=c("scale","center"),
                         trControl = econtrol)


model_C5 <- train(Model ~., data=newshannon,
                  method=c( "C5.0"),
                  preProcess=c("scale","center"),
                  trControl = econtrol)
model_nb <- train(Model ~., data=SSq0,
                  method=c("nb"),
                  preProcess=c("scale","center"),
                  trControl = econtrol)
confusionMatrix(predict(model_dnn_SSq1,SSq1[,-1]),reference = SSq1$Model)
model_avnnet_SSq0 <- train(Model ~., data=SSq0,
                      method=c("avNNet"),
                      preProcess=c("scale","center"),
                      trControl = econtrol)

model_avnnet_SSq1 <- train(Model ~., data=SSq1,
                           method=c("avNNet"),
                           preProcess=c("scale","center"),
                           trControl = econtrol)


model_avnet_SSq2 <- train(Model ~., data=SSq2,
                          method=c("avNNet"),
                          preProcess=c("scale","center"),
                          trControl = econtrol)
model_avnet_SS_trad <- train(Model ~., data=SS_tra,
        
                            method=c("avNNet"),
                             preProcess=c("scale","center"),
                             trControl = econtrol)
model_avnet_SS_shannon <- train(Model ~., data=Shannon_all,
                                method=c("avNNet"),
                                preProcess=c("scale","center"),
                                trControl = econtrol)
model_avnet_SSq0_q2_Shannon <- train(Model ~., data=SSq0_q2_Shannon,
                                     method=c("avNNet"),
                                     preProcess=c("scale","center"),
                                     trControl = econtrol)
model_avnet_SSq0_q2_Sh_D <- train(Model ~., data=SSq0_q2_Sh_D,
                                  method=c("avNNet"),
                                  preProcess=c("scale","center"),
                                  trControl = econtrol)

model_avnet_SSq1_shannon <- train(Model ~., data=SSq1_Shannnon,
                                  method=c("avNNet"),
                                  preProcess=c("scale","center"),
                                  trControl = econtrol)
model_avnet_SSq0_q2_D <- train(Model ~., data=SSall,
                               method=c("avNNet"),
                               preProcess=c("scale","center"),
                               trControl = econtrol)




model_avnnet_SSq0

model_avnnet_SSq1



mlpWeightDecayML

library(RSNNS)
gbmGrid_mlpWeightDecayML <-  expand.grid(layer1 = c(1,5, 10,15), 
                                         layer2=c(0,5,10,15),
                                         layer3=c(0,5,10,15),
                                         decay = c(0,1e-5,1e-4,1e-3,1e-2,1e-1))

gbmGrid_model_mlpWeightDecayML_SSq0 <- train(Model ~., data=SSq0,
                                     method=c("mlpWeightDecayML"),
                                     preProcess=c("scale","center"),
                                     trControl = econtrol,tuneGrid = gbmGrid_mlpWeightDecayML)


write.csv(gbmGrid_model_mlpWeightDecayML_SSq0$results,file = "gbmGrid_model_mlpWeightDecayML_SSq0.csv") 
lapply(gbmGrid_model_mlpWeightDecayML_SSq0$finalModel, function(x) write.table(data.frame(x), 'testSSq2.csv'  , append= T, sep=',' ))
capture.output(gbmGrid_model_mlpWeightDecayML_SSq0$finalModel,file = "gbmGrid_model_mlpWeightDecayML_SSq0_finalmodel.txt")

confusionMatrix(predict(gbmGrid_model_mlpWeightDecayML_SSq0,SSq0[,-1]),reference = SSq0$Model)
date()
write.csv(gbmGrid_model_mlpWeightDecayML_SSq0$finalModel)
confusionMatrix(predict.train(gbmGrid_model_mlpWeightDecayML_SSq0),SSq0$Model) 
confusionMatrix.train(gbmGrid_model_mlpWeightDecayML_SSq0)
save.image(file="SSq1.RData")
gbmGrid_model_mlpWeightDecayML_SSq1 <- train(Model ~., data=SSq1,
                                     method=c("mlpWeightDecayML"),
                                     preProcess=c("scale","center"),
                                     trControl = econtrol,tuneGrid = gbmGrid_mlpWeightDecayML)
factor(obj.classes[max.col(posteriors1)], levels = obj.classes)

obj.classes = sort(unique(Model))
a=factor(obj.classes[max.col(gbmGrid_model_mlpWeightDecayML_SSq0$finalModel$fitted.values)],levels=obj.classes)


plotROC(target,rference)
write.csv(gbmGrid_model_mlpWeightDecayML_SSq0$pred,file = "gbGrid_mlpweightDecayML_SSq0.csv")
plotIterativeError(gbmGrid_model_mlpWeightDecayML_SSq0$finalModel)
print(gbmGrid_model_mlpWeightDecayML_SSq0$finalModel)


confusionMatrix(a,Model)
gbmGrid_model_mlpWeightDecayML_SSq2 <- train(Model ~., data=SSq2,
                                     method=c("mlpWeightDecayML"),
                                     preProcess=c("scale","center"),
                                     trControl = econtrol,tuneGrid = gbmGrid_mlpWeightDecayML)
gbmGrid_model_mlpWeightDecayML_SS_trad <- train(Model ~., data=SS_tra,
                                        method=c("mlpWeightDecayML"),
                                        preProcess=c("scale","center"),
                                        trControl = econtrol,tuneGrid = gbmGrid_mlpWeightDecayML)
gbmGrid_model_mlpWeightDecayML_SS_shannon <- train(Model ~., data=Shannon_all,
                                           method=c("mlpWeightDecayML"),
                                           preProcess=c("scale","center"),
                                           trControl = econtrol,tuneGrid = gbmGrid_mlpWeightDecayML)
gbmGrid_model_mlpWeightDecayML_SSq0_q2_Shannon <- train(Model ~., data=SSq0_q2_Shannon,
                                                method=c("mlpWeightDecayML"),
                                                preProcess=c("scale","center"),
                                                trControl = econtrol,tuneGrid = gbmGrid_mlpWeightDecayML)
gbmGrid_model_mlpWeightDecayML_SSq0_q2_Sh_D <- train(Model ~., data=SSq0_q2_Sh_D,
                                             method=c("mlpWeightDecayML"),
                                             preProcess=c("scale","center"),
                                             trControl = econtrol,tuneGrid = gbmGrid_mlpWeightDecayML)

gbmGrid_model_mlpWeightDecayML_SSq1_shannon <- train(Model ~., data=SSq1_Shannnon,
                                             method=c("mlpWeightDecayML"),
                                             preProcess=c("scale","center"),
                                             trControl = econtrol,tuneGrid = gbmGrid_mlpWeightDecayML)
gbmGrid_model_mlpWeightDecayML_SSq0_q2_D <- train(Model ~., data=SSall,
                                          method=c("mlpWeightDecayML"),
                                          preProcess=c("scale","center"),
                                          trControl = econtrol,tuneGrid = gbmGrid_mlpWeightDecayML)

#X00=expression("1"^D)  
#expression('Mean annual Q,  m'^"3"*' s'^"-1")
#expression('z'['0m']*', m')
resamps <- resamples(list(Neural_network_Ar = gbmGrid_model_mlpWeightDecayML_SSq0,
                          Neural_network_1D = gbmGrid_model_mlpWeightDecayML_SSq1,
                          Neural_network_He = gbmGrid_model_mlpWeightDecayML_SSq2,
                          Neural_network_H = gbmGrid_model_mlpWeightDecayML_SS_shannon,
                          Neural_network_Ar_He = gbmGrid_model_mlpWeightDecayML_SS_trad,
                          Neural_network_H_D = gbmGrid_model_mlpWeightDecayML_SSq1_shannon,
                          Neural_network_Ar_H_He = gbmGrid_model_mlpWeightDecayML_SSq0_q2_Shannon,
                          Neural_network_Ar_1D_He = gbmGrid_model_mlpWeightDecayML_SSq0_q2_D,
                          Neural_network_Ar_H_He_1D = gbmGrid_model_mlpWeightDecayML_SSq0_q2_Sh_D))
resamps

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))
#### oirginal without extending the parameter _ tuneGrid
model_mlpWeightDecayML_SSq0 <- train(Model ~., data=SSq0,
                           method=c("mlpWeightDecayML"),
                           preProcess=c("scale","center"),
                           trControl = econtrol)


model_mlpWeightDecayML_SSq1 <- train(Model ~., data=SSq1,
                           method=c("mlpWeightDecayML"),
                           preProcess=c("scale","center"),
                           trControl = econtrol)


model_mlpWeightDecayML_SSq2 <- train(Model ~., data=SSq2,
                          method=c("mlpWeightDecayML"),
                          preProcess=c("scale","center"),
                          trControl = econtrol)
model_mlpWeightDecayML_SS_trad <- train(Model ~., data=SS_tra,
                             
                             method=c("mlpWeightDecayML"),
                             preProcess=c("scale","center"),
                             trControl = econtrol)
model_mlpWeightDecayML_SS_shannon <- train(Model ~., data=Shannon_all,
                                method=c("mlpWeightDecayML"),
                                preProcess=c("scale","center"),
                                trControl = econtrol)
model_mlpWeightDecayML_SSq0_q2_Shannon <- train(Model ~., data=SSq0_q2_Shannon,
                                     method=c("mlpWeightDecayML"),
                                     preProcess=c("scale","center"),
                                     trControl = econtrol)
model_mlpWeightDecayML_SSq0_q2_Sh_D <- train(Model ~., data=SSq0_q2_Sh_D,
                                  method=c("mlpWeightDecayML"),
                                  preProcess=c("scale","center"),
                                  trControl = econtrol)

model_mlpWeightDecayML_SSq1_shannon <- train(Model ~., data=SSq1_Shannnon,
                                  method=c("mlpWeightDecayML"),
                                  preProcess=c("scale","center"),
                                  trControl = econtrol)
model_mlpWeightDecayML_SSq0_q2_D <- train(Model ~., data=SSall,
                               method=c("mlpWeightDecayML"),
                               preProcess=c("scale","center"),
                               trControl = econtrol)



model_mlpWeightDecayML_SSq0_q2_D$finalModel$fitted.values
mlpKerasDropoutCost

model_mlpKerasDropoutCost_SSq0 <- train(as.matrix(SSq0[,-1]),as.numeric(Model), 
                                     method=c("mlpKerasDropoutCost"),
                                     preProcess=c("scale","center"),
                                     trControl = econtrol)

model_mlpKerasDropoutCost_SSq1 <- train(Model ~., data=SSq1,
                                     method=c("mlpKerasDropoutCost"),
                                     preProcess=c("scale","center"),
                                     trControl = econtrol)


model_mlpKerasDropoutCost_SSq2 <- train(Model ~., data=SSq2,
                                     method=c("mlpKerasDropoutCost"),
                                     preProcess=c("scale","center"),
                                     trControl = econtrol)
model_mlpKerasDropoutCost_SS_trad <- train(Model ~., data=SS_tra,
                                        
                                        method=c("mlpKerasDropoutCost"),
                                        preProcess=c("scale","center"),
                                        trControl = econtrol)
model_mlpKerasDropoutCost_SS_shannon <- train(Model ~., data=Shannon_all,
                                           method=c("mlpKerasDropoutCost"),
                                           preProcess=c("scale","center"),
                                           trControl = econtrol)
model_mlpKerasDropoutCost_SSq0_q2_Shannon <- train(Model ~., data=SSq0_q2_Shannon,
                                                method=c("mlpKerasDropoutCost"),
                                                preProcess=c("scale","center"),
                                                trControl = econtrol)
model_mlpKerasDropoutCost_SSq0_q2_Sh_D <- train(Model ~., data=SSq0_q2_Sh_D,
                                             method=c("mlpKerasDropoutCost"),
                                             preProcess=c("scale","center"),
                                             trControl = econtrol)

model_mlpKerasDropoutCost_SSq1_shannon <- train(Model ~., data=SSq1_Shannnon,
                                             method=c("mlpKerasDropoutCost"),
                                             preProcess=c("scale","center"),
                                             trControl = econtrol)
model_mlpKerasDropoutCost_SSq0_q2_D <- train(Model ~., data=SSall,
                                          method=c("mlpKerasDropoutCost"),
                                          preProcess=c("scale","center"),
                                          trControl = econtrol)


mx.set.seed(9)
gbmGrid_mxnet <-  expand.grid(layer1 = c(1,5, 10), 
                        layer2=c(0,5,10),
                        layer3=c(0,5,10),
                        dropout = c(0,0.35,0.7),
                        learning.rate=2e-05,
                          momentum=0.9, 
                        activation=c("relu","tanh"))
model_mxnet_SSq0 <- train(Model ~., data=SSq0,
                     method=c("mxnetAdam"),
                     preProcess=c("scale","center"),
                     trControl = econtrol, tuneGrid = gbmGrid_mxnet)
model_mxnet_SSq1 <- train(Model ~., data=SSq1,
                          method=c("mxnet"),
                          preProcess=c("scale","center"),
                          trControl = econtrol,tuneGrid = gbmGrid_mxnet)
write.csv(model_mxnet_SSq1$results,file = "model_mxnet_SSq1_tuning_results.csv")
detach("package:keras", unload=TRUE)
unloadNamespace("tensorflow")
plot(model_mxnet_SSq1)


model_mxnet_SSq2 <- train(Model ~., data=SSq2,
                          method=c("mxnet"),
                          preProcess=c("scale","center"),
                          trControl = econtrol)
model_mxnet_SS_trad <- train(Model ~., data=SS_tra,
                          method=c("mxnet"),
                          preProcess=c("scale","center"),
                          trControl = econtrol)
model_mxnet_SS_shannon <- train(Model ~., data=Shannon_all,
                          method=c("mxnet"),
                          preProcess=c("scale","center"),
                          trControl = econtrol)
model_mxnet_SSq0_q2_Shannon <- train(Model ~., data=SSq0_q2_Shannon,
                          method=c("mxnet"),
                          preProcess=c("scale","center"),
                          trControl = econtrol)
model_mxnet_SSq0_q2_Sh_D <- train(Model ~., data=SSq0_q2_Sh_D,
                          method=c("mxnet"),
                          preProcess=c("scale","center"),
                          trControl = econtrol)

model_mxnet_SSq1_shannon <- train(Model ~., data=SSq1_Shannnon,
                          method=c("mxnet"),
                          preProcess=c("scale","center"),
                          trControl = econtrol)
model_mxnet_SSq0_q2_D <- train(Model ~., data=SSall,
                                  method=c("mxnet"),
                                  preProcess=c("scale","center"),
                                  trControl = econtrol)


model_mxnet_SSq0
model_mxnet_SSq1

model_dnn_SSq0 <- train(Model ~., data=SSq0,
                   method=c("dnn"),
                   preProcess=c("scale","center"),
                   trControl = econtrol)
write.csv(model_dnn$results,file ="Deep_eural_auto_encoder_SSq0_tuning_results.csv") 
model_dnn_SSq1 <- train(Model ~., data=SSq1,
                   method=c("dnn"),preProcess=c("scale","center"),
                   trControl = econtrol)

write.csv(model_dnn_SSq1$results,file = "Deep_neural_auto_encoder_SSq1_tuning_results.csv")
#preProcess=c("scale","center"),
print(model_dnn)

write.csv(model_dnn$results,file="SSq0_dnn_autoencoder.csv")


tunegrid <- expand.grid(.mtry = (2:44)) 
model_RF_SSq0 <- train(Model ~., data=SSq0,
                  method=c("rf"),
                  trControl = econtrol,tuneGrid = tunegrid)

model_RF_SSq1<- train(Model ~., data=SSq1,
                  method=c("rf"),
                  trControl = econtrol)

model_RF_SS_all<- train(Model ~., data=SSq0_q2_Sh_D,
                      method=c("rf"),
                      trControl = econtrol)


model_RF_SSq0
model_RF_SSq1$finalModel$confusion

print(model_RF_SSq1)

##### do feature selection using recursive feature elimination (rfe)? It can be implemented using the rfe() function and you have the flexibility to control what algorithm rfe uses and how it cross validates by defining the rfeControl().
set.seed(999)
options(warn=-1)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=trainData[, 1:18], y=trainData$Purchase,
                 sizes = subsets,
                 rfeControl = ctrl)



#e layer1 = 2, layer2 = 0, layer3 = 0, hidden_dropout = 0 and visible_dropout = 0.35
library(deepnet)
nue_model=as.numeric(Model)

sae_dnn_shannon=sae.dnn.train(as.matrix(Shannon), as.vector(nue_model), hidden = c(3), activationfun = "sigm", learningrate = 0.8,
                              momentum = 0.5, learningrate_scale = 1, output = "sigm", sae_output = "linear",
                              numepochs = 3, batchsize = 100, hidden_dropout = 0, visible_dropout = 0.35)
nntrain_sh=nn.train(as.matrix(Shannon),as.vector(nue_model),hidden =c(3))

nntest_sae=nn.test(sae_dnn_shannon,as.matrix(Shannon),as.vector(nue_model))
nntest_=nn.test(nntrain_sh,as.matrix(Shannon[1,]),as.vector(nue_model))

str(sae_dnn_shannon$output)

print(model_svm)
print(model_C5)
print(model_nb)
print(model_nnet)
print(model_avnnet)
print(model_mxnet)
print(model_dnn) 

results <- resamples(model_list)
