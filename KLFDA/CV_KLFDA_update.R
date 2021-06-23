


library(lfda)
library(kernlab)
library(MASS)
library(caret)
load("KLFDA_updated_KernelMatrix.RData")
print("CV Ar")
date()
## SSq0

#klfdaq0=list()
#n=500
#klfdaq0_model=list()
#pred_klfdaq0=list()

#pred_klfdaq02=list()
#pred_klfdaq0p=list()
#pred_klfdaq0Z=list()
#for(i in 1:n) {
#  klfdaq0[[i]]=list()
#  pred_klfdaq0[[i]]=list()
  
#  pred_klfdaq02[[i]]=list()
#  pred_klfdaq0p[[i]]=list()
#  pred_klfdaq0Z[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
#  klfdaq0=klfda_1(as.matrix(SSq0_scaled[-i,]),Model[-i],r=20,prior=NULL,knn=10,kernel=rbfdot(sigma = 0.5),tol=1e-30,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
#  pred_klfdaq0[[i]]= predict.kfda(klfdaq0,as.matrix(as.data.frame(SSq0_scaled)[i,]), prior = NULL)
  # pred_klfdaq00[[i]]=pred_klfdaq0[[i]]$posteriors.class
  # pred_klfdaq01[[i]]=pred_klfdaq0[[i]]$posteriors.class0
#  pred_klfdaq02[[i]]=pred_klfdaq0[[i]]$posteriors.class1
#  pred_klfdaq0p[[i]]=pred_klfdaq0[[i]]$bayes_jud_pred$post_class
#  pred_klfdaq0Z[[i]]=pred_klfdaq0[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfdaq0a(klfdaq0a[[i]],Shannon[i,])[1] 
 # 
 # klfdaq0_model[[i]]=Model[i]
#}

#class_pred_klfdaq02=do.call(rbind,lapply(pred_klfdaq02, data.frame))
#class_pred_klfdaq0p=do.call(rbind,lapply(pred_klfdaq0p, data.frame))
#class_pred_klfdaq0Z=do.call(rbind,lapply(pred_klfdaq0Z, data.frame))

#klfdaq0_model_class=do.call(rbind,lapply(klfdaq0_model, data.frame))

#capture.output(confusionMatrix(class_pred_klfdaq0Z$X..i..,klfdaq0_model_class$X..i..),file = "Confusionmatrix_Ar_scaled_knn_10_sigma0.5_klfda_VC.txt")
#print("finished Ar knn 10, cv Ar knn28")
#date()
##### not scaled doesn not work

#Sklfdaq0=list()
#n=500
#Sklfdaq0_model=list()
#Spred_klfdaq0=list()

#Spred_klfdaq02=list()
#Spred_klfdaq0p=list()
#Spred_klfdaq0Z=list()
#for(i in 1:n) {
#  Sklfdaq0[[i]]=list()
#  Spred_klfdaq0[[i]]=list()
  
#  Spred_klfdaq02[[i]]=list()
#  Spred_klfdaq0p[[i]]=list()
#  Spred_klfdaq0Z[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
#  Sklfdaq0=klfda_1(as.matrix(SSq0_scaled[-i,]),Model[-i],r=20,prior=NULL,knn=28,kernel=rbfdot(sigma = 0.5),tol=1e-30,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
#  Spred_klfdaq0[[i]]= predict.kfda(Sklfdaq0,as.matrix(as.data.frame(SSq0_scaled)[i,]), prior = NULL)
  # pred_klfdaq00[[i]]=pred_klfdaq0[[i]]$posteriors.class
  # pred_klfdaq01[[i]]=pred_klfdaq0[[i]]$posteriors.class0
#  Spred_klfdaq02[[i]]=Spred_klfdaq0[[i]]$posteriors.class1
#  Spred_klfdaq0p[[i]]=Spred_klfdaq0[[i]]$bayes_jud_pred$post_class
#  Spred_klfdaq0Z[[i]]=Spred_klfdaq0[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfdaq0a(klfdaq0a[[i]],Shannon[i,])[1] 
  
#  Sklfdaq0_model[[i]]=Model[i]
#}

#Sclass_pred_klfdaq02=do.call(rbind,lapply(Spred_klfdaq02, data.frame))
#Sclass_pred_klfdaq0p=do.call(rbind,lapply(Spred_klfdaq0p, data.frame))
#Sclass_pred_klfdaq0Z=do.call(rbind,lapply(Spred_klfdaq0Z, data.frame))

#Sklfdaq0_model_class=do.call(rbind,lapply(Sklfdaq0_model, data.frame))


#capture.output(confusionMatrix(Sclass_pred_klfdaq0Z$X..i..,Sklfdaq0_model_class$X..i..),file = "Confusionmatrix_Ar_scaled_knn_28_sigma_0.5_klfda_VC.txt")

klfdaq0=list()
n=500
klfdaq0_model=list()
pred_klfdaq0=list()
pred_klfdaq0Z=list()
for(i in 1:n) {
  klfdaq0[[i]]=list()
  pred_klfdaq0[[i]]=list()
  
   pred_klfdaq0Z[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  klfdaq0=klfda_1(as.matrix(SSq0_scaled[-i,]),Model[-i],r=20,prior=NULL,knn=50,kernel=rbfdot(sigma = 0.05),tol=1e-30,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  pred_klfdaq0[[i]]= predict.kfda(klfdaq0,as.matrix(as.data.frame(SSq0_scaled)[i,]), prior = NULL)
  # pred_klfdaq00[[i]]=pred_klfdaq0[[i]]$posteriors.class
  # pred_klfdaq01[[i]]=pred_klfdaq0[[i]]$posteriors.class0
 pred_klfdaq0Z[[i]]=pred_klfdaq0[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfdaq0a(klfdaq0a[[i]],Shannon[i,])[1] 
  
  klfdaq0_model[[i]]=Model[i]
}


class_pred_klfdaq0Z=do.call(rbind,lapply(pred_klfdaq0Z, data.frame))

klfdaq0_model_class=do.call(rbind,lapply(klfdaq0_model, data.frame))

capture.output(confusionMatrix(class_pred_klfdaq0Z$X..i..,klfdaq0_model_class$X..i..),file = "Confusionmatrix_Ar_scaled_knn_50_sigma0.05_klfda_VC.txt")
print("finished Ar knn 50 sigma0.05, cv Ar 0.1")
date()
##### not scaled doesn not work
save.image(file = "CV_KLFDA.RData")
Sklfdaq0=list()
n=500
Sklfdaq0_model=list()
Spred_klfdaq0=list()
Spred_klfdaq0Z=list()
for(i in 1:n) {
  Sklfdaq0[[i]]=list()
  Spred_klfdaq0[[i]]=list()
  
  
  Spred_klfdaq0Z[[i]]=list()
  #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
  
  Sklfdaq0=klfda_1(as.matrix(SSq0_scaled[-i,]),Model[-i],r=20,prior=NULL,knn=50,kernel=rbfdot(sigma = 0.1),tol=1e-30,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
  Spred_klfdaq0[[i]]= predict.kfda(Sklfdaq0,as.matrix(as.data.frame(SSq0_scaled)[i,]), prior = NULL)
  # pred_klfdaq00[[i]]=pred_klfdaq0[[i]]$posteriors.class
  # pred_klfdaq01[[i]]=pred_klfdaq0[[i]]$posteriors.class0
  Spred_klfdaq0Z[[i]]=Spred_klfdaq0[[i]]$bayes_assig_pred$class
  # predict1 = function(i) predict.klfdaq0a(klfdaq0a[[i]],Shannon[i,])[1] 
  
  Sklfdaq0_model[[i]]=Model[i]
}


Sclass_pred_klfdaq0Z=do.call(rbind,lapply(Spred_klfdaq0Z, data.frame))

Sklfdaq0_model_class=do.call(rbind,lapply(Sklfdaq0_model, data.frame))


capture.output(confusionMatrix(Sclass_pred_klfdaq0Z$X..i..,Sklfdaq0_model_class$X..i..),file = "Confusionmatrix_Ar_scaled_knn_50_sigma_0.5_klfda_VC.txt")

print("finished Ar cv D1  scaled")
save.image(file = "CV_KLFDA.RData")
date()



date()
##SSq1

#lapply(a0, function(x) write.table(data.frame(x), 'confusionmatrixklfdaSSq1.csv'  , append= T, sep=',' ))
       ### this a little change for data
       Sklfdaq1=list()
       m=500
       Sklfdaq1_model=list()
       Spred_klfdaq1=list()
       Spred_klfdaq10=list()
       Spred_klfdaq11=list()
     
       Spred_klfdaq1Z=list()
       for(i in 1:m) {
         Sklfdaq1[[i]]=list()
         Spred_klfdaq1[[i]]=list()
         Spred_klfdaq10[[i]]=list()
         Spred_klfdaq11[[i]]=list()

         Spred_klfdaq1Z[[i]]=list()
         #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
         
         Sklfdaq1=klfda_1(as.matrix(SSq1_scaled[-i,]),Model[-i],r=20,kernel=rbfdot(sigma = 0.1),knn=45,tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
         Spred_klfdaq1[[i]]= predict.kfda(Sklfdaq1,as.matrix(as.data.frame(SSq1_scaled)[i,]),prior = NULL)
         # pred_klfdaq10[[i]]=pred_klfdaq1[[i]]$posteriors.class
         # pred_klfdaq11[[i]]=pred_klfdaq1[[i]]$posteriors.class0
         
         Spred_klfdaq1Z[[i]]=Spred_klfdaq1[[i]]$bayes_assig_pred$class
         # predict1 = function(i) predict.klfdaq1a(klfdaq1a[[i]],Shannon[i,])[1] 
         
         Sklfdaq1_model[[i]]=Model[i]
       }
       #### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
     
       Sclass_pred_klfdaq1Z=do.call(rbind,lapply(Spred_klfdaq1Z, data.frame))
       
       Sklfdaq1_model_class=do.call(rbind,lapply(Sklfdaq1_model, data.frame))
       
       capture.output(confusionMatrix(Sclass_pred_klfdaq1Z$X..i..,Sklfdaq1_model_class$X..i..),file = "Confusionmatrix_1D_scaledknn45_sigma0.1_klfda_VC.txt")
       print("finished D sigma0.1 cv knn 55")
       save.image(file = "CV_KLFDA.RData")
       date()
       Sklfdaq1=list()
       m=500
       Sklfdaq1_model=list()
       Spred_klfdaq1=list()
       Spred_klfdaq10=list()
       Spred_klfdaq11=list()
       Spred_klfdaq1Z=list()
       for(i in 1:m) {
         Sklfdaq1[[i]]=list()
         Spred_klfdaq1[[i]]=list()
         Spred_klfdaq10[[i]]=list()
         Spred_klfdaq11[[i]]=list()
         Spred_klfdaq1Z[[i]]=list()
         #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
         
         Sklfdaq1=klfda_1(as.matrix(SSq1_scaled[-i,]),Model[-i],r=20,kernel=rbfdot(sigma = 0.05),knn=55,tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
         Spred_klfdaq1[[i]]= predict.kfda(Sklfdaq1,as.matrix(as.data.frame(SSq1_scaled)[i,]),prior = NULL)

         Spred_klfdaq1Z[[i]]=Spred_klfdaq1[[i]]$bayes_assig_pred$class
         # predict1 = function(i) predict.klfdaq1a(klfdaq1a[[i]],Shannon[i,])[1] 
         
         Sklfdaq1_model[[i]]=Model[i]
       }
       #### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
 
       Sclass_pred_klfdaq1Z=do.call(rbind,lapply(Spred_klfdaq1Z, data.frame))
       
       Sklfdaq1_model_class=do.call(rbind,lapply(Sklfdaq1_model, data.frame))
       
       capture.output(confusionMatrix(Sclass_pred_klfdaq1Z$X..i..,Sklfdaq1_model_class$X..i..),file = "Confusionmatrix_1D_sigma0.05scaled_knn_55_klfda_VC.txt")
       
       print("finished D1 knn 55 cv sigma0.05 knn 35")
       save.image(file = "CV_KLFDA.RData")
       date()
       Sklfdaq1=list()
       m=500
       Sklfdaq1_model=list()
       Spred_klfdaq1=list()
       Spred_klfdaq10=list()
       Spred_klfdaq11=list()
     
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
         
         Sklfdaq1=klfda_1(as.matrix(SSq1_scaled[-i,]),Model[-i],r=20,kernel=rbfdot(sigma = 0.05),knn=35,tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
         Spred_klfdaq1[[i]]= predict.kfda(Sklfdaq1,as.matrix(as.data.frame(SSq1_scaled)[i,]),prior = NULL)
         # pred_klfdaq10[[i]]=pred_klfdaq1[[i]]$posteriors.class
         # pred_klfdaq11[[i]]=pred_klfdaq1[[i]]$posteriors.class0
      
         Spred_klfdaq1Z[[i]]=Spred_klfdaq1[[i]]$bayes_assig_pred$class
         # predict1 = function(i) predict.klfdaq1a(klfdaq1a[[i]],Shannon[i,])[1] 
         
         Sklfdaq1_model[[i]]=Model[i]
       }
       #### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
       
       Sclass_pred_klfdaq1Z=do.call(rbind,lapply(Spred_klfdaq1Z, data.frame))
       
       Sklfdaq1_model_class=do.call(rbind,lapply(Sklfdaq1_model, data.frame))
       
       capture.output(confusionMatrix(Sclass_pred_klfdaq1Z$X..i..,Sklfdaq1_model_class$X..i..),file = "Confusionmatrix_1D_scaledsigma0.05_knn_35_klfda_VC.txt")
       print("finished D knn 35 Cv knn 22")
       save.image(file = "CV_KLFDA.RData")
       date()
       Sklfdaq1=list()
       m=500
       Sklfdaq1_model=list()
       Spred_klfdaq1=list()
       Spred_klfdaq10=list()
       Spred_klfdaq11=list()
       Spred_klfdaq1Z=list()
       for(i in 1:m) {
         Sklfdaq1[[i]]=list()
         Spred_klfdaq1[[i]]=list()
         Spred_klfdaq10[[i]]=list()
  
         Spred_klfdaq1p[[i]]=list()
         Spred_klfdaq1Z[[i]]=list()
         #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
         
         Sklfdaq1=klfda_1(as.matrix(SSq1_scaled[-i,]),Model[-i],r=20,kernel=rbfdot(sigma = 0.1),knn=22,tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
         Spred_klfdaq1[[i]]= predict.kfda(Sklfdaq1,as.matrix(as.data.frame(SSq1_scaled)[i,]),prior = NULL)
         # pred_klfdaq10[[i]]=pred_klfdaq1[[i]]$posteriors.class
         Spred_klfdaq1Z[[i]]=Spred_klfdaq1[[i]]$bayes_assig_pred$class
         # predict1 = function(i) predict.klfdaq1a(klfdaq1a[[i]],Shannon[i,])[1] 
         
         Sklfdaq1_model[[i]]=Model[i]
       }
       #### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
    
       Sclass_pred_klfdaq1Z=do.call(rbind,lapply(Spred_klfdaq1Z, data.frame))
       
       Sklfdaq1_model_class=do.call(rbind,lapply(Sklfdaq1_model, data.frame))
       
       capture.output(confusionMatrix(Sclass_pred_klfdaq1Z$X..i..,Sklfdaq1_model_class$X..i..),file = "Confusionmatrix_1D_scaled_knnsigma0.1_22_klfda_VC.txt")
    
       print("finished D1 knn 22, CV D1 Knn 66")
       save.image(file = "CV_KLFDA.RData")
       date()
       ##knn=76   
       Sklfdaq1_76=list()
       m=500
       Sklfdaq1_76_model=list()
       Spred_klfdaq1_76=list()
       Spred_klfdaq1_760=list()
       Spred_klfdaq1_761=list()
     
       Spred_klfdaq1_76Z=list()
       for(i in 1:m) {
         Sklfdaq1_76[[i]]=list()
         Spred_klfdaq1_76[[i]]=list()
         Spred_klfdaq1_760[[i]]=list()
         Spred_klfdaq1_761[[i]]=list()
      
         Spred_klfdaq1_76Z[[i]]=list()
         #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
         
         Sklfdaq1_76=klfda_1(as.matrix(SSq1_scaled[-i,]),Model[-i],r=20,kernel=rbfdot(sigma = 0.1),knn=66,tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
         Spred_klfdaq1_76[[i]]= predict.kfda(Sklfdaq1_76,as.matrix(as.data.frame(SSq1_scaled)[i,]),prior = NULL)
         # pred_klfdaq1_760[[i]]=pred_klfdaq1_76[[i]]$posteriors.class
         # pred_klfdaq1_761[[i]]=pred_klfdaq1_76[[i]]$posteriors.class0
      
         Spred_klfdaq1_76Z[[i]]=Spred_klfdaq1_76[[i]]$bayes_assig_pred$class
         # predict1 = function(i) predict.klfdaq1_76a(klfdaq1_76a[[i]],Shannon[i,])[1] 
         
         Sklfdaq1_76_model[[i]]=Model[i]
       }
       #### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
      
       Sclass_pred_klfdaq1_76Z=do.call(rbind,lapply(Spred_klfdaq1_76Z, data.frame))
       
       Sklfdaq1_76_model_class=do.call(rbind,lapply(Sklfdaq1_76_model, data.frame))
       
       capture.output(confusionMatrix(Sclass_pred_klfdaq1_76Z$X..i..,Sklfdaq1_76_model_class$X..i..),file = "Confusionmatrix_1D_sigma0.1scaled_knn_66_klfda_VC.txt")
      
        print("finished D1 cv He")
        save.image(file = "CV_KLFDA.RData")
         date()
       
       
         ##SSq2
       
       klfdaq2=list()
       n=500
       klfdaq2_model=list()
       pred_klfdaq2=list()
       pred_klfdaq20=list()
       pred_klfdaq21=list()
       pred_klfdaq2Z=list()
       for(i in 1:n) {
         klfdaq2[[i]]=list()
         pred_klfdaq2[[i]]=list()
         pred_klfdaq20[[i]]=list()
         pred_klfdaq21[[i]]=list()
         pred_klfdaq2Z[[i]]=list()
         #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
         
         klfdaq2[[i]]=klfda_1(as.matrix(SSq2_scaled[-i,]),as.factor(Model)[-i],r=20,kernel=rbfdot(sigma = 0.1),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=68,metric="plain")
         pred_klfdaq2[[i]]= predict.kfda(klfdaq2[[i]],as.matrix(as.data.frame(SSq2_scaled)[i,]),prior = NULL)
         # pred_klfdaq20[[i]]=pred_klfdaq2[[i]]$posteriors.class
         # pred_klfdaq21[[i]]=pred_klfdaq2[[i]]$posteriors.class0

         pred_klfdaq2Z[[i]]=pred_klfdaq2[[i]]$bayes_assig_pred$class
         # predict1 = function(i) predict.klfdaq2a(klfdaq2a[[i]],Shannon[i,])[1] 
         
         klfdaq2_model[[i]]=Model[i]
       }
  
       class_pred_klfdaq2Z=do.call(rbind,lapply(pred_klfdaq2Z, data.frame))
       
       klfdaq2_model_class=do.call(rbind,lapply(klfdaq2_model, data.frame))
       
       #confusionMatrix(class_pred_klfdaq2Z$X..i..,klfdaq2_model_class$X..i..)
       capture.output(confusionMatrix(class_pred_klfdaq2Z$X..i..,klfdaq2_model_class$X..i..),file = "Confusionmatrix_He_scaled_aigma0.1knn68_klfda_VC.txt")
       
       print("finished q2 Cv SS Shannon")
       save.image(file = "CV_KLFDA.RData")
       date()
       ##
       
       ##SS_shannon
       
       klfdash=list()
       n=500
       klfdash_model=list()
       pred_klfdash=list()
       pred_klfdash0=list()
       pred_klfdash1=list()
    
       pred_klfdashZ=list()
       for(i in 1:n) {
         klfdash[[i]]=list()
         pred_klfdash[[i]]=list()
         pred_klfdash0[[i]]=list()
         pred_klfdash1[[i]]=list()
       
         pred_klfdashZ[[i]]=list()
         #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
         
         klfdash[[i]]=klfda_1(as.matrix(Shannon_all_scaled[-i,]),as.factor(Model)[-i],r=20,kernel=rbfdot(sigma = 0.05),knn=51,tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,metric="plain")
         pred_klfdash[[i]]= predict.kfda(klfdash[[i]],as.matrix(as.data.frame(Shannon_all_scaled)[i,]),prior = NULL)
         # pred_klfdash0[[i]]=pred_klfdash[[i]]$posteriors.class
         # pred_klfdash1[[i]]=pred_klfdash[[i]]$posteriors.class0
       
         pred_klfdashZ[[i]]=pred_klfdash[[i]]$bayes_assig_pred$class
         # predict1 = function(i) predict.klfdasha(klfdasha[[i]],Shannon[i,])[1] 
         
         klfdash_model[[i]]=Model[i]
       }
       #### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
     
       class_pred_klfdashZ=do.call(rbind,lapply(pred_klfdashZ, data.frame))
       
       klfdash_model_class=do.call(rbind,lapply(klfdash_model, data.frame))
       
       #confusionMatrix(class_pred_klfdashZ$X..i..,klfdash_model_class$X..i..)
       capture.output(confusionMatrix(class_pred_klfdashZ$X..i..,klfdash_model_class$X..i..),file = "Confusionmatrix_Shannon_scaledsigma0.05_knn51_klfda_VC.txt")
       print("finished shannon cv SS_trad")
       save.image(file = "CV_KLFDA.RData")
        date()
       #####SS_trad
       
       klfda_trad=list()
       m=500
       klfda_trad_model=list()
       pred_klfda_trad=list()
 
     
       pred_klfda_tradZ=list()
       for(i in 1:m) {
         klfda_trad[[i]]=list()
         pred_klfda_trad[[i]]=list()
  
      
         pred_klfda_tradZ[[i]]=list()
         #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
         
         klfda_trad[[i]]=klfda_1(as.matrix(SS_trad_scaled[-i,]),Model[-i],r=20,kernel=rbfdot(sigma = 0.05),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=50,metric="plain")
         pred_klfda_trad[[i]]= predict.kfda(klfda_trad[[i]],as.matrix(as.data.frame(SS_trad_scaled)[i,]),prior = NULL)
        
       
         pred_klfda_tradZ[[i]]=pred_klfda_trad[[i]]$bayes_assig_pred$class
         # predict1 = function(i) predict.klfda_trada(klfda_trada[[i]],Shannon[i,])[1] 
         
         klfda_trad_model[[i]]=Model[i]
       }
       #### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
    
       class_pred_klfda_tradZ=do.call(rbind,lapply(pred_klfda_tradZ, data.frame))
       
       klfda_trad_model_class=do.call(rbind,lapply(klfda_trad_model, data.frame))
       
       #confusionMatrix(class_pred_klfda_trad2$X..i..,klfda_trad_model_class$X..i..)
       capture.output( confusionMatrix(class_pred_klfda_tradZ$X..i..,klfda_trad_model_class$X..i..),file = "Confusionmatrix_SS_q0_q2_trad_scaled_sogma0.05_knn50_klfda_VC.txt")
       print("finished q0_q1, cv q0_q2_1D")
       save.image(file = "CV_KLFDA.RData")
       date()
       ## SS_q0_q2_1D
       
       klfda_q0_1D_q2=list()
       n=500
       klfda_q0_1D_q2_model=list()
       pred_klfda_q0_1D_q2=list()
      
      
       pred_klfda_q0_1D_q2Z=list()
       for(i in 1:n) {
         klfda_q0_1D_q2[[i]]=list()
         pred_klfda_q0_1D_q2[[i]]=list()
        
       
         pred_klfda_q0_1D_q2Z[[i]]=list()
         #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
         
         klfda_q0_1D_q2[[i]]=klfda_1(as.matrix(SSall_scaled[-i,]),as.factor(Model)[-i],r=20,kernel=rbfdot(sigma = 0.05),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=50,metric="plain")
         pred_klfda_q0_1D_q2[[i]]= predict.kfda(klfda_q0_1D_q2[[i]],as.matrix(as.data.frame(SSall_scaled)[i,]),prior = NULL)
        
        
         pred_klfda_q0_1D_q2Z[[i]]=pred_klfda_q0_1D_q2[[i]]$bayes_assig_pred$class
         # predict1 = function(i) predict.klfda_q0_1D_q2a(klfda_q0_1D_q2a[[i]],Shannon[i,])[1] 
         
         klfda_q0_1D_q2_model[[i]]=Model[i]
       }
       #### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
    
       class_pred_klfda_q0_1D_q2Z=do.call(rbind,lapply(pred_klfda_q0_1D_q2Z, data.frame))
       
       klfda_q0_1D_q2_model_class=do.call(rbind,lapply(klfda_q0_1D_q2_model, data.frame))
       
      # confusionMatrix(class_pred_klfda_q0_1D_q2Z$X..i..,klfda_q0_1D_q2_model_class$X..i..)
       capture.output(confusionMatrix(class_pred_klfda_q0_1D_q2Z$X..i..,klfda_q0_1D_q2_model_class$X..i..),file = "Confusionmatrix_SS_q0_q2_1D_scaledsigma0.05_knn_50_klfda_VC.txt")
      
       print("finished q0_q2_1D cv q0_q2_Shannon")
       save.image(file = "CV_KLFDA.RData")
       date()
       ### q0_q2_Shannon
       
       klfda_q0_q2_Shannon=list()
       n=500
       klfda_q0_q2_Shannon_model=list()
       pred_klfda_q0_q2_Shannon=list()
     
     
       pred_klfda_q0_q2_ShannonZ=list()
       for(i in 1:n) {
         klfda_q0_q2_Shannon[[i]]=list()
         pred_klfda_q0_q2_Shannon[[i]]=list()
      

         pred_klfda_q0_q2_ShannonZ[[i]]=list()
         #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
         
         klfda_q0_q2_Shannon[[i]]=klfda_1(as.matrix(SSq0_q2_Shannon_scaled[-i,]),as.factor(Model)[-i],r=20,kernel=rbfdot(sigma = 0.01),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=50,metric="plain")
         pred_klfda_q0_q2_Shannon[[i]]= predict.kfda(klfda_q0_q2_Shannon[[i]],as.matrix(as.data.frame(SSq0_q2_Shannon_scaled)[i,]),prior = NULL)
        
         pred_klfda_q0_q2_ShannonZ[[i]]=pred_klfda_q0_q2_Shannon[[i]]$bayes_assig_pred$class
         
         
         klfda_q0_q2_Shannon_model[[i]]=Model[i]
       }
       #### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data

       class_pred_klfda_q0_q2_ShannonZ=do.call(rbind,lapply(pred_klfda_q0_q2_ShannonZ, data.frame))
       
       klfda_q0_q2_Shannon_model_class=do.call(rbind,lapply(klfda_q0_q2_Shannon_model, data.frame))
       
       # confusionMatrix(class_pred_klfda_q0_q2_ShannonZ$X..i..,klfda_q0_q2_Shannon_model_class$X..i..)
       capture.output(confusionMatrix(class_pred_klfda_q0_q2_ShannonZ$X..i..,klfda_q0_q2_Shannon_model_class$X..i..),file = "Confusionmatrix_SS_q0_q2_Shannon_scaledsigma0.01_knn_50_klfda_VC.txt")
       
       print("finished q0_q2_Shannon, cv q1_shannon")
       save.image(file = "CV_KLFDA.RData")
       date()
       ### q1_Shannon
       
       klfda_q1_Shannon=list()
       n=500
       klfda_q1_Shannon_model=list()
       pred_klfda_q1_Shannon=list()
 

       pred_klfda_q1_ShannonZ=list()
       for(i in 1:n) {
         klfda_q1_Shannon[[i]]=list()
         pred_klfda_q1_Shannon[[i]]=list()
         

         pred_klfda_q1_ShannonZ[[i]]=list()
        
         
         klfda_q1_Shannon[[i]]=klfda_1(as.matrix(Sh_1D_scaled[-i,]),as.factor(Model)[-i],r=20,kernel=rbfdot(sigma = 0.05),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=50,metric="plain")
         pred_klfda_q1_Shannon[[i]]= predict.kfda(klfda_q1_Shannon[[i]],as.matrix(as.data.frame(Sh_1D_scaled)[i,]),prior = NULL)
        

         pred_klfda_q1_ShannonZ[[i]]=pred_klfda_q1_Shannon[[i]]$bayes_assig_pred$class
         
         
         klfda_q1_Shannon_model[[i]]=Model[i]
       }
     
      
       class_pred_klfda_q1_ShannonZ=do.call(rbind,lapply(pred_klfda_q1_ShannonZ, data.frame))
       
       klfda_q1_Shannon_model_class=do.call(rbind,lapply(klfda_q1_Shannon_model, data.frame))
       
       # confusionMatrix(class_pred_klfda_q1_ShannonZ$X..i..,klfda_q1_Shannon_model_class$X..i..)
       capture.output(confusionMatrix(class_pred_klfda_q1_ShannonZ$X..i..,klfda_q1_Shannon_model_class$X..i..),file = "Confusionmatrix_SS_q1_Shannon_scaled_sigma0_05knn_50_klfda_VC.txt")
      ###
       print("finished SSq1_Shannon, cv all")
       save.image(file = "CV_KLFDA.RData")
       date()
       
       ### q0_q1_q2_Shannon
       
       klfda_q0_1D_q2_Shannon=list()
       n=500
       klfda_q0_1D_q2_Shannon_model=list()
       pred_klfda_q0_1D_q2_Shannon=list()
     
  
       pred_klfda_q0_1D_q2_ShannonZ=list()
       for(i in 1:n) {
         klfda_q0_1D_q2_Shannon[[i]]=list()
         pred_klfda_q0_1D_q2_Shannon[[i]]=list()
        
      
         pred_klfda_q0_1D_q2_ShannonZ[[i]]=list()
         #predict1[[i]]=list()   r,order, regParam, priors,tol,reg,plotFigures=FALSE,verbose
         
         klfda_q0_1D_q2_Shannon[[i]]=klfda_1(as.matrix(SSq0_q2_Sh_D_scaled[-i,]),as.factor(Model)[-i],r=20,kernel=rbfdot(sigma = 0.01),tol=1e-30,prior=c(1,1,1,1,1)/5,usekernel = TRUE,CV=FALSE,fL=0.25,knn=55,metric="plain")
         pred_klfda_q0_1D_q2_Shannon[[i]]= predict.kfda(klfda_q0_1D_q2_Shannon[[i]],as.matrix(as.data.frame(SSq0_q2_Sh_D_scaled)[i,]),prior = NULL)
         # pred_klfda_q0_1D_q2_Shannon0[[i]]=pred_klfda_q0_1D_q2_Shannon[[i]]$posteriors.class
         # pred_klfda_q0_1D_q2_Shannon1[[i]]=pred_klfda_q0_1D_q2_Shannon[[i]]$posteriors.class0
        
         pred_klfda_q0_1D_q2_ShannonZ[[i]]=pred_klfda_q0_1D_q2_Shannon[[i]]$bayes_assig_pred$class
         # predict1 = function(i) predict.klfda_q0_1D_q2_Shannona(klfda_q0_1D_q2_Shannona[[i]],Shannon[i,])[1] 
         
         klfda_q0_1D_q2_Shannon_model[[i]]=Model[i]
       }
       #### if we do not give r the number of reduced dimensionality, the results will fullly take the use of all data
      
       class_pred_klfda_q0_1D_q2_ShannonZ=do.call(rbind,lapply(pred_klfda_q0_1D_q2_ShannonZ, data.frame))
       
       klfda_q0_1D_q2_Shannon_model_class=do.call(rbind,lapply(klfda_q0_1D_q2_Shannon_model, data.frame))
       
       # confusionMatrix(class_pred_klfda_q0_1D_q2_ShannonZ$X..i..,klfda_q0_1D_q2_Shannon_model_class$X..i..)
       capture.output(confusionMatrix(class_pred_klfda_q0_1D_q2_ShannonZ$X..i..,klfda_q0_1D_q2_Shannon_model_class$X..i..),file = "Confusionmatrix_SS_q0_1D_q2_H_0_01_55klfda_VC.txt")
       save.image(file = "CV_KLFDA.RData")
       print("finished")
       date()
     