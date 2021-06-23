library("caret")
library("randomForest")
library("caretEnsemble")
set.seed(999)

econtrol <- trainControl(method="LOOCV")
tunegrid <- expand.grid(.mtry = (1:44)) 
model_RF_SSq0 <- train(Model ~., data=SSq0,
                       method=c("rf"),ntree=2000,
                       trControl = econtrol,tuneGrid = tunegrid)
RF_varimp_SSq0 <- varImp(model_RF_SSq0)

tunegridD <- expand.grid(.mtry = (1:48)) 
model_RF_SSq1<- train(Model ~., data=SSq1,
                      method=c("rf"),ntree=2000,
                      trControl = econtrol,tuneGrid=tunegridD)
RF_varimp_SSq1 <- varImp(model_RF_SSq1)

model_RF_SSq2<- train(Model ~., data=SSq2,
                        method=c("rf"),ntree=2000,
                        trControl = econtrol,tuneGrid=tunegrid)
RF_varimp_SSq2 <- varImp(model_RF_SSq2)

model_RF_SS_Shannon<- train(Model ~., data=Shannon_all,
                      method=c("rf"),ntree=2000,
                      trControl = econtrol,tuneGrid = tunegridD)
varimp_SS_shannon <- varImp(model_RF_SS_Shannon)

tunegrid2 <- expand.grid(.mtry = (1:90))
model_RF_SS_Shannon_D<- train(Model ~., data=SSq1_Shannnon,
                            method=c("rf"),ntree=2000,
                            trControl = econtrol,tuneGrid = tunegrid2)
varimp_SS_shannon_D <- varImp(model_RF_SS_Shannon_D)


tunegrid3 <- expand.grid(.mtry = (1:88))
model_RF_SS_trad <- train(Model ~., data=SS_tra,
                       method=c("rf"),ntree=2000,
                       trControl = econtrol,tuneGrid = tunegrid3)
varimp_SS_trad= varImp(model_RF_SS_trad)


tunegrid4 <- expand.grid(.mtry = (1:136))
model_RF_SSq0_q2_D<- train(Model ~., data=SSall,
                      method=c("rf"),ntree=2000,
                      trControl = econtrol,tuneGrid = tunegrid4)

varimp_SSq0_q2_D <- varImp(model_RF_SSq0_q2_D)

model_RF_SSq0_q2_Shannon<- train(Model ~., data=SSq0_q2_Shannon,
                        method=c("rf"),ntree=2000,
                        trControl = econtrol,tuneGrid = tunegrid4)

varimp_SSq0_q2_Shannon <- varImp(model_RF_SSq0_q2_Shannon)

tunegrid5 <- expand.grid(.mtry = (1:176))
model_RF_SSq0_q2_Shannon_D<- train(Model ~., data=SSq0_q2_Sh_D,
                                 method=c("rf"),ntree=2000,
                                 trControl = econtrol,tuneGrid = tunegrid5)

varimp_SSq0_q2_Shannon_D<- varImp(model_RF_SSq0_q2_Shannon_D)

save.image(file = "LOOV_CV_RF.RData")
