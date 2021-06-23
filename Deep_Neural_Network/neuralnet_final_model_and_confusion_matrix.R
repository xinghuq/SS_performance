
  
write.csv(gbmGrid_model_mlpWeightDecayML_SSq0$pred,file = "CV_Neural_SS_Ar.csv")
write.csv(gbmGrid_model_mlpWeightDecayML_SSq1$pred,file = "CV_Neural_SS_1D.csv")
write.csv(gbmGrid_model_mlpWeightDecayML_SSq2$pred,file = "CV_Neural_SS_He.csv")
write.csv(gbmGrid_model_mlpWeightDecayML_SS_shannon$pred,file = "CV_Neural_SS_H.csv")
write.csv(gbmGrid_model_mlpWeightDecayML_SS_trad$pred,file = "CV_Neural_SS_Ar_He.csv")
write.csv(gbmGrid_model_mlpWeightDecayML_SSq0_q2_D$pred,file = "CV_Neural_SS_Ar_D_He.csv")
write.csv(gbmGrid_model_mlpWeightDecayML_SSq0_q2_Shannon$pred,file = "CV_Neural_SS_Ar_H_He.csv")
write.csv(gbmGrid_model_mlpWeightDecayML_SSq1_shannon$pred,file = "CV_Neural_SS_H_D.csv")
write.csv(gbmGrid_model_mlpWeightDecayML_SSq0_q2_Sh_D$pred,file = "CV_Neural_SS_Ar_H_He_D.csv")

pred_Ar=read.excel()
pred_H=read.excel()
pred_He=read.excel()
pred_D=read.excel()
pred_Ar_He=read.excel()
pred_Ar_He_D=read.excel()
pred_Ar_He_H=read.excel()
pred_H_D=read.excel()
pred_Ar_H_He_D=read.excel()

library(caret)

confusionMatrix(pred_Ar$pred,reference=pred_Ar$obs,mode="everything")
confusionMatrix(pred_H$pred,reference=pred_H$obs)
confusionMatrix(pred_He$pred,reference=pred_He$obs)
confusionMatrix(pred_D$pred,reference=pred_D$obs)
confusionMatrix(pred_Ar_He$pred,reference=pred_Ar_He$obs)
confusionMatrix(pred_Ar_He_D$pred,reference=pred_Ar_He_D$obs)
confusionMatrix(pred_Ar_He_H$pred,reference=pred_Ar_He_H$obs)
confusionMatrix(pred_H_D$pred,reference=pred_H_D$obs)
confusionMatrix(pred_Ar_H_He_D$pred,reference=pred_Ar_H_He_D$obs)
gbmGrid_model_mlpWeightDecayML_SSq2$modelInfo

ggplot(gbmGrid_model_mlpWeightDecayML_SSq0_q2_Sh_D)
gbmGrid_model_mlpWeightDecayML_SSq0_q2_Sh_D$finalModel

 varImp(gbmGrid_model_mlpWeightDecayML_SSq0_q2_Sh_D)
 
 save.image(file = "neural_net_confusion_matrix_data.RData")
