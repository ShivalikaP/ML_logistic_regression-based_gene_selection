## Select most frequently identified descriptors using logistic regression-based feature selection

#1. Load essential libraries and datasets
library("glmnet")
t(as.matrix(exprs(data_exp)))-> dataC #data_exp: expression dataset variable
as.matrix(cbind(dataC, c(rep(1,24), rep(0,28))))->dataC2  #...1 to strees and 0 to control
colnames(dataC2)[ncol(dataC2)]<-"Outcome"
dataC2[,-ncol(dataC2)]->dataC3


#2. Feature (gene) selection
#2.1 Compute importanrt features
set.seed(1)
cv.fit.nmC <- cv.glmnet(dataC3[,1:(ncol(dataC2)-1)], dataC3[,ncol(dataC2)])
cv.fit.nmC$lambda.min #0.02908022 #Set  as best lambda
eNetPermC<-function(dataC3)
{
  descSC<-list()
  descCoeffC<-list()
  for (i in 1:1000)
{
    set.seed(i)
    newdataC3IndC<-sample(1:nrow(dataC3),floor(0.7*nrow(dataC3)))
    newdataC3<-dataC3[newdataC3IndC,]
    fittC<-glmnet(newdataC3[,1:(ncol(dataC2)-1)], newdataC3[,ncol(dataC2)], family="binomial",alpha=0.5, lambda=cv.fit.nmC$lambda.min) #default alpha =0.5 for glmnet
    CoefficientsC<-as.vector(coef(fittC))
    indexActC<-which(CoefficientsC!=0)
    descSC[[length(descSC)+1]]<-indexActC
    indexCoeffC<-CoefficientsC[indexActC]
    descCoeffC[[length(descCoeffC)+1]]<-indexCoeffC
    print(i)
  }
  return(list(SelecteddescsC=descSC,descCoeffC=descCoeffC))
}

# Run the function on dataC3
eNetPermC(dataC3)->RAND1000C
alldescsC <- unlist(RAND1000C$Selecteddescs)
alldescsTableC <- as.data.frame(table(alldescsC))
alldescsTableC
colnames(alldescsTableC)<-c("alldescs","Freq")

descnamesC <- c("intercept", colnames(dataC3[,1:1:(ncol(dataC2)-1)]))
tmp.dfC <- as.data.frame(cbind(descnamesC, alldescsTableC2))


#2.2 Compute importanrt features with frequenc >600
tmp.dfC[order(tmp.dfC[,3],decreasing=T),]->ALL_descS_ORDEREDC
ALL_descS_ORDEREDC[ALL_descS_ORDEREDC$Freq>600,]->imp_desc
as.character(imp_desc$descnames)[-1]->desc_selected_glmnet 
write.table(desc_selected_glmnet, "desc_selected_glmnet.txt", col.names=F, quote=F, row.names=F)




#3. Make a classifier based on selected features (5 fold cross-validation)
require(party)
require(ROCR)
dataC[,match(desc_selected_glmnetD, colnames(dataC))]->dataD4
cbind(dataD4, dataC[,dim(desc_selected_glmnet)[1]])->dataD5
colnames(dataD5)[dim(desc_selected_glmnet)[1]]+1]<-c("OUTCOME")

x.cf1.perf<-list() #store "tpr" and "fpr" values
auc_val<-list() #Store AUC values


for (i in 1:5) # 5 fold cross-validation
{
set.seed(i)
split<-sample(nrow(dataD5), floor(0.8*nrow(dataD5)))
train<-as.data.frame(dataD5[split,])
test<-as.data.frame(dataD5[-split,])

x.cf1 <- cforest(OUTCOME ~ ., data=train1, control = cforest_unbiased(mtry = ncol(train1)-2))
x.cf1.pred <- predict(x.cf1, newdata=test)
x.cf1.prob <-  1- unlist(treeresponse(x.cf1, test), use.names=F)[seq(1,nrow(test)*2,2)]

x.cf1.prob.rocr[i] <- prediction(x.cf1.prob, test[,dim(desc_selected_glmnet)[1]]+1])
x.cf1.perf[i] <- performance(x.cf1.prob.rocr, "tpr","fpr")
auc_val[i]<- performance(x.cf1.prob.rocr,"auc") #
}

for (i in 1:length(auc_val))
{
png(paste0("AUC",i,".png"), width = 5, height = 5, units = 'in', res = 300); 
plot(auc_val[i], col=1,lwd=2.5)
dev.off()
}














