### Modeling tests
###
### INPUT train and test sets, id variable for test set
### train has labels in first column test not
### Output:
### Dataframe with all the predictions for test set in each column
### Dataframe with all the predictions for train set in each column
# Create container datasets for all predictions

t1_predictions<-as.data.frame(t1$label)
t2_predictions<-as.data.frame(t2$label)
names(t1_predictions)<-"label"
names(t2_predictions)<-"label"
test_predictions<-as.data.frame(id)
names(test_predictions)<-"id"

max_acc<-0
best_method="none"

accdf<-data.frame(method="",accuracy=0)
names(accdf)<-c("method","accuracy")
accdf<-accdf[-1]


for (method in c("svmRadial","C5.0","RRF","nnet")) {
    print (paste("Running:",method))
    # Fit training 
    fit<-train(as.factor(t1$label)~.,method=method,data=t1)
    # Save Model
    save(fit,file=paste("model_",method,".model",sep=""))
    
    # Make predictions
    pred_t1<-predict(fit,t1)
    pred_t2<-predict(fit,t2[,-1])
    pred_test<-predict(fit,test)
    

    # Compute Accuracy
    acc<- sum((pred_t2 == t2$label))/length(t2$label)
    print (paste("Accuracy for ",method,":",acc))
    accdf1<-data.frame(method=method,accuracy=toString(acc))
    accdf = rbind(accdf,accdf1)
    write.csv(accdf,"accuracy.csv",row.names=FALSE)
    
    if (acc>max_acc) {
        max_acc <- acc
        best_method <- method
    }
    
    # Aggregate
    pred_t1<-as.data.frame(pred_t1)
    pred_t2<-as.data.frame(pred_t2)
    pred_test<-as.data.frame(pred_test)
    
    names(pred_t1)<-method
    names(pred_t2)<-method
    names(pred_test)<-method
    
    t1_predictions<-cbind(t1_predictions,pred_t1)
    t2_predictions<-cbind(t2_predictions,pred_t2)
    test_predictions<-cbind(test_predictions,pred_test)
    
    # Save Aggregate
    write.csv(t1_predictions,"t1_predictions.csv",row.names=FALSE)
    write.csv(t2_predictions,"t2_predictions.csv",row.names=FALSE)
    write.csv(test_predictions,"test_predictions.csv",row.names=FALSE)

    # NOW create prediction for test set
    result <- pred_test
    
    ### IMPORTANT TO CHANGE THIS
    submit <- data.frame(problem_id = id, classe = result)
    
    result<-as.data.frame(result)
    names(result)<-method
    write.csv(submit, file = paste("submit_",method,".csv",sep=""), row.names = FALSE)
}

print(paste("Best method was:",best_method,"with accuracy:",max_acc))

for (method in c("gam","rf","LogitBoost")) {
  fit<- train(as.factor(t1_predictions$label)~.,method=method,data=t1_predictions)
  pred<-predict(fit,t2[,-1])
  acc<- sum((pred == t2$label))/length(t2$label)
  write.csv(accdf,"accuracy.csv",row.names=FALSE)
  accdf<-rbind(accdf,c(paste("ensamble_",method,sep=""),acc))
  print (paste("Accuracy for ensamble with:",method,"=",acc))
  pred_test<-predict(fit,test)
  result <- pred_test
  submit <- data.frame(problem_id = id, classe = result)
  result<-as.data.frame(result)
  names(result)<-method
  write.csv(submit, file = paste("submit_ensamble_",method,".csv",sep=""), row.names = FALSE)
}