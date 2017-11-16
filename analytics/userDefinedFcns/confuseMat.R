confuseMat <- function(predictedC, trueC)
{
  #generate random data 
  data = data.frame(trueC,predictedC)
  names(data) = c("Actual", "Predicted") 
  
  #compute frequency of actual categories
  actual = as.data.frame(table(data$Actual))
  names(actual) = c("Actual","ActualFreq")
  
  #build confusion matrix
  confusion = as.data.frame(table(data$Actual, data$Predicted))
  names(confusion) = c("Actual","Predicted","Freq")
  
  #calculate percentage of test cases based on actual frequency
  confusion = merge(confusion, actual, by=c("Actual"))
  confusion$Percent = confusion$Freq/confusion$ActualFreq*100
  
  #render plot
  # we use three different layers
  # first we draw tiles and fill color based on percentage of test cases
  tile <- ggplot() +
    geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1) +
    labs(x="Actual",y="Predicted")
  tile = tile + 
    geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black") +
    #scale_fill_gradientn(values=c(0,0.5,1), colors=c("white","red","green"))
    scale_fill_gradient(low="white",high="blue")
    
  # lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
  tile = tile + 
    geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 
  
  #render
  tile
}
