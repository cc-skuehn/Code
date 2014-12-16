# function for recommender postprocessing

# needs spred, pred_mat, adred, adcamp to be available in workspace

recom_postprocessing <- function(space="None",filename="None") {
  
  # find indices
  ind <- which(spred==space)
  # order dat according to index
  data_ord <- order(pred_mat[ind,]*cpc,decreasing=TRUE)
  top_list <- data.frame(cbind(t(pred_mat[ind,])*cpc*1000,as.character(adred),adcamp))[data_ord,]
  top_list <- top_list[!is.na(top_list[,3]),]
  
  colnames(top_list)=c("eTKP","Werbemittel","Kampagnen-ID","Kampagne")
  write.csv(top_list,paste(filename,".csv",sep=""),row.names=FALSE)
  
}