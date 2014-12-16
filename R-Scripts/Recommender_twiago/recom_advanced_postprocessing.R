# function for advanced recommender postprocessing

# needs spred, pred_mat, adred, adcamp, cpc, data_matrix to be available in workspace

recom_advanced_postprocessing <- function(space="None",filename="None") {
  
  # find indices
  ind <- which(spred==space)
  # order dat according to index
  data_ord <- order(pred_mat[ind,]*cpc,decreasing=TRUE)
  # Advanced part, compare with input data
  compare_vec <- (data_matrix[,ind]*cpc*1000)
  compare_vec[data_index_matrix[,ind]==0] <- NA
  # Combine data
  top_list <- data.frame(cbind(t(pred_mat[ind,])*cpc*1000,compare_vec,as.character(adred),adcamp))[data_ord,]
  colnames(top_list)=c("eTKP","Vergleichswert","Werbemittel","Kampagnen.ID","Kampagne")
  top_list <- top_list[!is.na(top_list$Kampagnen.ID),]
  
  write.csv(top_list,paste(filename,"_advanced",".csv",sep=""),row.names=FALSE)
  
}