# twiago recommender preparation
# test campaign - in production from Monday, 13th of October 2014

# Testing Goal: Prediction for Mobile.de Gebrauchtwagen Oktober Testkampagne

# Load and explore data
# Everything related to campaign 653 - Mobile.de Gebrauchtwagen Oktober 2014

### TODO ###
# Output: For each ad give the best adspaces
# Output: For each adspaces give the best ads 
# Combine both "RECOMMENDATION VIEWS"
############

# Load packages
require("R.matlab")

# LOAD and EXTRACT
if (1) {
  # replace "report.csv" by daily report - my guess would be that accumulated data is more robust than daily data
  # nevertheless: we should compare daily and average performance to be sure that nothing goes wrong
  # read header first
  file_name = "report_october01-23rd.csv"
  twnames_raw <- read.csv(file_name,nrow=1, header=FALSE,colClasses="character")
  twnames <- sub("\xe4","ae",twnames_raw)
  twnames[5]="CPC"
  twnames <- sub("-",".",twnames)
  # read only the data, skip header
  tw01 <- read.csv(file_name,header=FALSE,skip=1)
  #jvm_data <- jvm_data[,1:length(cnames)]
  colnames(tw01) = twnames 
  # remove last line, if needed
  lastline <- length(october_start[,1])
  if (tw01$Werbeflaeche[lastline]=="[Summe]") tw01 <- tw01[-lastline,]
  
}

# Extract relevant adspaces and ads
if (0){

  # Ad-hoc criteria, for testing only, will be refined later on, much more data to learn from
  # adspaces: more than 1000 Views in October
  tw_rel <- tw01[is.element(tw01$Werbeflaeche,rel_adspace_name),]
  
  # Remove Kampagne.ID (is always 0...)
  tw_rel <- tw_rel[,-7]

  # ads: only campaigns that run until end of October
  # Irrelevant ad-ids (from campaigns that end during october or are already inactive)
  # Filter these out
  irr_ad_ids <- c(2926,2996,2756:2761,2768:2773,3032:3033,3036,3043,3124:3126)
  tw_rel <- tw_rel[!is.element(tw_rel$Werbemittel.ID,irr_ad_ids),]
  
#twRedOx <- tw01[tw01$Kampagne=="",]
#twRedOxdata <- data.frame(twRedOx[,c(7,9,4:6)])
#twRedOxdata_100plus <- twRedOxdata[twRedOxdata$Klicks>=10,]

}

### ### ###
# Build data table for werbeflaeche / werbemittel ctr and fill matrices, 10plus klicks
if (0) {
  
  unique_ads <- unique(tw_rel$Werbemittel.ID)
  unique_spaces <- unique(tw_rel$Werbeflaechen.ID)
  nof_ads <- length(unique_ads)
  nof_spaces <- length(unique_spaces)
  
  unique_adspace_names = rep(0,nof_spaces)
  mode(uni_adspace_names)= "character"
  for (i in 1:nof_spaces) {
    temp_name <- tw_rel[tw_rel$Werbeflaechen.ID==unique_spaces[i],1]
    #print(temp_name[1])
    unique_adspace_names[i]<-toString(temp_name[1])
  }
  
  unique_ad_names = rep(0,nof_ads)
  mode(uni_ad_names)= "character"
  for (i in 1:nof_ads) {
    temp_name <- tw_rel[tw_rel$Werbemittel.ID==unique_ads[i],2]
    #print(temp_name[1])
    unique_ad_names[i]<-toString(temp_name[1])
  }
  
  #write.table(unique_adspace_names,file="adspace_ids.txt",sep= " ")
  #unique_ad_names<-unique(tw_rel$Werbemittel)

#  unique_ads <- unique(twRedOxdata_100plus$Werbemittel.ID)
 # unique_spaces <- unique(twRedOxdata_100plus$Werbeflaechen.ID)
#  unique_adspace_names<-unique(twRedOx[twRedOx$Klicks>=10,1])
#  unique_ad_names<-unique(twRedOx[twRedOx$Klicks>=10,3])
  
recom_matrix <- matrix(0,nrow=nof_ads,ncol=nof_spaces)
recom_real_matrix <- matrix(NA,nrow=nof_ads,ncol=nof_spaces)
recom_real_matrix_norm <- recom_real_matrix
index_matrix <- matrix(0,nrow=nof_ads,ncol=nof_spaces)

# For removing empty rows:
index_test <- rep(0,nof_ads)
print("Start data table")
# Fill matrices with data
for (i in 1:nof_ads){
  temp_mat <- tw_rel[tw_rel$Werbemittel.ID==unique_ads[i],]
  #print(temp_mat)
  for (j in 1:nof_spaces) {
    if (is.element(unique_spaces[j],unique(temp_mat$Werbeflaechen.ID))) {
      #print(unique_spaces[j])
      if (temp_mat[temp_mat[,6]==unique_spaces[j],3]!=0 & temp_mat[temp_mat[,6]==unique_spaces[j],4]>0) {
             
      index_matrix[i,j] = 1
      recom_matrix[i,j] = temp_mat[temp_mat[,6]==unique_spaces[j],4]/temp_mat[temp_mat[,6]==unique_spaces[j],3]
      if (is.na(recom_matrix[i,j])) print(paste("NA",paste(i,j))) 
      if (is.infinite(recom_matrix[i,j])) print(paste("Inf",paste(i,j))) 
      recom_real_matrix[i,j] = recom_matrix[i,j]
      }
    } 
  }
  index_test[i] = sum(index_matrix[i,]>0)
}
# For removing empty columns:
#space_test = rep(0,nof_spaces)
#for (j in 1:nof_spaces) space_test[j] = sum(index_matrix[,j]>0)

rtest <- recom_matrix[index_test>0,]
itest <- index_matrix[index_test>0,]
space_test = rep(0,nof_spaces)
for (j in 1:nof_spaces) space_test[j] = sum(itest[,j]>0)
rtest2<-rtest[,space_test>0]
itest2<-itest[,space_test>0]

print("End data table")
}

# solve some naming problems (am-guenstigsten...), adspace names cannot be the same, add IDs
if (0) {
  
unique_relevant_spaces <- unique_adspace_names[space_test>0]
# there are some names twice, add ID (am guenstigsten ROD...):
unique_relevant_spaces[85:87]<-paste(unique_relevant_spaces[85:87],"ID",c(29,28,27))
unique_relevant_sids <- unique_spaces[space_test>0]
write.table(unique_relevant_spaces,file="adspace_ids.txt",sep= " ")
unique_relevant_ads <- unique_ads[index_test>0]
unique_relevant_ad_names <- unique_ad_names[index_test>0]

}

# Fill matrices with full data and reduce dimensions
if (0){
  print("Start Fill matrices")
data_matrix <- matrix(0,nrow=nof_ads,ncol=nof_spaces)
data_index_matrix <- matrix(0,nrow=nof_ads,ncol=nof_spaces)

for (i in 1:nof_ads){
  temp_mat <- tw_rel[tw_rel$Werbemittel.ID==unique_ads[i],]
  #print(temp_mat)
  for (j in 1:nof_spaces) {
    if (is.element(unique_spaces[j],unique(temp_mat$Werbeflaechen.ID))) {
      #print(unique_spaces[j])
      data_index_matrix[i,j] = 1
      data_matrix[i,j] = temp_mat[temp_mat[,6]==unique_spaces[j],4]/temp_mat[temp_mat[,6]==unique_spaces[j],3]
    }
  }
}

# Reduce dimensions
if (0) {
  
# Reduce size to 8 -x- 32 -> remove row 13 which has only 2 entries anyway
model_matrix <- data_matrix[,-13]
model_index_matrix <- data_index_matrix[,-13]

# Store IDs of relevant ads and spaces
model_unique_ads <- unique_ads
model_unique_spaces <- unique_spaces[-13]

} # end reduce dimensions 

print("End Fill matrices")
} # end full data matrices and reduce to 8-x-32 

# Write csv-file for recommender input
if (0){
#  write.matrix(index_matrix[index_test>0,],"index.matrix")
#  write.matrix(recom_matrix[index_test>0,],"recom.matrix")
  
  write.matrix(itest2,"index.matrix")
  write.matrix(rtest2,"recom.matrix")
  
} 

# Do not use:
# write.csv(data_matrix,"recommender_input.csv")


# Compute matrix for special test recommendation using the IDEA approach
# Test each ad for 2 days on 4 adspaces per day -> special index matrix
if (0) {
  
  special_index_matrix <- matrix(0,nrow=8,ncol=32)
  for (i in 1:8) special_index_matrix[i,((i-1)*4+1):(4*i)]=1
  for (i in 1:7) special_index_matrix[i,(i*4+1):(4*(i+1))]=1
  special_index_matrix[8,1:4]=1
  special_index_matrix=(special_index_matrix & model_index_matrix)
  mode(special_index_matrix)="integer"
  special_data_matrix = model_matrix
  for (i in 1:(32*8)) if (special_index_matrix[i]==0) special_data_matrix[i] = 0
} # end special recommender setup

# compute summary information for all ads with 10+ Klicks on at least one adspace
# and for all adspaces that have hosted at least one ad with 10+ Klicks
# The summary is computed from the whole data set restricted to these 8 ads and 33 adspaces
if (0) {
  
summary_ads <- matrix(0, nrow=nof_ads,3)
colnames(summary_ads) <- c("Werbemittel.ID","Total Views","Total Klicks")
summary_spaces <- matrix(0, nrow=nof_spaces,3)
colnames(summary_spaces) <- c("Werbeflaechen.ID","Total Views","Total Klicks")

for (i in 1:nof_ads){
  summary_ads[i,1] = unique_ad_names[i]
  summary_ads[i,2:3] = colSums(tw_rel[tw_rel$Werbemittel.ID==unique_ads[i],3:4])
}
for (i in 1:nof_spaces){
  summary_spaces[i,1] = unique_adspace_names[i]
  summary_spaces[i,2:3] = colSums(tw_rel[tw_rel$Werbeflaechen.ID==unique_spaces[i],3:4])
}

# Now compute the same summary for the 8-x-33 combinations only
summary_10plusads <- matrix(0, nrow=nof_ads,3)
colnames(summary_10plusads) <- c("Werbemittel.ID","Total Views","Total Klicks")
summary_10plusspaces <- matrix(0, nrow=nof_spaces,3)
colnames(summary_10plusspaces) <- c("Werbeflaechen.ID","Total Views","Total Klicks")

for (i in 1:nof_ads){
  summary_10plusads[i,1] = unique_ad_names[i]
  summary_10plusads[i,2:3] = colSums(twRedOxdata_100plus[twRedOxdata_100plus$Werbemittel.ID==unique_ads[i],3:4])
}
for (i in 1:nof_spaces){
  summary_10plusspaces[i,1] = unique_adspace_names[i]
  summary_10plusspaces[i,2:3] = colSums(twRedOxdata_100plus[twRedOxdata_100plus$Werbeflaechen.ID==unique_spaces[i],3:4])
}

# combine overview
overview_ads <- cbind(summary_ads,summary_10plusads[,2:3])
colnames(overview_ads) = c("Ad.ID","Views Total","Klicks Total","Views Best","Klicks Best")
overview_ads = overview_ads[,c(1,2,4,3,5)]
overview_spaces <- cbind(summary_spaces,summary_10plusspaces[,2:3])
colnames(overview_spaces) = c("Space.ID","Views Total","Klicks Total","Views Best","Klicks Best")
overview_spaces = overview_spaces[,c(1,2,4,3,5)]

} # end summary


###########################
### NOT WORKING
###########################
# compute recommenderlabs recommendation - first tests indicate that it seems to be very bad
# no optimization, no regularization, no (mathematical) collaborative filtering, weird naming
if (0) {
  
# Normalize using rowMeans
rowmeans_vec = rowSums(recom_matrix)/rowSums(index_matrix)
recom_matrix_norm <- recom_matrix - rowmeans_vec
recom_real_matrix_norm[index_matrix] <- recom_matrix_norm[index_matrix]
# Start recommendations, prepare testing as well
recom_real_matrix <- as(recom_real_matrix_norm,"realRatingMatrix")
#recom_norm_matrix <- normalize(recom_real_matrix)
#recom_norm_compare <- as(recom_norm_matrix,"matrix")

# Setting up the test
recom_real_test = recom_matrix
recom_real_test[1,13] = 0
index_matrix_test <- index_matrix
index_matrix_test[1,13] = 0
rowmeans_vec_test = rowSums(recom_real_test)/rowSums(index_matrix_test)
recom_norm_test <- recom_real_test - rowmeans_vec_test 
recom_norm_test[1,13] = NA
#print(recom_norm_test)
recom_norm_test[recom_matrix==0]=NA
recom_norm_test <- as(recom_norm_test,"realRatingMatrix")
#recom_norm_test <- normalize(recom_real_test) 

# Buld Item-based collaborative filtering recommendations (IBCF) and compare results
norm_test_rsys <- Recommender(recom_norm_test,method="IBCF")
rec_pred_test_r1 <- predict(norm_test_rsys,recom_norm_test,type="ratings")
rec_preds <- as(rec_pred_test_r1,"matrix")
rec_preds_denorm <- rec_preds + rowmeans_vec_test
#print(cbind(rec_preds[13:16],recom_norm_compare[1,13:16],abs(rec_preds[13:16]-recom_norm_compare[1,13:16])))

} # end recommenderlabs

# plot population/occupation pattern of index matrix
if (0) {
  
library(reshape2)
library(ggplot2)
m = matrix(rnorm(20),5)
ggplot(melt(index_matrix), aes(Var1,Var2, fill=value)) + geom_raster()

} # end population pattern