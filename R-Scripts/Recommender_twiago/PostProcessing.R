# Script for processing the final predictions

# load octave recommendation output
pred_mat<-read.table("test_pred.mat")
pred_mat=t(pred_mat)

# find ad_name indices
go_ind = which(final_ad_names_red=="Gebrauchtwagen Orange")
gr_ind = which(final_ad_names_red=="Gebrauchtwagen Rot")
gs_ind = which(final_ad_names_red=="Gebrauchtwagen silber")

# compute average daily number of views per adspace
daily_average = rel_adspace_summary$Views
nof_days = 23

# Extract and sort prediction for orange
or_sort <- order(pred_mat[,go_ind],decreasing=TRUE)
daily_avg_orange <- floor(daily_average[or_sort]/23)
pred_orange <- data.frame(cbind(1:l_spaces,pred_mat[or_sort,go_ind],daily_avg_orange))
#pred_orange <- data.frame(pred_mat[order(pred_mat[,2],decreasing=TRUE),2])
colnames(pred_orange) = c("Platz","CTR Orange","Reichweite")
#rownames(pred_orange) = unique_relevant_spaces[order(pred_mat[,2],decreasing=TRUE)]
rownames(pred_orange) = final_adspace_names[order(pred_mat[,go_ind],decreasing=TRUE)]
#write.csv(pred_orange,"pred_orange.csv")
write.csv(pred_orange,"pred_orange.csv")

red_sort <- order(pred_mat[,gr_ind],decreasing=TRUE)
daily_avg_red <- floor(daily_average[red_sort]/23)
pred_rot <- data.frame(cbind(1:l_spaces,pred_mat[red_sort,gr_ind],daily_avg_red))
colnames(pred_rot) = c("Platz","CTR Rot","Reichweite")

#pred_rot <- data.frame(cbind(1:l_spaces,pred_mat[order(pred_mat[,gr_ind],decreasing=TRUE),gr_ind]))
##pred_rot <- data.frame(pred_mat[order(pred_mat[,3],decreasing=TRUE),3])
#colnames(pred_rot) = c("Platz","CTR Rot")
##rownames(pred_rot) = unique_relevant_spaces[order(pred_mat[,3],decreasing=TRUE)]

rownames(pred_rot) = final_adspace_names[order(pred_mat[,gr_ind],decreasing=TRUE)]
write.csv(pred_rot,"pred_rot.csv")

sil_sort <- order(pred_mat[,gs_ind],decreasing=TRUE)
daily_avg_silber <- floor(daily_average[sil_sort]/23)
pred_silber <- data.frame(cbind(1:l_spaces,pred_mat[sil_sort,gs_ind],daily_avg_silber))
#pred_orange <- data.frame(pred_mat[order(pred_mat[,2],decreasing=TRUE),2])
colnames(pred_silber) = c("Platz","CTR Silber","Reichweite")

#pred_silber <- data.frame(cbind(1:l_spaces,pred_mat[order(pred_mat[,gs_ind],decreasing=TRUE),gs_ind]))
##pred_silber <- data.frame(pred_mat[order(pred_mat[,4],decreasing=TRUE),4])
#colnames(pred_silber) = c("Platz","CTR Silber")
##rownames(pred_silber) = unique_relevant_spaces[order(pred_mat[,4],decreasing=TRUE)]

rownames(pred_silber) = final_adspace_names[order(pred_mat[,gs_ind],decreasing=TRUE)]
write.csv(pred_silber,"pred_silber.csv")

### Top Kicker Section

ind_tk <- which(final_adspace_names=="kicker.de - PromoAd")
ord_kicker<-order(pred_mat[ind_tk,],decreasing=TRUE)
top_kickerPromoAd<-data.frame(cbind(pred_mat[98,],as.character(final_ad_names_red)))[ord_kicker,]


