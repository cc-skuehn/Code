# Script for processing the final predictions

# load octave recommendation output
pred_mat<-read.table("test_pred.mat")

# load campaign names and ids
#camp <- camp_list 
#camp<-read.csv("campaignsads11.11.2014.csv",skip=1)
camp <- data.frame(cbind(vztage$Kampagne,vztage$Werbemittel,vztage$Kampagnen.ID))
if (1){
  
file_name = "today.csv"
today <- read.csv(file_name,header=F,skip=1)
today <- today[,c(2:4,8)]
colnames(today) <- c("Kampagne","Werbemittel","Views","Kampagnen.ID")
today <- today[!today$Views<10,]
for (i in 1:length(today[1,])){
 today[,i] = as.character(today[,i]) 
}
# remove umlauts etc.
for (i in 1:2) {
  today[,i] <- gsub("\xe4","ae",today[,i])
  today[,i] <- gsub("\xe9","e",today[,i])
  today[,i] <- gsub("\xd6","Oe",today[,i])
  today[,i] <- gsub("\xf6","oe",today[,i])
  today[,i] <- gsub("\xfc","ue",today[,i])
  today[,i] <- gsub("\xa0","_",today[,i])
}


camp <- data.frame(cbind(today$Kampagne,today$Werbemittel,today$Kampagnen.ID))

}

#colnames(camp)=c("Kampagne","Werbemittel","Views","Kampagnen.ID","Werbemittel.ID")
colnames(camp)=c("Kampagne","Werbemittel","Kampagnen.ID")

# TODO: extract current ads
#curads <- unique(camp$Werbemittel)
#currelads <- intersect(curads,adred)
#lcr <- length(currelads)

#adcamp <- rep("--None--",lcr)

adcamp <- cbind(rep("--None--",lna),rep("--None--",lna))

for (i in 1:lna) {
  adcamp[i,] = c(as.character(camp$Kampagnen.ID[camp$Werbemittel==as.character(adred[i])][1]),as.character(camp$Kampagne[camp$Werbemittel==as.character(adred[i])][1]))
  #print(camp$Kampagnen.ID[camp$Werbemittel==as.character(adred[i])][1])
  #print(camp$Kampagnen.ID[camp$Werbemittel==as.character(currelads[i])][1])
  #adcamp[i] = camp$Kampagnen.ID[camp$Werbemittel==as.character(currelads[i])][1]
  
}

recom_space_list = c("kicker.de - PromoAd",
                     "Zeit - MR (300x250)",
                     "Handelsblatt - S (405x150)",
                     "ARIVA - wSky (160x600)",
                     "ARIVA - LB (728x90)",
                     "Sueddeutsche.de - Homepage Marginalspalte",
                     "WetterOnline - Navi rechts (300x90) | Adj 1 IHCL (80x80)",
                     "welt.de - Panorama - PAd BT 8-SP Artikel",
                     "AGOF Premium 1 - Logout Lounge Teaserline",
                     "AGOF Premium 2 - Logout Lounge Teaserline",
                     "finanztreff.de - S (940x45)",
                     "tvinfo.de - S (599x331)",
                     "Neue Zuercher Zeitung | S 4 IHCL (560x250) ",
                     "T-Online.de ContentBox")

recom_filename_list = c("kicker-Promo-Ad",
                        "Zeit-MR",
                        "Handelsblatt-S-405x150",
                        "ARIVA-wSky",
                        "ARIVA-LB",
                        "Sueddeutsche-Marginal",
                        "Wetter-Online-Navi-rechts",
                        "welt.de-Panorama-PAd-BT-8-SP-Artikel",
                        "AGOF-Premium-1-Logout-Lounge-Teaserline",
                        "AGOF-Premium-2-Logout-Lounge-Teaserline",
                        "finanztreff.de-S",
                        "tvinfo.de-S",
                        "NZZ-S-4-IHCL",
                        "T-Online.de-ContentBox")

rec_len <- length(recom_space_list)
#for (i in 1:rec_len) recom_postprocessing(recom_space_list[i],recom_filename_list[i])
for (i in 1:rec_len) recom_advanced_postprocessing(recom_space_list[i],recom_filename_list[i])

#recom_postprocessing("kicker.de - PromoAd","KickerPromoAd")

if (0) {
  
### Top Kicker + Zeit Section

# indices of spaces kicker Promo and Zeit MR
#indkick = 90
indkick = which(spred=="kicker.de - PromoAd")
#indzeit = 137
indzeit = which(spred=="Zeit - MR (300x250)")
#ind_tk <- which(final_adspace_names=="kicker.de - PromoAd")

# Neue Platzierungen
indhandels = which(spred=="Handelsblatt - S (405x150)")
indarivawsky = which(spred=="ARIVA - wSky (160x600)")
indarivalb = which(spred=="ARIVA - LB (728x90)")
indsuedmarginal = which(spred=="Sueddeutsche.de - Homepage Marginalspalte")

# eTKP predicition
if (0) {
  ord_kicker <- order(pred_mat[indkick,],decreasing=TRUE)
ord_zeit <- order(pred_mat[indzeit,],decreasing=TRUE)
top_kickerPromoAd <- data.frame(cbind(t(pred_mat[indkick,]),as.character(adred),as.character(adcamp)))[ord_kicker,]
top_zeitMR <- data.frame(cbind(t(pred_mat[indzeit,]),as.character(adred),as.character(adcamp)))[ord_zeit,]
}

# CTR prediction
ord_kicker <- order(pred_mat[indkick,]*cpc,decreasing=TRUE)
ord_zeit <- order(pred_mat[indzeit,]*cpc,decreasing=TRUE)
top_kickerPromoAd <- data.frame(cbind(t(pred_mat[indkick,])*cpc*1000,as.character(adred),adcamp))[ord_kicker,]
top_kickerPromoAd <- top_kickerPromoAd[!is.na(top_kickerPromoAd[,3]),]
top_zeitMR <- data.frame(cbind(t(pred_mat[indzeit,])*cpc*1000,as.character(adred),adcamp))[ord_zeit,]
top_zeitMR <- top_zeitMR[!is.na(top_zeitMR[,3]),]

colnames(top_kickerPromoAd)=c("eTKP","Werbemittel","Kampagnen-ID","Kampagne")
write.csv(top_kickerPromoAd,"top_kicker_test.csv",row.names=FALSE)

colnames(top_zeitMR)=c("eTKP","Werbemittel","Kampagnen-ID","Kampagne")
write.csv(top_zeitMR,"top_zeit_test.csv",row.names=FALSE)

# Neue Platzierungen
# CTR prediction
ord_handels <- order(pred_mat[indhandels,]*cpc,decreasing=TRUE)
ord_arivawsky <- order(pred_mat[indarivawsky,]*cpc,decreasing=TRUE)
ord_arivalb <- order(pred_mat[indarivalb,]*cpc,decreasing=TRUE)
ord_suedmarginal <- order(pred_mat[indsuedmarginal,]*cpc,decreasing=TRUE)
top_handels <- data.frame(cbind(t(pred_mat[indhandels,])*cpc*1000,as.character(adred),adcamp))[ord_handels,]
top_handels <- top_handels[!is.na(top_handels[,3]),]

top_arivawsky <- data.frame(cbind(t(pred_mat[indarivawsky,])*cpc*1000,as.character(adred),adcamp))[ord_arivawsky,]
top_arivawsky <- top_arivawsky[!is.na(top_arivawsky[,3]),]

top_arivalb <- data.frame(cbind(t(pred_mat[indarivalb,])*cpc*1000,as.character(adred),adcamp))[ord_arivalb,]
top_arivalb <- top_arivalb[!is.na(top_arivalb[,3]),]

top_suedmarginal <- data.frame(cbind(t(pred_mat[indsuedmarginal,])*cpc*1000,as.character(adred),adcamp))[ord_suedmarginal,]
top_suedmarginal <- top_suedmarginal[!is.na(top_suedmarginal[,3]),]

colnames(top_handels)=c("eTKP","Werbemittel","Kampagnen-ID","Kampagne")
write.csv(top_handels,"top_handelsblatt_s.csv",row.names=FALSE)
colnames(top_arivawsky)=c("eTKP","Werbemittel","Kampagnen-ID","Kampagne")
write.csv(top_arivawsky,"top_ariva_wsky.csv",row.names=FALSE)
colnames(top_arivalb)=c("eTKP","Werbemittel","Kampagnen-ID","Kampagne")
write.csv(top_arivalb,"top_ariva_lb.csv",row.names=FALSE)
colnames(top_suedmarginal)=c("eTKP","Werbemittel","Kampagnen-ID","Kampagne")
write.csv(top_suedmarginal,"top_sueddeutsche_marginal.csv",row.names=FALSE)


## load campaign names and ids
#camp<-read.csv("campaigns.csv",skip=1)
#colnames(camp)=c("Kampagne","Werbemittel","Views","Werbeflaechen_ID","Kampagnen_ID")

}