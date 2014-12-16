# list of commands for some test preparations

# TODO Remove CPX/CPA campagin and inactive ads
# CPA/CPX: campaign names end on CPA/CPX
# inactive ads: check against adlist of the current day

# extracting all ads and all relevant adspaces
# START PREPARATION

# List of adspaces that are too good:
toogood <- c("Express","ablida","RP Online","navigogo","wearables","welt-blog.ch","MSN.de","naanoo.com","123-und-weg.de")

# List of current campaigns and IDs, reduced by CPX campaigns
if (0) {
  # update file name
  #camp_file ="campaigns_11.11.2014.csv"
  camp_file ="campaigns_17.11.2014.csv"
  camp_list <- data.frame(read.csv(camp_file))
  #camp_list <- camp_list[,c(1,6)]
  camp_list <- unique(camp_list[,c(2,8)])
  camp_list[,1] <- gsub("\xe4","ae",camp_list[,1])
  camp_list[,1] <- gsub("\xe9","e",camp_list[,1])
  camp_list[,1] <- gsub("\xd6","Oe",camp_list[,1])
  camp_list[,1] <- gsub("\xf6","oe",camp_list[,1])
  camp_list[,1] <- gsub("\xfc","ue",camp_list[,1])
  # alternatively, remove everything with \x
  #camp_list[,1] <- gsub("\x","_",camp_list[,1])
  print(camp_list[,1])
  # remove CPX campaigns
  cpx_ind <- which(grepl("CPX",camp_list[,1]))
  print(cpx_ind)
  # use this for filterung out cpx campaigns -> Kampagnen.ID
  cpx_camp <- camp_list[cpx_ind,]
 # print(cpx_camp)
  camp_list = camp_list[-cpx_ind,]
  print(camp_list[1,])
  # remove last 2 lines (Summe und Gelöscht)
  lc <- length(camp_list[,1])
  camp_list = camp_list[-((lc-1):lc),]
}

# prepare everything
if (1) {
  
#file_name = "14tage_18.11.2014.csv"
#file_name = "report_21.-27.11.2014.csv"
file_name = "report_06.-12.12.2014.csv"
#file_name = "report_26.11.-02.12.2014.csv"
vztage <- read.csv(file_name, header=F, skip=1)
cn <- c("Werbeflaeche","Kampagne","Werbemittel","Views","Klicks","Summe_Advertiser","CPC","Kampagnen.ID","Werbemittel.ID")
#cn <- c("Werbeflaeche","Werbemittel","Views","Klicks","Summe_Advertiser","CPC","Kampagnen.ID","Werbemittel.ID")
#cn_raw <- read.csv(file_name,nrow=1, header=FALSE,colClasses="character")
#cn <- sub("\xe4","ae",cn_raw)
colnames(vztage) <- cn

#remove last line, if needed
lastline <- length(vztage[,1])
if (vztage$Werbeflaeche[lastline]=="[Summe]") vztage <- vztage[-lastline,]
# remove Werbemittel [Unbekannt]
if (length(which(vztage$Werbemittel=="[Unbekannt]"))>0) vztage <- vztage[-which(vztage$Werbemittel=="[Unbekannt]"),]
# remove Werbefläche 0
if (length(which(vztage$Werbeflaeche=="0"))>0) vztage <- vztage[-which(vztage$Werbeflaeche=="0"),]

# remove twiago masterkampagne
if (length(which(vztage$Kampagnen.ID==4))>0) vztage = vztage[-which(vztage$Kampagnen.ID==4),]

# remove umlauts etc.
for (i in 1:3) {
  vztage[,i] <- gsub("\xe4","ae",vztage[,i])
  vztage[,i] <- gsub("\xe9","e",vztage[,i])
  vztage[,i] <- gsub("\xd6","Oe",vztage[,i])
  vztage[,i] <- gsub("\xf6","oe",vztage[,i])
  vztage[,i] <- gsub("\xfc","ue",vztage[,i])
  vztage[,i] <- gsub("\xa0","_",vztage[,i])
}

camp_list <- data.frame(cbind(vztage$Kampagne,vztage$Kampagnen.ID))
colnames(camp_list)=c("Kampagne","Kampagnen.ID")

# use this for filterung out cpx campaigns -> Kampagnen.ID
cpx_ind <- which(grepl("CPX",camp_list[,1]))

if (length(cpx_ind)>0) {
  cpx_camp <- camp_list[cpx_ind,]
  camp_list = camp_list[-cpx_ind,]
  cpx_camp_id = unique(cpx_camp$Kampagnen.ID)
#  cpx_camp_id = c(670,691,718,16)
  
  # remove cpx campaings
  vztage <- vztage[-which(is.element(vztage$Kampagnen.ID,cpx_camp_id)),]
}

#cpx_camp_id = c(670,691,718,16)

# extract ads and spaces
all_ads <- unique(vztage$Werbemittel)
all_spaces <- unique(vztage$Werbeflaeche)

nonzero_ads <- unique(vztage$Werbemittel[vztage$Klicks>0])
lna = length(nonzero_ads)

# remove corrupt data
cor_index <- which(vztage$Views<=vztage$Klicks)
if (sum(cor_index>0)) vztage <- vztage[-cor_index,]

# Compute ad summary
nonzero_ads <- unique(vztage$Werbemittel[vztage$Klicks>0])
lna = length(nonzero_ads)

ad_sum <- data.frame(matrix(0,nrow=lna,ncol=3))
colnames(ad_sum) = c("Views","Klicks","CTR")
for (i in 1:lna) {
  # adapt column numbers for views/klicks
  ad_sum[i,1:2] = colSums(vztage[vztage$Werbemittel==nonzero_ads[i],4:5]) 
}
ad_sum[,3] = ad_sum[,2]/ad_sum[,1]
ad_sum <- cbind(ad_sum,nonzero_ads)

# Compute space summary
nonzero_spaces <- unique(vztage$Werbeflaeche[vztage$Klicks>0])
lns = length(nonzero_spaces)

space_sum <- data.frame(matrix(0,nrow=lns,ncol=3))
colnames(space_sum) = c("Views","Klicks","CTR")
for (i in 1:lns) {
  # adapt column numbers for views/klicks
  space_sum[i,1:2] = colSums(vztage[vztage$Werbeflaeche==nonzero_spaces[i],4:5]) 
}
space_sum[,3] = space_sum[,2]/space_sum[,1]
space_sum <- cbind(space_sum,nonzero_spaces)

# reduce data, exclude unreliable data and "toogoods"
vzred <- vztage[vztage$Views>=1000 & vztage$Klicks>0,]
for (i in 1:length(toogood)) {
  vzred <- vzred[!grepl(toogood[i],vzred$Werbeflaeche,perl=TRUE),]
}
adred <- unique(vzred$Werbemittel)
lna <-length(adred)
spred <- unique(vzred$Werbeflaeche)
lns <- length(spred)

# Collect CPC for each ad
cpc <- rep(0,lna)
for (i in 1:lna) {
  temp_mat = vzred$CPC[vzred$Werbemittel==adred[i]]
  #print(temp_mat)
  cpc[i] = max(unique(temp_mat))
}

} # end preparation

# Fill matrices with full data and reduce dimensions
if (1){
  print("Start Fill matrices")
  data_matrix <- matrix(0,nrow=lna,ncol=lns)
  data_index_matrix <- data_matrix
  
  for (i in 1:lna){
    temp_mat <- vzred[vzred$Werbemittel==adred[i],]
    # print(temp_mat)
    for (j in 1:lns) {
      if (is.element(spred[j],unique(temp_mat$Werbeflaeche))) {
       # print(spred[j])
        data_index_matrix[i,j] = 1
        # eTKP
        # data_matrix[i,j] = sum(temp_mat$Klicks[temp_mat$Werbeflaeche==spred[j]])/sum(temp_mat$Views[temp_mat$Werbeflaeche==spred[j]]) * cpc[i] * 1000
        # CTR
        data_matrix[i,j] = sum(temp_mat$Klicks[temp_mat$Werbeflaeche==spred[j]])/sum(temp_mat$Views[temp_mat$Werbeflaeche==spred[j]])  
      }
    }
  }
}
  
# Write csv-file for recommender input
if (1){
  
  print("Write Matrices")
  require("MASS")
  write.matrix(data_index_matrix,"index.matrix")
  write.matrix(data_matrix,"recom.matrix")
  
} 

