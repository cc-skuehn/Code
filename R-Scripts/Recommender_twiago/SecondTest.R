# list of commands for some test preparations

# TODO Remove CPX/CPA campagin and inactive ads
# CPA/CPX: campaign names end on CPA/CPX
# inactive ads: check against adlist of the current day

# extracting all ads and all relevant adspaces
# START PREPARATION

# List of current campaigns and IDs, reduced by CPX campaigns
if (1) {
  # update file name
  camp_file ="campaigns_11.11.2014.csv"
  camp_list <- data.frame(read.csv(camp_file))
  camp_list <- camp_list[,c(1,6)]
  camp_list[,1] <- gsub("\xe4","ae",camp_list[,1])
  camp_list[,1] <- gsub("\xe9","e",camp_list[,1])
  camp_list[,1] <- gsub("\xd6","Oe",camp_list[,1])
  camp_list[,1] <- gsub("\xf6","oe",camp_list[,1])
  camp_list[,1] <- gsub("\xfc","ue",camp_list[,1])
  # alternatively, remove everything with \x
  #camp_list[,1] <- gsub("\x","_",camp_list[,1])
  
  # remove CPX campaigns
  cpx_ind <- which(grepl("CPX",camp_list[,1]))
  # use this for filterung out cpx campaigns -> Kampagnen.ID
  cpx_camp <- camp_list[cpx_ind,]
  camp_list = camp_list[-cpx_ind,]
  # remove last 2 lines (Summe und Gelöscht)
  lc <- length(camp_list[,1])
  camp_list = camp_list[-((lc-1):lc),]
}


if (1) {
  
#file_name = "14Tagereport.csv"
#file_name = "report14TageCPC.csv"
file_name = "14tage_11.11.2014.report.csv"
vztage <- read.csv(file_name, header=F, skip=1)
cn <- c("Werbeflaeche","Kampagne","Werbemittel","Views","Klicks","Summe_Advertiser","CPC","Kampagnen.ID")
#cn_raw <- read.csv(file_name,nrow=1, header=FALSE,colClasses="character")
#cn <- sub("\xe4","ae",cn_raw)
colnames(vztage) <- cn
#colnames(vztage)[5:7] = c("CTR","TKP","CPC")

#remove last line, if needed
lastline <- length(vztage[,1])
if (vztage$Werbeflaeche[lastline]=="[Summe]") vztage <- vztage[-lastline,]
# remove Werbemittel [Unbekannt]
vztage <- vztage[-which(vztage$Werbemittel=="[Unbekannt]"),]
# remove Werbefläche 0
vztage <- vztage[-which(vztage$Werbeflaeche=="0"),]

# remove cpx campaings
vztage <- vztage[-which(is.element(vztage$Kampagnen.ID,cpx_camp$Kampagnen.ID)),]

# remove twiago masterkampagne
vztage = vztage[-which(vztage$Kampagnen.ID==4),]

#print(tail(vztage))

# extract ads and spaces
all_ads <- unique(vztage$Werbemittel)
all_spaces <- unique(vztage$Werbeflaeche)

nonzero_ads <- unique(vztage$Werbemittel[vztage$Klicks>0])
lna = length(nonzero_ads)

# remove corrupt data
cor_index <- which(vztage$Views<=vztage$Klicks)
if (sum(cor_index>0)) vztage <- vztage[-cor_index,]

#print(tail(vztage))

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

vzred <- vztage[vztage$Views>=1000 & vztage$Klicks>0,]
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
        data_matrix[i,j] = sum(temp_mat$Klicks[temp_mat$Werbeflaeche==spred[j]])/sum(temp_mat$Views[temp_mat$Werbeflaeche==spred[j]]) * cpc[i] * 1000
      }
    }
  }
}
  
# Write csv-file for recommender input
if (1){

  write.matrix(data_index_matrix,"index.matrix")
  write.matrix(data_matrix,"recom.matrix")
  
} 

