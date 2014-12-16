# Summary for AdSpaces

# LOAD and EXTRACT
# october_start contains the data from 1.-10. October 2014 

options(scipen=100,digits=6)

if (1) {

  # read header first
  file_name = "report_october01-23rd.csv"
  cnames_raw <- read.csv(file_name,nrow=1, header=FALSE,colClasses="character")  
  #cnames_raw <- read.csv("report.csv",nrow=1, header=FALSE,colClasses="character")
  cnames <- sub("\xe4","ae",cnames_raw)
  # read only the data, skip header
  october_start <- read.csv(file_name,header=FALSE,skip=1)
  #october_start <- read.csv("report.csv",header=FALSE,skip=1)
  #jvm_data <- jvm_data[,1:length(cnames)]
  colnames(october_start) = cnames 
  #remove last line, if needed
  lastline <- length(october_start[,1])
  if (october_start$Werbeflaeche[lastline]=="[Summe]") october_start <- october_start[-lastline,]
}

tw_uni_space <- unique(october_start$Werbeflaeche)
nof_tw_us <- length(tw_uni_space)
tw_uni_ads <- unique(october_start$Werbemittel)
nof_tw_ads <- length(tw_uni_ads)

tw_us_summary <- data.frame(matrix(0,nrow=nof_tw_us,ncol=5))
tw_us_summary[,1] <- tw_uni_space
colnames(tw_us_summary) <- c("Werbeflaeche","Views","Klicks","CTR","Anzahl Ads")
for (i in 1:nof_tw_us) {
  tw_us_summary[i,2:3] <- colSums(october_start[october_start$Werbeflaeche==tw_uni_space[i],c(3,4)])
  tw_us_summary[i,5] <- length(october_start[october_start$Werbeflaeche==tw_uni_space[i],2])
}
tw_us_summary[,4] <- tw_us_summary[,3]/tw_us_summary[,2]

# Sort by Views
tw_us_summary <- tw_us_summary[order(tw_us_summary[,2],decreasing=TRUE),]

# Extract relevant adspace names for october
#rel_adspace_summary <- tw_us_summary[tw_us_summary$Views>999 & tw_us_summary$Klick>0,]
rel_adspace_summary <- tw_us_summary[tw_us_summary$Views>9999 & tw_us_summary$Klick>9,]
rel_adspace_name <- unique(rel_adspace_summary$Werbeflaeche)

rel_october_raw <- october_start[is.element(october_start$Werbeflaeche,rel_adspace_name),]
rel_october <- rel_october_raw[rel_october_raw$Views>1000,]

irr_ads_index <- is.element(rel_october$Werbemittel,irr_ad_names)
rel_october_red = rel_october[!irr_ads_index,]
rel_oct_ads <- unique(rel_october_red$Werbemittel) 
len_rel_octads <- length(rel_oct_ads) 
rel_oct_spaces <- unique(rel_october_red$Werbeflaeche) 
len_rel_octspaces <- length(rel_oct_spaces)

rel_adspace_name <- rel_oct_spaces
rel_ads_name <- rel_oct_ads
