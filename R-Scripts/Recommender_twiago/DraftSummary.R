# Summary for AdSpaces

# LOAD and EXTRACT
# october_start contains the data from 1.-10. October 2014
# daily data comes as report.csv

# Load packages
require("R.matlab")
require("MASS")
options(scipen=100,digits=6)

### Given data, precomputed - be careful: _names are not unique 
# ads:        final_ad_names
# spaces:     final_adspace_names
# nof ads:    l_ads
# nof spaces: l_spaces

# read data: tw01 = read.csv(...)

### Loop over ads and spaces and accumulate views and clicks, prepare ctr and index

# Prepare matrices
if (0) {
   
view_matrix <- matrix(0, nrow = l_spaces, ncol = l_ads)
klick_matrix <- matrix(0, nrow = l_spaces, ncol = l_ads)
ctr_matrix <- matrix(0, nrow = l_spaces, ncol = l_ads)
index_matrix <- matrix(0, nrow = l_spaces, ncol = l_ads)

}

# read data
if (0) {
  
  # read header first
  file_name <- "report_1510_neu.csv"
  cnames_raw <- read.csv(file_name,nrow=1, header=FALSE,colClasses="character")  
  cnames <- sub("\xe4","ae",cnames_raw)
  # read only the data, skip header
  twdf <- read.csv(file_name,header=FALSE,skip=1)
  colnames(twdf) = cnames 

}

# Aggregate views+klicks
if (0) {
  
nonex = 0
for (i in 1:l_spaces) {
  for (j in 1:l_ads){
    temp_views = sum(twdf$Views[twdf$Werbemittel==final_ad_names[j] & twdf$Werbeflaeche==final_adspace_names[i]])
    # Check: Entry nonexistent
    if (sum(temp_views)==0) {
      nonex = nonex + 1
    }
    # Entry existent:
    else {
      # Take care of non-uniqueness of names
      temp_klicks = sum(twdf$Klicks[twdf$Werbemittel==final_ad_names[j] & twdf$Werbeflaeche==final_adspace_names[i]])
      view_matrix[i,j] = view_matrix[i,j] + temp_views
      klick_matrix[i,j] = klick_matrix[i,j] + temp_klicks  
    } # end if else 
  } # end for j
} # end for i

}

# Compute CTR and Index Matrices
if (0) {
   index_matrix = (klick_matrix>0)+0
   for (i in 1:l_spaces) {
     for (j in 1:l_ads){
       if (index_matrix[i,j]>0){
         ctr_matrix[i,j] = klick_matrix[i,j] / view_matrix[i,j]                 
       }
     } # end for j
   } # end for i
   
 } # end compute ctr and index

# write matrices to files
if (1) {
  write.matrix(index_matrix,"index.matrix")
  write.matrix(ctr_matrix,"recom.matrix")
}

# rest of workflow
if (0) {
  
# read data
if (0) {

  # read header first
  cnames_raw <- read.csv("report_october_old.csv",nrow=1, header=FALSE,colClasses="character")  
  #cnames_raw <- read.csv("report.csv",nrow=1, header=FALSE,colClasses="character")
  cnames <- sub("\xe4","ae",cnames_raw)
  # read only the data, skip header
  october_start <- read.csv("report_october_old.csv",header=FALSE,skip=1)
  #october_start <- read.csv("report.csv",header=FALSE,skip=1)
  #jvm_data <- jvm_data[,1:length(cnames)]
  colnames(october_start) = cnames 
}

# compute summaries
if (0) {
  
tw_uni_space <- unique(october_start$Werbeflaeche)
nof_tw_us <- length(tw_uni_space)
tw_us_summary <- data.frame(matrix(0,nrow=nof_tw_us,ncol=4))
tw_us_summary[,1] <- tw_uni_space
colnames(tw_us_summary) <- c("Werbeflaeche","Views","Klicks","CTR")
for (i in 1:nof_tw_us) {
  tw_us_summary[i,2:3] <- colSums(october_start[october_start$Werbeflaeche==tw_uni_space[i],c(3,4)])
}
tw_us_summary[,4] <- tw_us_summary[,3]/tw_us_summary[,2]

# Sort by Views
tw_us_summary <- tw_us_summary[order(tw_us_summary[,2],decreasing=TRUE),]

# Extract relevant adspace names for october
#rel_adspace_summary <- tw_us_summary[tw_us_summary$Views>999 & tw_us_summary$Klick>0,]
rel_adspace_summary <- tw_us_summary[tw_us_summary$Views>9999 & tw_us_summary$Klick>9,]
rel_adspace_name <- rel_adspace_summary$Werbeflaeche[-1]

rel_october_raw <- october_start[is.element(october_start$Werbeflaeche,rel_adspace_name),]
rel_october <- rel_october_raw[rel_october_raw$Views>5000,]
rel_ad_name <- unique(rel_october$Werbemittel)

### Finally, we have
### rel_adspace_name: list of relevant adspaces
### rel_ad_name: list of relevant adspaces for prediction
### rel_adspace_sum_final: overall Views/Klick/CTR, sorted by Views

# ads: only campaigns that run until end of October
# Irrelevant ad-ids (from campaigns that end during october or are already inactive)
# Filter these out
irr_ad_ids <- c(2926,2996,2756:2761,2768:2773,3032:3033,3036,3043,3124:3126)
irr_ad_names <- c("Strammer Max","Strammer Max (Kopie)","Peter M Text 1","Peter M Text 2")

### Finally, we have

### final_ad_name: list of relevant adspaces for prediction
final_ad_names <- as.character(rel_ad_name[!is.element(rel_ad_name,irr_ad_names)])
l_ads <- length(final_ad_names)

### final_adspace_name: list of relevant adspaces
final_adspace_names <- as.character(rel_adspace_name)
l_spaces <- length(final_adspace_names)

### final_adspace_summary: overall Views/Klick/CTR, sorted by Views
final_adspace_summary <- rel_adspace_summary[is.element(rel_adspace_summary$Werbeflaeche,final_adspace_names),]

} # end summaries

# compute start and daily data
if (0) {
  # replace "report.csv" by daily report - my guess would be that accumulated data is more robust than daily data
  # nevertheless: we should compare daily and average performance to be sure that nothing goes wrong
  # read header first
  twnames_raw <- read.csv("report.csv",nrow=1, header=FALSE,colClasses="character")
  twnames <- sub("\xe4","ae",twnames_raw)
  twnames[5]="CPC"
  twnames <- sub("-",".",twnames)
  # read only the data, skip header
  tw01 <- read.csv("report.csv",header=FALSE,skip=1)
  #jvm_data <- jvm_data[,1:length(cnames)]
  colnames(tw01) = twnames 
} # end compute data

# combine start and daily data
if (0) {
  
# figure out the details - daily update - Views and Klicks
rec_dat_mat <- matrix(0,nrow=l_spaces,ncol=l_ads)
views_dat_mat <- matrix(0,nrow=l_spaces,ncol=l_ads)
klicks_dat_mat <- matrix(0,nrow=l_spaces,ncol=l_ads)
rec_ind_mat <- matrix(0,nrow=l_spaces,ncol=l_ads)
vcount_1000 = 0
nonex = 0
for (i in 1:l_spaces) {
  for (j in 1:l_ads){
    temp_views = tw01[tw01$Werbemittel==final_ad_names[j] & tw01$Werbeflaeche==final_adspace_names[i],3]
    # Check: Entry nonexistent
    if (length(temp_views)==0) {
      nonex = nonex + 1
    }
    # Check: Existent and Enough Views
    else if (sum(temp_views)>1000){
      vcount_1000 = vcount_1000 + 1
      temp_klicks = sum(tw01[tw01$Werbemittel==final_ad_names[j] & tw01$Werbeflaeche==final_adspace_names[i],4])
      rec_dat_mat[i,j] = temp_klicks / sum(temp_views)
      views_dat_mat[i,j] = views_dat_mat[i,j] + sum(temp_views)
      klicks_dat_mat[i,j] = klicks_dat_mat[i,j] + temp_klicks
      rec_ind_mat[i,j] = 1
    }
  }
}

# figure out the details - october start
start_dat_mat <- matrix(0,nrow=l_spaces,ncol=l_ads)
start_ind_mat <- matrix(0,nrow=l_spaces,ncol=l_ads)
svcount_1000 = 0
snonex = 0
for (i in 1:l_spaces) {
  for (j in 1:l_ads){
    temp_views = october_start[october_start$Werbemittel==final_ad_names[j] & october_start$Werbeflaeche==final_adspace_names[i],3]
    # Check: Entry nonexistent
    if (length(temp_views)==0) {
      snonex = snonex + 1
    }
    # Check: Enough Views
    else if (sum(temp_views)>1000){
      svcount_1000 = svcount_1000 + 1
      temp_klicks = sum(october_start[october_start$Werbemittel==final_ad_names[j] & october_start$Werbeflaeche==final_adspace_names[i],4])
      start_dat_mat[i,j] = temp_klicks / sum(temp_views)
      views_dat_mat[i,j] = views_dat_mat[i,j] + sum(temp_views)
      klicks_dat_mat[i,j] = klicks_dat_mat[i,j] + temp_klicks
      start_ind_mat[i,j] = 1
    }
  }
}

final_rec_dat_mat <- matrix(0,nrow=l_spaces,ncol=l_ads)
final_rec_ind_mat <- (start_ind_mat | rec_ind_mat)+0
final_rec_dat_mat[final_rec_ind_mat] = klicks_dat_mat[final_rec_ind_mat] / views_dat_mat[final_rec_ind_mat] 

} # end combine data

# write data to files
write.table(final_adspace_names,file="adspace_ids.txt",sep=" ")
write.matrix(final_rec_ind_mat+0,"index.matrix")
write.matrix(final_rec_dat_mat,"recom.matrix")

}