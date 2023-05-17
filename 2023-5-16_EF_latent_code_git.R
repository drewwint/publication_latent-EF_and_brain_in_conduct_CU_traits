###############################################################################
#
#         Latent EF in relation to CU and conduct
#
#     Code by: Drew E. Winters, PhD. 
#
#     Pregistration: https://osf.io/ha5re
#
###############################################################################






library(lavaan) ## SEM
library(dplyr) ## data manipulation
library(gimme) ## gimme networks 
library(interactions) ## to examine simple slopes 
library(VIM)# missing data pattern viewing
library(MissMech) # for  MCAR test
library(psych) # descriptive stats
library(corrplot) # correlation plot
library(ggplot2) # plotting
library(ggpubr) # combining plots ino one



#### BX data set up ####

# getting the EF data
CWIT <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_Color_Word_Interference_20180929.csv", skip = 1)
CWIT_names <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_Color_Word_Interference_20180929.csv", header = FALSE)[1:2,]
t(data.frame(CWIT_names))


tower <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_TOWER_20180929.csv", skip = 1)
tower_names <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_TOWER_20180929.csv", header = FALSE)[1:2,]
t(data.frame(tower_names))


DFT <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_Design_Fluency_20180929.csv",skip = 1)
DFT_names <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_Design_Fluency_20180929.csv", header = FALSE)[1:2,]
t(data.frame(DFT_names))


sort <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_Sorting_20180929.csv",skip= 1)
sort_names <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_Sorting_20180929.csv", header = FALSE)[1:2,]
t(data.frame(sort_names))


VFT <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_Verbal_Fluency_20180929.csv",skip= 1)
VFT_names <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_Verbal_Fluency_20180929.csv", header = FALSE)[1:2,]
t(data.frame(VFT_names))


TMT <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_Trails_20180929.csv",skip= 1)
TMT_names <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_DKEFS_Trails_20180929.csv", header = FALSE)[1:2,]
t(data.frame(TMT_names))


WASI <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_WASI-II_20180929.csv",skip= 1)
WASI_names <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_WASI-II_20180929.csv", header = FALSE)[1:2,]
t(data.frame(WASI_names))



# extracting needed itmes for anlaysis
tower_df <- tower[,c(1,5,52)]
colnames(tower_df) <- c("ID", "visit", "tower_total")

CWIT_df <- CWIT[,c(1,5,13,16, 7, 10)]
colnames(CWIT_df) <- c("ID", "visit", "CWIT_inhibit", "CWIT_inhib_swit", "DKEFSCWI_01", "DKEFSCWI_04")

TMT_df <- TMT[,c(1,5,27, 8, 9)]
colnames(TMT_df) <- c("ID", "visit", "TMT_switch", "DKEFSTMT_02", "DKEFSTMT_03")


DFT_df <- DFT[,c(1,5,21, 25, 23)]
colnames(DFT_df) <- c("ID", "visit", "DFT_switch", "DFT_fill_empt", "DF_17")


sort_df <- sort[,c(1,5,48)]
colnames(sort_df) <- c("ID", "visit", "sort_sorts")


VFT_df <- VFT[,c(1,5,35, 37)]
colnames(VFT_df) <- c("ID", "visit", "VFT_letter", "VFT_category")


WASI_df <- WASI[,c(1,5,9)]
colnames(WASI_df) <- c("ID", "visit", "INT_03")


# joining ef data
ef_df <- left_join(tower_df, CWIT_df, by = c("ID", "visit"))
ef_df <- left_join(ef_df, TMT_df, by = c("ID", "visit"))
ef_df <- left_join(ef_df, DFT_df, by = c("ID", "visit"))
ef_df <- left_join(ef_df, sort_df, by = c("ID", "visit"))
ef_df <- left_join(ef_df, VFT_df, by = c("ID", "visit"))
ef_df <- left_join(ef_df, WASI_df, by = c("ID", "visit"))

ef_df$visit <- car::recode(ef_df$visit, "'V1'='V2'")



# DL brain data
df <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\2_4_19 newest data r code\\2019_5_24_imaging_cases_112_FINAL.csv")


# DL ICU data  
ICUY <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\Assessments_rklnd_SEM\\8100_ICUY_20180929.csv",skip=1)
names(ICUY)[names(ICUY)=="VISIT"] <- "visit"
ICUY$visit <- car::recode(ICUY$visit, "'V1'='V2'")


# joining all dataframes 
df <- left_join(df, ICUY, by = c("ID", "visit"))
df <- left_join(df, ef_df, by = c("ID", "visit"))


# Testing accuracy of data joining
NROW(df) == 112 ## FALSE - need to investigate
## investigating
df[which(duplicated(df$ID)==TRUE),]
# only one participant - so we will remove
df <- df[-which(duplicated(df$ID)==TRUE),]

NROW(df) == 112 ## TRUE




# residualizing the necessary EFs
  # residualized scores: https://doi.org/10.1093/arclin/acy043 
  #> To calculate a residualized difference score, 
    #> an ordinary least squares regression was conducted, 
      #> where the control condition (e.g., combined Word Reading and Color Naming performance) 
      #> predicts the condition with higher cognitive demands (e.g., the Inhibition condition). 
      #> Through this equation, the residual value (i.e., error) is saved and used as a residualized difference score. 
    #> The **two** Color-Word Interference scores were made orthogonal to the summed performance on the Word Reading and Color Naming trials.
    #> Trail Making Number-Letter Switching score was made orthogonal to the summed performance on the Number and Letter Sequencing trials.
    #> The Design Fluency Switching score that loaded on the shifting factor was made orthogonal to the Design Fluency score that loaded on the fluency factor.
    #> the *two* Verbal Fluency scores were made orthogonal to the WASI Vocabulary subtest, controlling for the impact of language functioning on these outcomes.

df <- left_join(df, 
                data.frame("ID" = df$ID[as.numeric(names(resid(lm(df$CWIT_inhibit ~ (df$DKEFSCWI_01 + df$DKEFSCWI_04)))))], 
                           "CWIT_inhibit_r"=  resid(lm(df$CWIT_inhibit ~ (df$DKEFSCWI_01 + df$DKEFSCWI_04)))), by = "ID")

df <- left_join(df, 
                data.frame("ID" = df$ID[as.numeric(names(resid(lm(df$CWIT_inhib_swit ~ (df$DKEFSCWI_01 + df$DKEFSCWI_04)))))], 
                           "CWIT_inhib_swit_r"=  resid(lm(df$CWIT_inhib_swit ~ (df$DKEFSCWI_01 + df$DKEFSCWI_04)))), by = "ID")

df <- left_join(df, 
                data.frame("ID" = df$ID[as.numeric(names(resid(lm(df$TMT_switch ~ (df$DKEFSTMT_02 + df$DKEFSTMT_03)))))], 
                           "TMT_switch_r"= resid(lm(df$TMT_switch ~ (df$DKEFSTMT_02 + df$DKEFSTMT_03)))), by = "ID")

df <- left_join(df, 
                data.frame("ID" = df$ID[as.numeric(names(resid(lm(df$DFT_switch ~ (df$DFT_fill_empt)))))], 
                           "DFT_switch_r"= resid(lm(df$DFT_switch ~ (df$DFT_fill_empt)))), by = "ID")


df <- left_join(df, 
                data.frame("ID" = df$ID[as.numeric(names(resid(lm(df$VFT_letter ~ (df$INT_03)))))], 
                           "VFT_letter_r"= resid(lm(df$VFT_letter ~ (df$INT_03)))), by = "ID")



df <- left_join(df, 
                data.frame("ID" = df$ID[as.numeric(names(resid(lm(df$VFT_category ~ (df$INT_03)))))], 
                           "VFT_category_r"= resid(lm(df$VFT_category ~ (df$INT_03)))), by = "ID")



#### GIMME Set Up ####
# Formatting data
  # The steps here are
    # 1) import data files
    # 2) write the new data files with only the columns we may want to use (the only ones with names)
    # 3) downloading the name file & rename properly, importing the new data files, naming and saving only the columns I want 
    # 4) uploading each dataframe into one list for analysis 

# Setting up timeseries
# Step 1

data_files <- list.files("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\Subj_timeseries_denoised\\", pattern="^ROI")


for (i in 1:length(data_files)){
  assign(paste0("data",i),
         read.csv(paste0("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\Subj_timeseries_denoised\\",
                         data_files[i]),check.names = FALSE, header=FALSE))
}


# Step 2

pref<-"data"
suf<-seq(1:86)
data_names<-paste(pref,suf,sep="")

for(i in 1:length(data_names)){
  write.table(get(data_names[i])[,c(1:171)],
              paste0("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\Subj_timeseries_denoised\\",
                     data_names[i],
                     ".csv"),
              row.names=FALSE, col.names = FALSE,sep=",")
}


# Step 3

data_files <- list.files("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\Subj_timeseries_denoised\\", pattern="^data")
names<-read.csv("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\roi_names_coords\\ROInames.csv", check.names = FALSE, header=FALSE)
names <- as.character(names)

names[c(12:15,158,160,147,136,161)] <- c("IFC_Ra",
                                         "IFC_La",
                                         "IFC_Rb",
                                         "IFC_Lb",
                                         "DLPFC_R",
                                         "DLPFC_L",
                                         "ACC",
                                         "MPFC",
                                         "PPC_R")


for (i in 1:length(data_files)){
  assign(paste0("data",i),
         read.csv(paste0("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\Subj_timeseries_denoised\\",
                         data_files[i]),check.names = FALSE, header=FALSE, col.names = paste0(names))[,c(12:15,158,160,147,136,161)])
  
}  


# Step 4 - making a list of dataframes for analysis 

dat<-NULL

dat <- mget(paste0("data", 1:86))

str(dat)
class(dat)
names(dat)


## running gimme
fit_gm <- gimmeSEM(data= dat, out = "C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\gimme_out2",
                   subgroup = TRUE, paths = NULL, conv_interval = 1.4, standardize = TRUE)



# making sure file output for gimme is properly ordered
  # to do this we have to add a 0 to the beginning of files with single digits
  ## This was my error because I renamed my files with the assign data function above
    ## this fix ensure proper alignment of brain values with behavioral data. 

# base name
base_name <- "C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\gimme_out2\\individual\\"

# changing names for 1-9 inside folder
for(i in 1:9){
  file.rename(paste0(base_name,"data",i,"Betas.csv"),paste0(base_name,"data",0,i,"Betas.csv"))
}

# new list of properly ordered files
files <- list.files("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\gimme_out2\\individual",pattern = "Betas.csv$")

# initialzing new var and list
fit_gm <- rep(NA,length(files))
# initializing path estimates var
fit_gm$path_est_mats <- rep(NA,length(files))

# creating list of matricies in proper order
for (i in 1:length(files)){
  assign(paste0("est_mats",i),
         read.csv(paste0(base_name,files[i]))[,-1])
  
}  

# placing est matricies in new var
fit_gm$path_est_mats <- mget(paste0("est_mats", 1:length(files)))




#### Extracting GIMME Vars ####

# matricies to extract density 
stp = matrix(cbind(matrix(rep(1:7,14),ncol=1),matrix(rep(c(1:7,10:16),each=7),ncol=1)),ncol=2)
stp = stp[-c(8,16,24,32,40,48,  50,58,66,74,82,90,98),]


swt = matrix(cbind(matrix(rep(c(7,8),4),ncol=1),matrix(rep(c(c(7:8), c(16,17)),each=2),ncol=1)),ncol=2)
swt = swt[-c(1,3, 5,8),]


flu = matrix(cbind(matrix(rep(c(6,9),4),ncol=1),matrix(rep(c(c(6,9), c(16,18)),each=2),ncol=1)),ncol=2)
flu = flu[-c(3, 5,8),]




# inhibition network __________________________________________________________

## positive

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("stp_pos_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][stp],"0.0009:100=1;-100:-0.0009=-1")>0))
  #a <- mget(paste0("pos_ave", 1:length(fit_gm$path_est_mats)))
  #a <-rlist::list.ungroup(a)
  
  #stp_pos_den<-as.data.frame(cbind(a))
  #colnames(stp_pos_den)<-c("pos_all")
}

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("stp_pos_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][stp],"0.0009:100=1;-100:-0.0009=-1")>0))
  a <- mget(paste0("stp_pos_ave", 1:length(fit_gm$path_est_mats)))
  a <-rlist::list.ungroup(a)
  
  stp_pos_den<-as.data.frame(cbind(a))
  colnames(stp_pos_den)<-c("stp_pos_all")
}



## negative 

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("stp_neg_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][stp],"0.0009:100=1;-100:-0.0009=-1")<0))
  #b <- mget(paste0("neg_ave", 1:length(fit_gm$path_est_mats)))
  #b <-rlist::list.ungroup(b)
  
  #stp_neg_den<-as.data.frame(cbind(b))
  #colnames(stp_neg_den)<-c("neg_all")
}

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("stp_neg_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][stp],"0.0009:100=1;-100:-0.0009=-1")<0))
  b <- mget(paste0("stp_neg_ave", 1:length(fit_gm$path_est_mats)))
  b <-rlist::list.ungroup(b)
  
  stp_neg_den<-as.data.frame(cbind(b))
  colnames(stp_neg_den)<-c("stp_neg_all")
}



# switching network  ____________________________________________________________

## positive

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("swt_pos_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][swt],"0.0009:100=1;-100:-0.0009=-1")>0))
  #a <- mget(paste0("pos_ave", 1:length(fit_gm$path_est_mats)))
  #a <-rlist::list.ungroup(a)
  
  #swt_pos_den<-as.data.frame(cbind(a))
  #colnames(swt_pos_den)<-c("pos_all")
}

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("swt_pos_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][swt],"0.0009:100=1;-100:-0.0009=-1")>0))
  a <- mget(paste0("swt_pos_ave", 1:length(fit_gm$path_est_mats)))
  a <-rlist::list.ungroup(a)
  
  swt_pos_den<-as.data.frame(cbind(a))
  colnames(swt_pos_den)<-c("swt_pos_all")
}



## negative 

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("swt_neg_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][swt],"0.0009:100=1;-100:-0.0009=-1")<0))
  #b <- mget(paste0("neg_ave", 1:length(fit_gm$path_est_mats)))
  #b <-rlist::list.ungroup(b)
  
  #swt_neg_den<-as.data.frame(cbind(b))
  #colnames(swt_neg_den)<-c("neg_all")
}

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("swt_neg_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][swt],"0.0009:100=1;-100:-0.0009=-1")<0))
  b <- mget(paste0("swt_neg_ave", 1:length(fit_gm$path_est_mats)))
  b <-rlist::list.ungroup(b)
  
  swt_neg_den<-as.data.frame(cbind(b))
  colnames(swt_neg_den)<-c("swt_neg_all")
}



# fluency network ______________________________________________________________

## positive

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("flu_pos_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][flu],"0.0009:100=1;-100:-0.0009=-1")>0))
  #a <- mget(paste0("pos_ave", 1:length(fit_gm$path_est_mats)))
  #a <-rlist::list.ungroup(a)
  
  #flu_pos_den<-as.data.frame(cbind(a))
  #colnames(flu_pos_den)<-c("pos_all")
}

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("flu_pos_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][flu],"0.0009:100=1;-100:-0.0009=-1")>0))
  a <- mget(paste0("flu_pos_ave", 1:length(fit_gm$path_est_mats)))
  a <-rlist::list.ungroup(a)
  
  flu_pos_den<-as.data.frame(cbind(a))
  colnames(flu_pos_den)<-c("flu_pos_all")
}



## negative 

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("flu_neg_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][flu],"0.0009:100=1;-100:-0.0009=-1")<0))
  #b <- mget(paste0("neg_ave", 1:length(fit_gm$path_est_mats)))
  #b <-rlist::list.ungroup(b)
  
  #flu_neg_den<-as.data.frame(cbind(b))
  #colnames(flu_neg_den)<-c("neg_all")
}

for(i in 1:length(fit_gm$path_est_mats))
{
  assign(paste0("flu_neg_ave",i),
         sum(car::recode(fit_gm$path_est_mats[[i]][flu],"0.0009:100=1;-100:-0.0009=-1")<0))
  b <- mget(paste0("flu_neg_ave", 1:length(fit_gm$path_est_mats)))
  b <-rlist::list.ungroup(b)
  
  flu_neg_den<-as.data.frame(cbind(b))
  colnames(flu_neg_den)<-c("flu_neg_all")
}






#### Joining data ####


# getting IDs for imaging/GIMME data
rkl86 <- read.csv("D:\\IU Box Sync\\2 Dissertation & Qualifying exam\\Rockland Data\\Data_Rockland_SEM\\2_4_19 newest data r code\\2019_6_6_imaging_cases_86_FINAL.csv", 
                  header = TRUE)
any(duplicated(rkl86$ID))

# binding GIMME data
xyz<-cbind(stp_pos_den,stp_neg_den,swt_pos_den,swt_neg_den,flu_pos_den,flu_neg_den)
# joining IDs to gimme data
conn_df <- cbind(rkl86["ID"], xyz)

# testing for duplicates
any(duplicated(df$ID)) # none


# joining data for analysis 
full_df <- dplyr::left_join(df, conn_df, by = "ID")



## Also can just download data from here
full_df <- read.csv("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\full_df_FINAL.csv")[,-1]





### Descriptive and preliminary analysis

# Missing Data

library(VIM)# missing data pattern viewing
vars_miss <- c("CWIT_inhibit_r", 
               "CWIT_inhib_swit_r", 
               "tower_total", 
               "TMT_switch_r", 
               "DFT_switch_r", 
               "VFT_letter", 
               "VFT_category", 
               "DFT_fill_empt", 
               "stp_pos_all", 
               "ICUY_TOTAL", 
               "age", 
               "race", 
               "sex", 
               "tanner", 
               "YSR_EXTERNALIZING_RAW")

Rklnd_112_miss <- full_df[,vars_miss]

mice_plot <- aggr(Rklnd_112_miss, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Rklnd_112_miss), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


    ###> 74% have full data
    ###> looks like the only missing values are FC parameters (22%)
    ###> and EF measures missing is very low (2.6%)

## re-coding the missing values so we can look at patterns 
Rklnd_112_miss$missing<-car::Recode(Rklnd_112_miss$stp_pos_all, "NA=1;else=0") 
sum(Rklnd_112_miss$missing)## 26

sum(car::Recode(Rklnd_112_miss$CWIT_inhibit_r, "NA=1;else=0")) ## 4

    ## shows 26 participants are missing (22% missing ) is only FC parameters
    ## lets test for MCAR 


# MCAR Test
library(MissMech) # for  MCAR test
TestMCARNormality(Rklnd_112_miss[,-c(17)])
## p > 0.05 so there is not enough evidence to reject MCAR



# doing t tests and correlatives to see if there are any systematic reasons for missingness
# T test for missign values
lapply(Rklnd_112_miss[,c("age", 
                         "sex", 
                         "race", 
                         "tanner", 
                         "ICUY_TOTAL")], function(x) t.test(x ~ Rklnd_112_miss$missing)) # , var.equal = TRUE

# Correlations for missing values

lapply(Rklnd_112_miss[,c("age", 
                         "sex", 
                         "race", 
                         "tanner", 
                         "ICUY_TOTAL")], function(x) cor.test(x, Rklnd_112_miss$missing))






## Measures reliability 

  # CFA of EF measures

cfa_ef <-'
# EF model
inhibit =~ CWIT_inhibit_r + CWIT_inhib_swit_r + tower_total
shift =~ TMT_switch_r + DFT_switch_r + sort_sorts
fluency =~ VFT_letter + VFT_category + DFT_fill_empt
'

cfa_ef_cfa <- cfa(cfa_ef,
                  data= full_df, 
                  estimator = "ML", 
                  missing = "fiml")

fitmeasures(cfa_ef_cfa, 
            fit.measures = c('chisq', 
                             "df", 
                             "pvalue", 
                             "tli", 
                             "cfi", 
                             "rmsea", 
                             "rmsea.ci.lower", 
                             "rmsea.ci.upper", 
                             "srmr"), 
            output = "matrix")

parameterestimates(cfa_ef_cfa, 
                   standardize = TRUE, 
                   rsquare = TRUE)[c(1:9, 22:24),c(1:3,11,7)]


  ## Extracting values for latent factors
    # taking values from latent factors above and pooling them together
rr<- semTools::plausibleValues(cfa_ef_cfa)
  ## averaging sets for most plausible values
nn <- ((rr[[1]][,-1] +
          rr[[2]][,-1] +
          rr[[3]][,-1] +
          rr[[4]][,-1] +
          rr[[5]][,-1] +
          rr[[6]][,-1] +
          rr[[7]][,-1] +
          rr[[8]][,-1] +
          rr[[9]][,-1] +
          rr[[10]][,-1] +
          rr[[11]][,-1] +
          rr[[12]][,-1] +
          rr[[13]][,-1] +
          rr[[14]][,-1] +
          rr[[15]][,-1] +
          rr[[16]][,-1] +
          rr[[17]][,-1] +
          rr[[18]][,-1] +
          rr[[19]][,-1] +
          rr[[20]][,-1])/length(rr))

colnames(nn) <- c("inhibit_m","shift_m", "fluency_m")
nn
# placing it into the dataset
full_df <- cbind(full_df,nn)


  ## Omega (reliability) of this EF model
ef_om<-data.frame(t(psych::omega(full_df[,c("CWIT_inhibit_r", 
                                            "CWIT_inhib_swit_r", 
                                            "tower_total", 
                                            "TMT_switch_r", 
                                            "DFT_switch_r", 
                                            "VFT_letter", 
                                            "VFT_category", 
                                            "DFT_fill_empt")], 
                                 nfactors = 3, 
                                 plot = FALSE)$omega.group[c(1,3,4,2),1]))

names(ef_om) <- c("total", 
                  "inhibit", 
                  "shift", 
                  "fluency")

rownames(ef_om) <- "EF omega"
t(ef_om)

    #            EF omega
    # total    0.7925983
    # inhibit  0.7375274
    # shift    0.6949587
    # fluency  0.6682771

  ## OMEGA of CU
cu_om <- data.frame(t(psych::omega(full_df[,86:113], 
                                   nfactors = 3, 
                                   plot = FALSE)$omega.group[1,1]))

names(cu_om) <- c("total")
rownames(cu_om) <- "CU omega"
t(cu_om)

    #         CU omega
    # total 0.9296118


    ## OMEGA CD
cd_om <- data.frame(t(psych::omega(full_df[,45:47], 
                                   nfactors = 1,
                                   plot = FALSE)$omega.group[1,1]))
names(cd_om) <- c("total")
rownames(cd_om) <- "CD omega"
t(cd_om)

    #       CU omega
    # total 0.7489667






# Descriptives
round(as.data.frame(psych::describe(full_df[,c("age",
                                               "tanner", 
                                               "SES",
                                               "ICUY_TOTAL", 
                                               "inhibit_m", 
                                               "shift_m", 
                                               "fluency_m",
                                               "YSR_EXTERNALIZING_RAW" , 
                                               "stp_pos_all",  
                                               "flu_pos_all", 
                                               "swt_pos_all")]))[,c(3:4,8:10)],3)

table(cor.dat$race)
table(cor.dat$race)/112

table(cor.dat$sex)
table(cor.dat$sex)/112






# Correlations


cor.dat <- full_df[,c("ICUY_TOTAL", "YSR_EXTERNALIZING_RAW", 
                      "inhibit_m", "shift_m", "fluency_m",
                      "stp_pos_all", "swt_pos_all","flu_pos_all",   
                      "tanner","sex","race","SES")]

corrs = corr.test(cor.dat)
round(corrs$r,3)



part.cor <- psych::partial.r(full_df,c("ICUY_TOTAL",
                                       "YSR_EXTERNALIZING_RAW",
                                       "inhibit_m", 
                                       "shift_m", 
                                       "fluency_m", 
                                       "stp_pos_all",  
                                       "swt_pos_all",  
                                       "flu_pos_all"),
                             c("tanner",
                               "sex",
                               "race",
                               "SES"))


part.cor





## Interaction Terms

# inhibit
cu_stppos <- as.numeric(resid(lm(I(ICUY_TOTAL*stp_pos_all) ~ ICUY_TOTAL + stp_pos_all, data = full_df[is.na(full_df$stp_pos_all)==FALSE])))
full_df$cu_stppos <- NA  
full_df$cu_stppos[which(is.na(full_df$stp_pos_all)==FALSE)] <- cu_stppos

cd_stppos <- as.numeric(resid(lm(I(YSR_EXTERNALIZING_RAW*stp_pos_all) ~ YSR_EXTERNALIZING_RAW + stp_pos_all, data = full_df[is.na(full_df$stp_pos_all)==FALSE])))
full_df$cd_stppos <- NA  
full_df$cd_stppos[which(is.na(full_df$stp_pos_all)==FALSE)] <- cd_stppos

three_stppos <- as.numeric(resid(lm(I(ICUY_TOTAL*full_df$cd_stppos) ~ ICUY_TOTAL + YSR_EXTERNALIZING_RAW + stp_pos_all+ full_df$cu_stppos + full_df$cd_stppos, data = full_df[is.na(full_df$stp_pos_all)==FALSE])))
full_df$three_stppos <- NA
full_df$three_stppos[which(is.na(full_df$stp_pos_all)==FALSE)] <- three_stppos


# Shift
cu_shift <- as.numeric(resid(lm(I(ICUY_TOTAL*full_df$swt_pos_all) ~ ICUY_TOTAL + full_df$swt_pos_all, data = full_df[is.na(full_df$swt_pos_all)==FALSE])))
full_df$cu_shift <- NA
full_df$cu_shift[which(is.na(full_df$swt_pos_all)==FALSE)] <- cu_shift

cd_shift <- as.numeric(resid(lm(I(YSR_EXTERNALIZING_RAW*full_df$swt_pos_all) ~ YSR_EXTERNALIZING_RAW + full_df$swt_pos_all, data = full_df[is.na(full_df$swt_pos_all)==FALSE])))
full_df$cd_shift <- NA
full_df$cd_shift[which(is.na(full_df$swt_pos_all)==FALSE)] <- cd_shift


three_shift <- as.numeric(resid(lm(I(ICUY_TOTAL*full_df$cd_shift) ~ ICUY_TOTAL + YSR_EXTERNALIZING_RAW + full_df$swt_pos_all+ full_df$cu_shift + full_df$cd_shift, data = full_df[is.na(full_df$swt_pos_all)==FALSE])))
full_df$three_shift <- NA
full_df$three_shift[which(is.na(full_df$swt_pos_all)==FALSE)] <- three_shift


# Fluency
cu_flu <- as.numeric(resid(lm(I(ICUY_TOTAL*full_df$flu_pos_all) ~ ICUY_TOTAL + full_df$flu_pos_all, data = full_df[is.na(full_df$flu_pos_all)==FALSE])))
full_df$cu_flu <- NA
full_df$cu_flu[which(is.na(full_df$flu_pos_all)==FALSE)] <- cu_flu

cd_flu <- as.numeric(resid(lm(I(YSR_EXTERNALIZING_RAW*full_df$flu_pos_all) ~ YSR_EXTERNALIZING_RAW + full_df$flu_pos_all, data = full_df[is.na(full_df$flu_pos_all)==FALSE])))
full_df$cd_flu <- NA
full_df$cd_flu[which(is.na(full_df$flu_pos_all)==FALSE)] <- cd_flu


three_flu <- as.numeric(resid(lm(I(ICUY_TOTAL*full_df$cd_flu) ~ ICUY_TOTAL + YSR_EXTERNALIZING_RAW + full_df$flu_pos_all + full_df$cu_flu + full_df$cd_flu, data = full_df[is.na(full_df$flu_pos_all)==FALSE])))
full_df$three_flu <- NA
full_df$three_flu[which(is.na(full_df$flu_pos_all)==FALSE)] <- three_flu




  ## Just showing how these interaction terms are orthogonalized 

round(data.frame(corr.test(full_df[,c("cu_stppos", 
                                      "cd_stppos", 
                                      "three_stppos", 
                                      "cu_shift", 
                                      "cd_shift", 
                                      "three_shift",
                                      "cu_flu", 
                                      "cd_flu", 
                                      "three_flu",
                                      "ICUY_TOTAL", 
                                      "YSR_EXTERNALIZING_RAW" , 
                                      "inhibit_m", 
                                      "shift_m", 
                                      "fluency_m",
                                      # "ef_m",
                                      "stp_pos_all", 
                                      "swt_pos_all",  
                                      "flu_pos_all", 
                                      "sex",
                                      "tanner",
                                      "race_w",
                                      "SES")])$r)[1:9,10:22],3)



        #### Analyses ####

  ## NOTE - this initial just BX analysis is just to see if these estimates are the same as those in the moderation
inital <-'
# EF latent factors
inhibit =~ CWIT_inhibit_r + CWIT_inhib_swit_r + tower_total
shift =~ TMT_switch_r + DFT_switch_r + sort_sorts
fluency =~ VFT_letter + VFT_category + DFT_fill_empt


# regressions
  # bx data on EFs
inhibit ~ stp_pos_all + ICUY_TOTAL + YSR_EXTERNALIZING_RAW  + sex + tanner + race_w + SES
shift ~ swt_pos_all + ICUY_TOTAL + YSR_EXTERNALIZING_RAW + sex + tanner + race_w + SES
fluency ~ flu_pos_all + ICUY_TOTAL + YSR_EXTERNALIZING_RAW + sex + tanner + race_w + SES

'




inital_sem <- sem(inital,
                  data= full_df, 
                  estimator = "ML", 
                  missing = "fiml",
                  se="bootstrap",
                  bootstrap=5000)



mod_1 <-'
# EF latent factors
inhibit =~ CWIT_inhibit_r + CWIT_inhib_swit_r + tower_total
shift =~ TMT_switch_r + DFT_switch_r + sort_sorts
fluency =~ VFT_letter + VFT_category + DFT_fill_empt

tower_total ~~ CWIT_inhib_swit_r

# regressions
inhibit ~ a1*stp_pos_all + ICUY_TOTAL + YSR_EXTERNALIZING_RAW + sex + tanner + race_w + SES + b1*cu_stppos + b2*cd_stppos + b3*three_stppos
shift ~   swt_pos_all + ICUY_TOTAL + YSR_EXTERNALIZING_RAW + sex + tanner + race_w + SES + cu_shift + cd_shift + three_shift
fluency ~ flu_pos_all + ICUY_TOTAL + YSR_EXTERNALIZING_RAW + sex + tanner + race_w + SES + cu_flu + cd_flu + three_flu

inhb_low.CU := a1 + b1*((2.597) - 1)
inhb_mean.CU  := a1 + b1*(2.597)
inhb_hith.CU  := a1 + b1*((2.597) + 1)

inhb_low.CD := a1 + b2*((1.403) - 1)
inhb_mean.CD  := a1 + b2*(1.403)
inhb_high.Cd  := a1 + b2*((1.403) + 1)

'


mod_1_sem <- sem(mod_1,
                 data= full_df, 
                 estimator = "ML",
                 missing = "fiml",
                 se="bootstrap",
                 bootstrap=5000)

parameterestimates(mod_1_sem, 
                   standardize=TRUE, 
                   rsquare=TRUE, 
                   boot.ci.type = "bca.simple")[c(11:40,272:274),c(1:3,5:6,12,7:10)]

  # Simple Slopes
parameterestimates(mod_1_sem, 
                   standardize=TRUE, 
                   rsquare=TRUE, 
                   boot.ci.type = "bca.simple")[c(257:262),c(1:3,5:6,13,7:10)]

# p value correction 
  # by family
    # inhibition: 
cbind(parameterestimates(mod_1_sem, 
                         standardize=TRUE, 
                         rsquare=TRUE, 
                         boot.ci.type = "bca.simple")[c(11:20,257:262),c(1:3)],
      round(p.adjust(parameterestimates(mod_1_sem, 
                                        standardize=TRUE, 
                                        rsquare=TRUE, 
                                        boot.ci.type = "bca.simple")[c(11:20,257:262),c(8)],"BH"),3))

    ## Shifting
cbind(parameterestimates(mod_1_sem, 
                         standardize=TRUE, 
                         rsquare=TRUE, 
                         boot.ci.type = "bca.simple")[c(21:30),c(1:3)],
      round(p.adjust(parameterestimates(mod_1_sem, 
                                        standardize=TRUE, 
                                        rsquare=TRUE, 
                                        boot.ci.type = "bca.simple")[c(21:30),c(8)],"BH"),3))

    ## Fluency
cbind(parameterestimates(mod_1_sem, 
                         standardize=TRUE, 
                         rsquare=TRUE, 
                         boot.ci.type = "bca.simple")[c(31:40),c(1:3)],
      round(p.adjust(parameterestimates(mod_1_sem, 
                                        standardize=TRUE, 
                                        rsquare=TRUE, 
                                        boot.ci.type = "bca.simple")[c(31:40),c(8)],"BH"),3))


  # Standardized CI
standardizedsolution(mod_1_sem)[c(11:37,235:240),9:10]




# TEST to demonstrate just BX values are the same for bx model and mod model

cbind(parameterestimates(mod_1_sem, standardize=TRUE, rsquare=TRUE)[c(11:17,21:27,31:37),c(1:3)],
      (parameterestimates(mod_1_sem, 
                    standardize=TRUE,
                    rsquare=TRUE)[c(11:17,21:27,31:37),c(12)] - parameterestimates(inital_sem, 
                                                                                   standardize=TRUE, 
                                                                                   rsquare=TRUE)[c(10:30),c(11)]))

  # creating an dataframe with all relevant values. 
data.frame(parameterestimates(mod_1_sem, standardize=TRUE, rsquare=TRUE)[c(11:17,21:27,31:37),c(1:3)],
           "delta_std_beta"=round(parameterestimates(mod_1_sem, standardize=TRUE, rsquare=TRUE)[c(11:17,21:27,31:37),c(12)] - parameterestimates(inital_sem, standardize=TRUE, rsquare=TRUE)[c(10:30),c(11)],3),
           "substantial_change"=((parameterestimates(mod_1_sem, standardize=TRUE, rsquare=TRUE)[c(11:17,21:27,31:37),c(12)] - parameterestimates(inital_sem, standardize=TRUE, rsquare=TRUE)[c(10:30),c(11)])>0.10),
           "delta_un_std_beta"=round(parameterestimates(mod_1_sem, standardize=TRUE, rsquare=TRUE)[c(11:17,21:27,31:37),c(5)] - parameterestimates(inital_sem, standardize=TRUE, rsquare=TRUE)[c(10:30),c(4)],3),
           "delta_un_std_ci_low"=round(parameterestimates(mod_1_sem, standardize=TRUE, rsquare=TRUE, boot.ci.type = "bca.simple")[c(11:17,21:27,31:37),c(9)] - parameterestimates(inital_sem, standardize=TRUE, rsquare=TRUE, boot.ci.type = "bca.simple")[c(10:30),c(8)],3),
           "delta_un_std_ci_high"=round(parameterestimates(mod_1_sem, standardize=TRUE, rsquare=TRUE, boot.ci.type = "bca.simple")[c(11:17,21:27,31:37),c(10)] - parameterestimates(inital_sem, standardize=TRUE, rsquare=TRUE, boot.ci.type = "bca.simple")[c(10:30),c(9)],3),
           "delta_std_ci_low"=round(standardizedsolution(mod_1_sem)[c(11:17,21:27,31:37),c(9)] - standardizedsolution(inital_sem)[c(10:30),c(8)],3),
           "delta_dtd_ci_high"=round(standardizedsolution(mod_1_sem)[c(11:17,21:27,31:37),c(10)] - standardizedsolution(inital_sem)[c(10:30),c(9)],3))
  ## note every parameter is different to the 100th or 1000th datapoint 
    ## this is not significant enought to change results




  ## Post hoc power
    ## extracting degress of freedom for calculation 
df_m<-fitmeasures(mod_1_sem, 
            fit.measures = c("df"), 
            output = "matrix")[1]
df_m

    # Power for SEM model
# semPower::semPower.powerPlot.byN(.08, 'RMSEA', alpha = .05,  df = df_m)
      ## Power for model 
semPower::semPower.postHoc(.08, 'RMSEA', alpha = .05, N = 112, df = df_m)$power
        ## 99% power
semPower::semPower.aPriori(.08, 'RMSEA', alpha = .05, power = 0.8,  df = df_m)$requiredN.g
        ## required N = 51
          ### we had 112 so well within our power for the entire model 
        ## so we had enough n to determine if the sample properly represented the data with this model. 


    ## power for interactions 

int_val <- parameterestimates(mod_1_sem, 
                   standardize=TRUE, 
                   rsquare=TRUE, 
                   boot.ci.type = "bca.simple")[c(18:20,28:30,38:40),c(3,12)]

for(i in 1:length(int_val$std.all)){
  print(paste(int_val$rhs[i],"power = ", round(pwr::pwr.r.test(r=int_val$std.all[i],n=112,sig.level = 0.05)$power,3)))
}
    
    # "cu_stppos power =  0.946"
    # "cd_stppos power =  0.565"
    # "three_stppos power =  0.426"
    # "cu_shift power =  0.108"
    # "cd_shift power =  0.213"
    # "three_shift power =  0.05"
    # "cu_flu power =  0.65"
    # "cd_flu power =  0.062"
    # "three_flu power =  0.147"


r_s <- c(1:100)/100
for(i in r_s){
  print(paste(i, "power =", round(pwr::pwr.r.test(r=i,n=112,sig.level = 0.05)$power,3)))
}

  # we need an association of at least 0.26 to have power of 80%

pwr::plot.power.htest(pwr::pwr.r.test(r=0.26,n=112,sig.level = 0.05))






# exploring common latent EF factor____________________________________________


# latent EF 
l_ef <-"
# EF latent factors
inhibit =~ CWIT_inhibit_r + CWIT_inhib_swit_r + tower_total
shift =~ TMT_switch_r + DFT_switch_r + sort_sorts
fluency =~ VFT_letter + VFT_category + DFT_fill_empt
EF =~ inhibit + shift + fluency
"

l_ef_sem <- sem(l_ef,
                data= full_df, 
                estimator = "ML", 
                missing = "fiml",
                se="bootstrap",
                bootstrap=5000)

fitmeasures(l_ef_sem, 
            fit.measures = c('chisq', 
                             "df", 
                             "pvalue", 
                             "tli", 
                             "cfi", 
                             "rmsea", 
                             "rmsea.ci.lower", 
                             "rmsea.ci.upper", 
                             "srmr"), 
            output = "matrix")

  # making sure fit is exactly the same b/t 3-factor to common factor models
round(fitmeasures(cfa_ef_cfa, 
            fit.measures = c('chisq', 
                             "df", 
                             "pvalue", 
                             "tli", 
                             "cfi", 
                             "rmsea", 
                             "rmsea.ci.lower", 
                             "rmsea.ci.upper", 
                             "srmr"), 
            output = "matrix"),3) == round(fitmeasures(l_ef_sem, 
                                              fit.measures = c('chisq', 
                                                               "df", 
                                                               "pvalue", 
                                                               "tli", 
                                                               "cfi", 
                                                               "rmsea", 
                                                               "rmsea.ci.lower", 
                                                               "rmsea.ci.upper", 
                                                               "srmr"), 
                                              output = "matrix"),3)
    ## TRUE - same fit


parameterestimates(l_ef_sem,standardized = TRUE)[1:12,c(1:3,11,7)]

# extracting latent common ef values
rr<- semTools::plausibleValues(l_ef_sem)
  ## averaging sets for most plausible values
nn <- ((rr[[1]][,-1] +
          rr[[2]][,-1] +
          rr[[3]][,-1] +
          rr[[4]][,-1] +
          rr[[5]][,-1] +
          rr[[6]][,-1] +
          rr[[7]][,-1] +
          rr[[8]][,-1] +
          rr[[9]][,-1] +
          rr[[10]][,-1] +
          rr[[11]][,-1] +
          rr[[12]][,-1] +
          rr[[13]][,-1] +
          rr[[14]][,-1] +
          rr[[15]][,-1] +
          rr[[16]][,-1] +
          rr[[17]][,-1] +
          rr[[18]][,-1] +
          rr[[19]][,-1] +
          rr[[20]][,-1])/length(rr))

nn <- data.frame(lavPredict(l_ef_sem))
full_df$ef_m <- nn$EF





explore <-'
# EF latent factors
inhibit =~ CWIT_inhibit_r + CWIT_inhib_swit_r + tower_total
shift =~ TMT_switch_r + DFT_switch_r + sort_sorts
fluency =~ VFT_letter + VFT_category + DFT_fill_empt
EF =~ inhibit + shift + fluency


# regressions
  # bx data on common EF
EF ~ ICUY_TOTAL + YSR_EXTERNALIZING_RAW + sex + tanner + race_w + SES
EF ~ a1*stp_pos_all + b1*cu_stppos + b2*cd_stppos + b3*three_stppos
EF ~ a2*swt_pos_all + b11*cu_shift + b22*cd_shift + b33*three_shift
EF ~ a3*flu_pos_all + b111*cu_flu + b222*cd_flu + b333*three_flu

# Moderations
  # 3-way inhibit
inhb_low_CD.CU_low := a1 + b3*((1.403) - 1)*((2.597) - 1)
inhb_mean_CD.CU_low  := a1 + b3*(1.403)*((2.597) - 1)
inhb_high_CD.CU_low  := a1 + b3*((1.403) + 1)*((2.597) - 1)

inhb_low_CD.CU_mean := a1 + b3*((1.403) - 1)*(2.597)
inhb_mean_CD.CU_mean  := a1 + b3*(1.403)*(2.597)
inhb_high_CD.CU_mean  := a1 + b3*((1.403) + 1)*(2.597)

inhb_low_CD.CU_high := a1 + b3*((1.403) - 1)*((2.597) + 1)
inhb_mean_CD.CU_high  := a1 + b3*(1.403)*((2.597) + 1)
inhb_high_CD.CU_high  := a1 + b3*((1.403) + 1)*((2.597) + 1)


  #3-way Shift
shift_low_CD.CU_low := a1 + b33*((1.403) - 1)*((2.597) - 1)
shift_mean_CD.CU_low  := a1 + b33*(1.403)*((2.597) - 1)
shift_high_CD.CU_low  := a1 + b33*((1.403) + 1)*((2.597) - 1)

shift_low_CD.CU_mean := a1 + b33*((1.403) - 1)*(2.597)
shift_mean_CD.CU_mean  := a1 + b33*(1.403)*(2.597)
shift_high_CD.CU_mean  := a1 + b33*((1.403) + 1)*(2.597)

shift_low_CD.CU_high := a1 + b33*((1.403) - 1)*((2.597) + 1)
shift_mean_CD.CU_high  := a1 + b33*(1.403)*((2.597) + 1)
shift_high_CD.CU_high  := a1 + b33*((1.403) + 1)*((2.597) + 1)

  # Fluency net by CU
flu_SD.below.CU := a1 + b111*((2.597) - 1)
flu_mean.CU  := a1 + b111*(2.597)
flu_SD.above.CU  := a1 + b111*((2.597) + 1)

'



explore_sem <- sem(explore,
                   data= full_df, 
                   estimator = "ML", 
                   missing = "fiml",
                   se="bootstrap",
                   bootstrap=5000)
parameterestimates(explore_sem, 
                   standardize=TRUE, 
                   rsquare=TRUE, 
                   boot.ci.type = "bca.simple")[c(13:30, 279),c(1:3,5:6,12,7:10)]

  # For interaction terms
parameterestimates(explore_sem, 
                   standardize=TRUE, 
                   rsquare=TRUE, 
                   boot.ci.type = "bca.simple")[c(246:266),c(1:3,5:6,13,7:10)]

### fdr corrected p
cbind(parameterestimates(explore_sem, 
                         standardize=TRUE, 
                         rsquare=TRUE, 
                         boot.ci.type = "bca.simple")[c(13:30, 246:263),c(1:3)],
      round(p.adjust(parameterestimates(explore_sem, 
                                        standardize=TRUE, 
                                        rsquare=TRUE, 
                                        boot.ci.type = "bca.simple")[c(13:30, 246:266),c(8)],"fdr"),3))

cbind(parameterestimates(explore_sem, 
                         standardize=TRUE, 
                         rsquare=TRUE, 
                         boot.ci.type = "bca.simple")[c(13:30),c(1:3)],
      round(p.adjust(parameterestimates(explore_sem, 
                                        standardize=TRUE, 
                                        rsquare=TRUE, 
                                        boot.ci.type = "bca.simple")[c(13:30),c(8)],"fdr"),3))


cbind(parameterestimates(explore_sem, 
                         standardize=TRUE, 
                         rsquare=TRUE, 
                         boot.ci.type = "bca.simple")[c(246:266),c(1:3)],
      round(p.adjust(parameterestimates(explore_sem, 
                                        standardize=TRUE, 
                                        rsquare=TRUE, 
                                        boot.ci.type = "bca.simple")[c(246:266),c(8)],"fdr"),3))

## Standardized CI
standardizedsolution(explore_sem)[c(13:29, 226:246),9:10]



  ## power for interaction terms 
int_val <- parameterestimates(explore_sem, 
                              standardize=TRUE, 
                              rsquare=TRUE, 
                              boot.ci.type = "bca.simple")[c(19:21,23:25,27:29),c(3,12)]

for(i in 1:length(int_val$std.all)){
  print(paste(int_val$rhs[i],"power = ", round(pwr::pwr.r.test(r=int_val$std.all[i],n=112,sig.level = 0.05)$power,3)))
}

    # "stp_pos_all power =  0.495"
    # "cu_stppos power =    0.506"
    # "cd_stppos power =    0.419"
    # "swt_pos_all power =  0.137"
    # "cu_shift power =     0.214"
    # "cd_shift power =     0.051"
    # "flu_pos_all power =  0.071"
    # "cu_flu power =       0.989"
    # "cd_flu power =       0.246"




                          #### FIGURES ####
full_df_2<-full_df

# Extracting mean and sd values to simulate data for figures 

## Main model
  # inhibit - CU
intxicu<-lm(inhibit_m ~ stp_pos_all * ICUY_TOTAL + sex + tanner + YSR_EXTERNALIZING_RAW + race_w , data=full_df_2)
a <- interact_plot(intxicu,pred = stp_pos_all, modx = ICUY_TOTAL)
    # to get the stats to simulate my own figure
psych::describeBy(a$data, a$data$modx_group) 


  # inhibit - CD
intxicu<-lm(inhibit_m ~ stp_pos_all * YSR_EXTERNALIZING_RAW + sex + tanner + ICUY_TOTAL + race_w , data=full_df_2)
b <- interact_plot(intxicu,pred = stp_pos_all, modx = YSR_EXTERNALIZING_RAW)
    # to get the stats to simulate my own figure
psych::describeBy(b$data, b$data$modx_group) 



# Explore model

  # 3-way inhibit
intxicu<-lm(ef_m ~ stp_pos_all * YSR_EXTERNALIZING_RAW * ICUY_TOTAL + sex + tanner + race_w , data=full_df_2)
c <- interact_plot(intxicu,pred = stp_pos_all, modx = YSR_EXTERNALIZING_RAW,mod2 = ICUY_TOTAL)
    # to get the stats to simulate my own figure
psych::describeBy(c$data, list(c$data$modx_group,c$data$mod2_group)) 



  # 3-way shift
intxicu<-lm(ef_m ~ swt_pos_all * YSR_EXTERNALIZING_RAW * ICUY_TOTAL + sex + tanner + race_w , data=full_df_2)
d <- interact_plot(intxicu,pred = swt_pos_all, modx = YSR_EXTERNALIZING_RAW,mod2 = ICUY_TOTAL)
    # to get the stats to simulate my own figure
psych::describeBy(d$data, list(d$data$modx_group,d$data$mod2_group)) 


  # fluency - CU
intxicu<-lm(ef_m ~ flu_pos_all * ICUY_TOTAL + sex + tanner + race_w , data=full_df_2)
e <- interact_plot(intxicu,pred = flu_pos_all, modx = ICUY_TOTAL)
    # to get the stats to simulate my own figure
psych::describeBy(e$data, e$data$modx_group) 




                  #### FIGURES ####

## MAIN: Inhibit - CU__________________________________________________________

psych::describeBy(a$data, 
                  a$data$modx_group) 

  # brain var for all
set.seed(123)
brain = rnorm(100,mean(a$data$stp_pos_all),
              sd(a$data$stp_pos_all))



  ### association values (pulling from mod terms in model)
in_cu <-t(data.frame("inh_cu_vals" = parameterestimates(mod_1_sem, 
                                                        standardize=TRUE, 
                                                        rsquare=TRUE, 
                                                        boot.ci.type = "bca.simple")[c(257:259),c(13)]))
colnames(in_cu) <- c("low_cu",
                     "mean_cu",
                     "high_cu")
in_cu
in_cu[1]


#  + 1 SD on CU
  # Generating inhibition values based on coefficient with brain interaction with original inhibit mean and SD
set.seed(123)
inhibit = (in_cu[3]*brain) + 
  rnorm(100,mean(a$data[1:100,]$inhibit_m),
        sd(a$data[1:100,]$inhibit_m))
    # condition
cond = rep("+ 1 SD",100)
    # creating high dataframe
high = data.frame(cbind(inhibit, brain))
high$cond = cond


# mean CU
set.seed(123)
inhibit = (in_cu[2]*brain) + 
  rnorm(100,mean(a$data[101:200,]$inhibit_m),
        sd(a$data[101:200,]$inhibit_m))
cond = rep("mean",100)
mean = data.frame(cbind(inhibit, brain))
mean$cond = cond


# - 1 SD
set.seed(123)
inhibit = (in_cu[1]*brain) + 
  rnorm(100,mean(a$data[201:300,]$inhibit_m),
        sd(a$data[201:300,]$inhibit_m))
cond = rep("- 1 SD",100)
low = data.frame(cbind(inhibit, brain))
low$cond = cond



dd <- data.frame(rbind(high,mean,low))
dd$cond <- factor(dd$cond,
                  levels = c("+ 1 SD", 
                             "mean", 
                             "- 1 SD"))
dd$inhibit_s <- scale(dd$inhibit)


library(ggplot2)

ggplot(data = dd, aes(x= brain, y= inhibit_s, color = cond)) + 
  scale_x_continuous("Inhibition Network Positive Connection Density") +
  scale_y_continuous("Inhibition \n Z-scored") +
  labs(color = "Callous Level") +
  geom_smooth(method='lm', formula= y~x, se= FALSE, size=1.8) + 
  scale_color_grey() + 
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"), 
        legend.spacing.y = unit(0.0008, 'cm'),
        legend.position = c(0.815, 0.20)) 


# ggsave(path = "C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\Figures", width = 5, height = 3, filename = "CU_inhib-net_interact.tiff", device='tiff', dpi=700, limitsize = FALSE)






## MAIN: Inhibit CD______________________________________________________________
psych::describeBy(b$data, b$data$modx_group) 


# brain var for all
set.seed(123)
brain = rnorm(100,mean(b$data$stp_pos_all),sd(b$data$stp_pos_all))

in_cd <-t(data.frame("inh_cd_vals" = parameterestimates(mod_1_sem,
                                                        standardize=TRUE, 
                                                        rsquare=TRUE, 
                                                        boot.ci.type = "bca.simple")[c(260:262),c(13)])) # 5  12
colnames(in_cd) <- c("low_cd",
                     "mean_cd",
                     "high_cd")
in_cd
in_cd[1]


#  + 1 SD on CD

set.seed(123)
inhibit = (in_cd[3] * brain) + 
  rnorm(100,mean(b$data[1:100,]$inhibit_m),
        sd(b$data[1:100,]$inhibit_m))
cond = rep("+ 1 SD",100)
high = data.frame(cbind(inhibit, brain))
high$cond = cond


# mean CD
set.seed(123)
inhibit = (in_cd[2] * brain) + 
  rnorm(100,mean(b$data[101:200,]$inhibit_m),
        sd(b$data[101:200,]$inhibit_m))
cond = rep("mean",100)
mean = data.frame(cbind(inhibit, brain))
mean$cond = cond


# - 1 SD CD

set.seed(123)
inhibit = (in_cd[1] * brain) + 
  rnorm(100,mean(b$data[201:300,]$inhibit_m),
        sd(b$data[201:300,]$inhibit_m))
cond = rep("- 1 SD",100)
low = data.frame(cbind(inhibit, brain))
low$cond = cond



dd <- data.frame(rbind(high,mean,low))
dd$cond <- factor(dd$cond,
                  levels = c("+ 1 SD", 
                             "mean", 
                             "- 1 SD"))
dd$inhibit_s <- scale(dd$inhibit)



ggplot(data = dd, aes(x= brain, y= inhibit_s, color = cond)) + 
  scale_x_continuous("Inhibition Network Positive Connection Density") +
  scale_y_continuous("Inhibition \n Z-scored") +
  labs(color = "Conduct Level") +
  geom_smooth(method='lm', formula= y~x, se= FALSE, size = 1.8) +
  scale_color_grey() + 
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"), 
        legend.spacing.y = unit(0.0008, 'cm'),
        legend.position = c(0.82, 0.18)) 



# ggsave(path = "C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\Figures", width = 5, height = 3, filename = "CD_inhib-net_interact.tiff", device='tiff', dpi=700, limitsize = FALSE)






                          ##### Exploratory Figures ####

## inhibit network on EF three way______________________________________________
psych::describeBy(c$data, 
                  list(c$data$modx_group,
                       c$data$mod2_group)) 



# brain var for all - for all three figs
set.seed(123)
brain = rnorm(100,mean(c$data$stp_pos_all),
              sd(c$data$stp_pos_all))


### association values (pulling from mod terms in model)
cd1_cu_a <-t(data.frame("inh_cu_vals" = parameterestimates(explore_sem, 
                                                        standardize=TRUE, 
                                                        rsquare=TRUE, 
                                                        boot.ci.type = "bca.simple")[c(246:248),c(13)])) 
colnames(cd1_cu_a) <- c("cd_low_cu_low",
                        "cd_mean_cu_low",
                        "cd_high_cu_low")
cd1_cu_a
cd1_cu_a[1]


# -1 CD & -1CU
set.seed(123)
inhibit = (cd1_cu_a[3]*brain)+ 
  rnorm(100,mean(c$data[201:300,]$ef_m),
        sd(c$data[201:300,]$ef_m))
cond = rep("+ 1 SD",100)
high = data.frame(cbind(inhibit, brain))
high$cond = cond



# mean CU
set.seed(123)
inhibit = (cd1_cu_a[2]*brain)+ 
  rnorm(100,mean(c$data[101:200,]$ef_m),
        sd(c$data[101:200,]$ef_m))
cond = rep("mean",100)
mean = data.frame(cbind(inhibit, brain))
mean$cond = cond


# - 1 SD
set.seed(123)
inhibit = (cd1_cu_a[1]*brain)+ 
  rnorm(100,mean(c$data[1:100,]$ef_m),
        sd(c$data[1:100,]$ef_m))
cond = rep("- 1 SD",100)
low = data.frame(cbind(inhibit, brain))
low$cond = cond


dd1.1 <- data.frame(rbind(high,mean,low))
dd1.1$cond <- factor(dd1.1$cond,
                  levels = c("+ 1 SD", 
                             "mean", 
                             "- 1 SD"))
dd1.1$inhibit_s <- scale(dd1.1$inhibit)


ef_inhb_1.1 <- ggplot(data = dd1.1, aes(x= brain, y= inhibit_s, color = cond)) + 
  xlab("Inhibition Network - Positive Connection Density") +
  scale_y_continuous("Executive Function - Common Factor \n Z-scored") +
  labs(color = "Conduct Level", title = "-1 SD CU Traits") +
  geom_smooth(method='lm', formula= y~x, se= FALSE, size = 1.8) + 
  scale_color_grey() + 
  theme_minimal()+
  theme(legend.position = c(0.8, 0.22), 
        legend.spacing.y = unit(0.0008, 'cm'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face= "bold"),
        plot.title = element_text(hjust = 0.5, face= "bold")) 

ef_inhb_1.1



## 2
psych::describeBy(c$data, 
                  list(c$data$modx_group,
                       c$data$mod2_group)) 

cd1_cu_b <-t(data.frame("inh_cu_vals" = parameterestimates(explore_sem, 
                                                           standardize=TRUE, 
                                                           rsquare=TRUE, 
                                                           boot.ci.type = "bca.simple")[c(249:251),c(13)])) 
colnames(cd1_cu_b) <- c("cd_low_cu_mean","cd_mean_cu_mean","cd_high_cu_mean")
cd1_cu_b
cd1_cu_b[1]


# -1 CD & -1CU
set.seed(123)
inhibit = (cd1_cu_b[3]*brain)+ 
  rnorm(100,mean(c$data[501:600,]$ef_m),
        sd(c$data[501:600,]$ef_m))
cond = rep("+ 1 SD",100)
high = data.frame(cbind(inhibit, brain))
high$cond = cond



# mean CU
set.seed(123)
inhibit = (cd1_cu_b[2]*brain)+ 
  rnorm(100,mean(c$data[401:500,]$ef_m),
        sd(c$data[401:500,]$ef_m))
cond = rep("mean",100)
mean = data.frame(cbind(inhibit, brain))
mean$cond = cond


# - 1 SD
set.seed(123)
inhibit = (cd1_cu_b[1]*brain)+ 
  rnorm(100,mean(c$data[301:400,]$ef_m),
        sd(c$data[301:400,]$ef_m))
cond = rep("- 1 SD",100)
low = data.frame(cbind(inhibit, brain))
low$cond = cond



dd1.2 <- data.frame(rbind(high,mean,low))
dd1.2$cond <- factor(dd1.2$cond,
                     levels = c("+ 1 SD", 
                                "mean", 
                                "- 1 SD"))
dd1.2$inhibit_s <- scale(dd1.2$inhibit)



ef_inhb_1.2 <- ggplot(data = dd1.2, aes(x= brain, y= inhibit_s, color = cond)) + 
  xlab("Inhibition Network - Positive Connection Density") +
  scale_y_continuous("Common Factor of Executive Function \n Z-scored") +
  labs(color = "Conduct Level", title = "Mean CU Traits") +
  geom_smooth(method='lm', formula= y~x, se= FALSE, size = 1.8) + 
  scale_color_grey() + 
  theme_minimal()+
  theme(legend.spacing.y = unit(0.0008, 'cm'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face= "bold"),
        legend.position="none") 

ef_inhb_1.2


## 3

psych::describeBy(c$data, 
                  list(c$data$modx_group,
                       c$data$mod2_group)) 

cd1_cu_c <-t(data.frame("inh_cu_vals" = parameterestimates(explore_sem, 
                                                           standardize=TRUE, 
                                                           rsquare=TRUE, 
                                                           boot.ci.type = "bca.simple")[c(252:254),c(13)])) 
colnames(cd1_cu_c) <- c("cd_low_cu_high",
                        "cd_mean_cu_high",
                        "cd_high_cu_high")
cd1_cu_c
cd1_cu_c[1]




# -1 CD & -1CU
set.seed(123)
inhibit = (cd1_cu_c[3]*brain)+ 
  rnorm(100,mean(c$data[801:900,]$ef_m),
        sd(c$data[801:900,]$ef_m))
cond = rep("+ 1 SD",100)
high = data.frame(cbind(inhibit, brain))
high$cond = cond



# mean CU
set.seed(123)
inhibit = (cd1_cu_c[2]*brain)+ 
  rnorm(100,mean(c$data[701:800,]$ef_m),
        sd(c$data[701:800,]$ef_m))
cond = rep("mean",100)
mean = data.frame(cbind(inhibit, brain))
mean$cond = cond


# - 1 SD
set.seed(123)
inhibit = (cd1_cu_c[1]*brain)+ 
  rnorm(100,mean(c$data[601:700,]$ef_m),
        sd(c$data[601:700,]$ef_m))
cond = rep("- 1 SD",100)
low = data.frame(cbind(inhibit, brain))
low$cond = cond

# summary(lm(inhibit~brain,data=low))
# summary(lm(inhibit~brain,data=mean))
# summary(lm(inhibit~brain,data=high))


dd1.3 <- data.frame(rbind(high,mean,low))
dd1.3$cond <- factor(dd1.3$cond,
                     levels = c("+ 1 SD", 
                                "mean", 
                                "- 1 SD"))
dd1.3$inhibit_s <- scale(dd1.3$inhibit)



ef_inhb_1.3 <- ggplot(data = dd1.3, aes(x= brain, y= inhibit_s, color = cond)) + 
  xlab("Inhibition Network - Positive Connection Density") +
  scale_y_continuous("Common Factor of Executive Function \n Z-scored") +
  labs(color = "Conduct Level", title = "+1 SD CU Traits") +
  geom_smooth(method='lm', formula= y~x, se= FALSE, size = 1.8) + 
  scale_color_grey() + 
  theme_minimal()+
  theme(legend.spacing.y = unit(0.0008, 'cm'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face= "bold"),
        legend.position="none") 

ef_inhb_1.3


library(ggpubr)
vb <- ggarrange(ef_inhb_1.1,
                ef_inhb_1.2,
                ef_inhb_1.3, 
                ncol=3, 
                nrow=1, 
                common.legend = TRUE, 
                legend = "bottom")
annotate_figure(vb, 
                bottom = text_grob("Inhibition Network Positive Connection Density", 
                                   face = "bold"))

# ggsave(path = "C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\Figures", width = 6.5, height = 3.5, filename = "ef_3-way_cu-cd_inhib.tiff", device='tiff', dpi=700, limitsize = FALSE)






#EXPLORE: Shifting 3-way_____________________________________________________________________

psych::describeBy(c$data, 
                  list(d$data$modx_group,
                       d$data$mod2_group)) 

# brain var for all - for all three figs
set.seed(123)
brain = rnorm(100,
              mean(d$data$swt_pos_all)
              ,sd(d$data$swt_pos_all))


### association values (pulling from mod terms in model)
cd2_cu_a <-t(data.frame("inh_cu_vals" = parameterestimates(explore_sem, 
                                                           standardize=TRUE, 
                                                           rsquare=TRUE, 
                                                           boot.ci.type = "bca.simple")[c(255:257),c(13)])) 
colnames(cd2_cu_a) <- c("cd_low_cu_low",
                        "cd_mean_cu_low",
                        "cd_high_cu_low")
cd2_cu_a
cd2_cu_a[1]




# -1 CD & -1CU
set.seed(123)
inhibit = (cd2_cu_a[3]*brain)+ 
  rnorm(100,mean(d$data[201:300,]$ef_m),
        sd(d$data[201:300,]$ef_m))
cond = rep("+ 1 SD",100)
high = data.frame(cbind(inhibit, brain))
high$cond = cond



# mean CU
set.seed(123)
inhibit = (cd2_cu_a[2]*brain)+ 
  rnorm(100,mean(d$data[101:200,]$ef_m),
        sd(d$data[101:200,]$ef_m))
cond = rep("mean",100)
mean = data.frame(cbind(inhibit, brain))
mean$cond = cond


# - 1 SD
set.seed(123)
inhibit = (cd2_cu_a[1]*brain)+ 
  rnorm(100,mean(d$data[1:100,]$ef_m),
        sd(d$data[1:100,]$ef_m))
cond = rep("- 1 SD",100)
low = data.frame(cbind(inhibit, brain))
low$cond = cond

# summary(lm(inhibit~brain,data=low))
# summary(lm(inhibit~brain,data=mean))
# summary(lm(inhibit~brain,data=high))


dd2.1 <- data.frame(rbind(high,mean,low))
dd2.1$cond <- factor(dd2.1$cond,
                     levels = c("+ 1 SD", 
                                "mean", 
                                "- 1 SD"))
dd2.1$inhibit_s <- scale(dd2.1$inhibit)


ef_shift_1.1 <- ggplot(data = dd2.1, aes(x= brain, y= inhibit_s, color = cond)) + 
  xlab("Shifting Network - Positive Connection Density") +
  scale_y_continuous("Executive Function - Common Factor \n Z-scored") +
  labs(color = "Conduct Level", title = "-1 SD CU Traits") +
  geom_smooth(method='lm', formula= y~x, se= FALSE, size = 1.8) + 
  scale_color_grey() + 
  theme_minimal()+
  theme(legend.position = c(0.8, 0.22), 
        legend.spacing.y = unit(0.0008, 'cm'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face= "bold"),
        plot.title = element_text(hjust = 0.5, face= "bold")) 

ef_shift_1.1



## 2
psych::describeBy(c$data, list(d$data$modx_group,d$data$mod2_group)) 

cd2_cu_b <-t(data.frame("inh_cu_vals" = parameterestimates(explore_sem, 
                                                           standardize=TRUE, 
                                                           rsquare=TRUE, 
                                                           boot.ci.type = "bca.simple")[c(258:260),c(13)])) 
colnames(cd2_cu_b) <- c("cd_low_cu_mean",
                        "cd_mean_cu_mean",
                        "cd_high_cu_mean")
cd2_cu_b
cd2_cu_b[1]


# -1 CD & -1CU
set.seed(123)
inhibit = (cd2_cu_b[3]*brain)+ 
  rnorm(100,mean(d$data[501:600,]$ef_m),
        sd(d$data[501:600,]$ef_m))
cond = rep("+ 1 SD",100)
high = data.frame(cbind(inhibit, brain))
high$cond = cond



# mean CU
set.seed(123)
inhibit = (cd2_cu_b[2]*brain)+ 
  rnorm(100,mean(d$data[401:500,]$ef_m),
        sd(d$data[401:500,]$ef_m))
cond = rep("mean",100)
mean = data.frame(cbind(inhibit, brain))
mean$cond = cond


# - 1 SD
set.seed(123)
inhibit = (cd2_cu_b[1]*brain)+ 
  rnorm(100,
        mean(d$data[301:400,]$ef_m),
        sd(d$data[301:400,]$ef_m))
cond = rep("- 1 SD",100)
low = data.frame(cbind(inhibit, brain))
low$cond = cond


# summary(lm(inhibit~brain,data=low))
# summary(lm(inhibit~brain,data=mean))
# summary(lm(inhibit~brain,data=high))


dd2.2 <- data.frame(rbind(high,mean,low))
dd2.2$cond <- factor(dd2.2$cond,
                     levels = c("+ 1 SD", 
                                "mean", 
                                "- 1 SD"))
dd2.2$inhibit_s <- scale(dd2.2$inhibit)



ef_shift_1.2 <- ggplot(data = dd2.2, aes(x= brain, y= inhibit_s, color = cond)) + 
  xlab("Inhibition Network - Positive Connection Density") +
  scale_y_continuous("Common Factor of Executive Function \n Z-scored") +
  labs(color = "Conduct Level", title = "Mean CU Traits") +
  geom_smooth(method='lm', formula= y~x, se= FALSE, size = 1.8) + 
  scale_color_grey() + 
  theme_minimal()+
  theme(legend.spacing.y = unit(0.0008, 'cm'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face= "bold"),
        legend.position="none") 

ef_shift_1.2


## 3

psych::describeBy(c$data, 
                  list(d$data$modx_group,
                       d$data$mod2_group)) 

cd2_cu_c <-t(data.frame("inh_cu_vals" = parameterestimates(explore_sem, 
                                                           standardize=TRUE, 
                                                           rsquare=TRUE, 
                                                           boot.ci.type = "bca.simple")[c(261:263),c(13)])) 
colnames(cd2_cu_c) <- c("cd_low_cu_high",
                        "cd_mean_cu_high",
                        "cd_high_cu_high")
cd2_cu_c
cd2_cu_c[1]




# +1 CD & -1CU
set.seed(123)
inhibit = (cd2_cu_c[3]*brain) - 
  rnorm(100,mean(d$data[801:900,]$ef_m),
        sd(d$data[801:900,]$ef_m))
cond = rep("+ 1 SD",100)
high = data.frame(cbind(inhibit, brain))
high$cond = cond



# mean CU
set.seed(123)
inhibit = (cd2_cu_c[2]*brain)+ 
  rnorm(100,mean(d$data[701:800,]$ef_m),
        sd(d$data[701:800,]$ef_m))
cond = rep("mean",100)
mean = data.frame(cbind(inhibit, brain))
mean$cond = cond


# - 1 SD
set.seed(123)
inhibit = (cd2_cu_c[1]*brain) + 
  rnorm(100,mean(d$data[601:700,]$ef_m),
        sd(d$data[601:700,]$ef_m))
cond = rep("- 1 SD",100)
low = data.frame(cbind(inhibit, brain))
low$cond = cond


# summary(lm(inhibit~brain,data=low))
# summary(lm(inhibit~brain,data=mean))
# summary(lm(inhibit~brain,data=high))

dd2.3 <- data.frame(rbind(high,mean,low))
dd2.3$cond <- factor(dd2.3$cond,
                     levels = c("+ 1 SD",
                                "mean", 
                                "- 1 SD"))
dd2.3$inhibit_s <- scale(dd2.3$inhibit)



ef_shift_1.3 <- ggplot(data = dd2.3, aes(x= brain, y= inhibit_s, color = cond)) + 
  xlab("Inhibition Network - Positive Connection Density") +
  scale_y_continuous("Common Factor of Executive Function \n Z-scored") +
  labs(color = "Conduct Level", title = "+1 SD CU Traits") +
  geom_smooth(method='lm', formula= y~x, se= FALSE, size = 1.8) + 
  scale_color_grey() + 
  theme_minimal()+
  theme(legend.spacing.y = unit(0.0008, 'cm'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face= "bold"),
        legend.position="none") 

ef_shift_1.3


library(ggpubr)
vb <- ggarrange(ef_shift_1.1,ef_shift_1.2,ef_shift_1.3, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom")
annotate_figure(vb, bottom = text_grob("Shifting Network Positive Connection Density", face = "bold"))




# ggsave(path = "C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\Figures", width = 6.5, height = 3.5, filename = "ef_3-way_cu-cd_shift.tiff", device='tiff', dpi=700, limitsize = FALSE)




##EXPLORE: fluency on EF___________________________________________________

flus <-t(data.frame("inh_cu_vals" = parameterestimates(explore_sem, 
                                                           standardize=TRUE, 
                                                           rsquare=TRUE, 
                                                           boot.ci.type = "bca.simple")[c(264:266),c(13)])) 
colnames(flus) <- c("cu_low",
                    "cu_mean",
                    "cu_high")
flus
flus[1]


psych::describeBy(e$data, 
                  list(e$data$modx_group)) 

# brain var for all - for all three figs
set.seed(123)
brain = rnorm(100,
              mean(e$data$flu_pos_all ),
              sd(e$data$flu_pos_all ))



#  + 1 SD on CD
set.seed(123)
inhibit = ((flus[3] * brain) - 
             rnorm(100,mean(e$data[1:100,]$ef_m),
                   sd(e$data[1:100,]$ef_m)))
cond = rep("+ 1 SD",100)
high = data.frame(cbind(inhibit, brain))
high$cond = cond


# mean CD
set.seed(123)
inhibit = (flus[2] * brain) - 
  rnorm(100,mean(e$data[101:200,]$ef_m),
        sd(e$data[101:200,]$ef_m))
cond = rep("mean",100)
mean = data.frame(cbind(inhibit, brain))
mean$cond = cond


# - 1 SD CD

set.seed(123)
inhibit = (flus[1] * brain) - 
  rnorm(100,mean(e$data[201:300,]$ef_m),
        sd(e$data[201:300,]$ef_m))
cond = rep("- 1 SD",100)
low = data.frame(cbind(inhibit, brain))
low$cond = cond

# summary(lm(inhibit~brain,data=low))
# summary(lm(inhibit~brain,data=mean))
# summary(lm(inhibit~brain,data=high))

flu_ef <- data.frame(rbind(high,mean,low))
flu_ef$cond <- factor(flu_ef$cond,
                     levels = c("+ 1 SD", 
                                "mean", 
                                "- 1 SD"))
flu_ef$fluency_s <- scale(flu_ef$inhibit)



ggplot(data = flu_ef, aes(x= brain, y= fluency_s, color = cond)) + 
  scale_x_continuous("Fluency Network Positive Connection Density") +
  scale_y_continuous("Executive Function - Common Factor \n Z-scored") +
  labs(color = "Callous Level") +
  geom_smooth(method='lm', formula= y~x, se= FALSE, size = 1.8) +
  scale_color_grey() + 
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"), 
        legend.spacing.y = unit(0.0008, 'cm'),
        legend.position = c(0.9,0.8)) 



# ggsave(path = "C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\EF_latent\\Figures", width = 6.5, height = 3.5, filename = "ef_cu_fluency.tiff", device='tiff', dpi=700, limitsize = FALSE)











