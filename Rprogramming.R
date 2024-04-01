

#load all the packages
library('NeuroBlu')
library('dplyr')
library('plyr')
library('tidyr')
library('lubridate')
library('ggpubr')
library('stringr')
library('tableone')

################TOTAL ADHD#################
patients = getCohort("ADHD")
length(patients)

##############USING RSAV FOR EXECUTIVE FUNCTIONING####################
ontology= data.frame(rbind(
    c("rsav_26_exec_function_0", 0, "Normal",0)
    ,c("rsav_26_exec_function_0", 1, "Some impairment",1)
    ,c("rsav_26_exec_function_0", 2, "Serious impairment",2)
    # Exec func - Due to DD
    ,c("rsav_27_exec_function_1",0,"Normal",0)
    ,c("rsav_27_exec_function_1",1,"Present",1)
    # Exec func - Due to TBI
    ,c("rsav_28_exec_function_2",0,"Normal",0)
    ,c("rsav_28_exec_function_2",1,"Present",1)
    # Exec func - Due to MH
    ,c("rsav_29_exec_function_3",0,"Normal",0)
    ,c("rsav_29_exec_function_3",1,"Present",1)
  ))
names(ontology) = c('condition','measurement',"definition","score") 

RSAV=getRSAV(patients, columns = unique(ontology$condition))
length(unique(RSAV$person_id))

#####PRESENCE OF ANY SYMPTOM###############
WITH_SYMP<-RSAV %>% group_by(person_id) %>%filter(if_any(starts_with("rsav"), ~ . != 0))
length(unique(WITH_SYMP$person_id))

#######ALTERNATIVE WAY tO PULL PAT WITH POSITIVE SYMPTOMS######
ALT=RSAV[rowSums(subset(RSAV, select = grep("^rsav", names(RSAV), value = TRUE)) != 0) > 0,]

length(unique(ALT$person_id))

######RETREIVING ONLY UNIQUE PATIENT IDS############
ADHD_WITH_EXECFUNC=unique(WITH_SYMP$person_id)
length(ADHD_WITH_EXECFUNC)

RSAV_unique=unique(RSAV$person_id)
length(RSAV_unique)

##############ADHD WITHOUT EXECUTIVE FUNCTIONING###############
ADHD_NO_EXECFUNC = setdiff(RSAV_unique,ADHD_WITH_EXECFUNC)
length(ADHD_NO_EXECFUNC)

#######ADHD WITH EXECUTIVE FUNCTIONING COHORT##############
###########BASELINE CGIS 

ADHD_cgis <- getCGIS(ADHD_WITH_EXECFUNC)
length(unique(ADHD_cgis$person_id))

#########FIRST DIAGNOSIS OF ADHD#############
diag_map <- getCategoryMap("diagnosis_map")
icd_codes <- diag_map[(diag_map$name =="Attention-deficit hyperactivity disorders") , ]$diagnosis
diag_date_ADHD<- getFirstDiagnosisDate(ADHD_WITH_EXECFUNC, unlist(icd_codes))
length(unique(diag_date_ADHD$person_id))

########MERGE FIRST DIAGNOSIS AND CGIS##########
ADHD_EXEC_cgis_diagnosis <- merge(ADHD_cgis,diag_date_ADHD, by = 'person_id')
ADHD_EXEC_cgis_diag<-ADHD_EXEC_cgis_diagnosis%>%filter(as.Date(measurement_date)>=as.Date(first_diagnosis_date))%>%mutate(Baseline=as.numeric(as.Date(measurement_date)-as.Date(first_diagnosis_date)<=14))
ADHD_EXEC_cgis_diag <- ADHD_EXEC_cgis_diag[ADHD_EXEC_cgis_diag$Baseline!=0, ]
ADHD_EXEC_cgis_diag <- ADHD_EXEC_cgis_diag[ADHD_EXEC_cgis_diag$value!=0, ]
length(unique(ADHD_EXEC_cgis_diag $person_id))

#######GET MEDIAN BASELINE CGIS VALUE PER PATIENT#################
ADHD_baseline_cgis_median_1 <- aggregate(ADHD_EXEC_cgis_diag$value, by = list(person_id = ADHD_EXEC_cgis_diag $person_id, diagnosis_date=ADHD_EXEC_cgis_diag $first_diagnosis_date), median)
ADHD_baseline_cgis_median_1$x<- ceiling(ADHD_baseline_cgis_median_1$x)
length(ADHD_baseline_cgis_median_1$person_id)

######SEPARATE CGIS 1 TO 7 VALUES INTO SEPARATE COLUMNS##############
#ADHD_CGIS_SEPARATED_1=ADHD_baseline_cgis_median_1%>%mutate(CGIS_1=as.numeric(x==1))%>%mutate(CGIS_2=as.numeric(x==2))%>%mutate(CGIS_3=as.numeric(x==3))%>%mutate(CGIS_4=as.numeric(x==4))%>%mutate(CGIS_5=as.numeric(x==5))%>%mutate(CGIS_6=as.numeric(x==6))%>%mutate(CGIS_7=as.numeric(x==7))

ADHD_CGIS_SEPARATED_1=ADHD_baseline_cgis_median_1%>%mutate(CGIS_1=as.character(x==1))%>%mutate(CGIS_2=as.character(x==2))%>%mutate(CGIS_3=as.character(x==3))%>%mutate(CGIS_4=as.character(x==4))%>%mutate(CGIS_5=as.character(x==5))%>%mutate(CGIS_6=as.character(x==6))%>%mutate(CGIS_7=as.character(x==7))
head(ADHD_CGIS_SEPARATED_1)

####DEMOGRAPHICS COHORT 1#######################
all_demo <- getDemographics(ADHD_CGIS_SEPARATED_1$person_id)
######JOIN AGE,OTHER DEMOGRAPHICS AND CGIS (SEPARATED VALUE 1 TO 7)
DEMO_CGIS_1= join_all(list(all_demo,ADHD_CGIS_SEPARATED_1),by='person_id',type='inner')%>%dplyr::group_by(x)%>%mutate( age = as.numeric(substring(diagnosis_date,1,4)) - birth_year) 
length(DEMO_CGIS_1$person_id)


###########################COHORT 2#############################
#########WITHOUT EXECUTIVE FUNCTIONING COHORT#########

###########BASELINE CGIS 
ADHD_cgis <- getCGIS(ADHD_NO_EXECFUNC)
length(unique(ADHD_cgis$person_id))

#########FIRST DIAGNOSIS OF ADHD#############
diag_map <- getCategoryMap("diagnosis_map")
icd_codes <- diag_map[(diag_map$name =="Attention-deficit hyperactivity disorders") , ]$diagnosis
diag_date_ADHD<- getFirstDiagnosisDate(ADHD_NO_EXECFUNC, unlist(icd_codes))
length(unique(diag_date_ADHD$person_id))

########MERGE FIRST DIAGNOSIS AND CGIS##########
ADHD_NO_EXEC_cgis_diagnosis <- merge(ADHD_cgis,diag_date_ADHD, by = 'person_id')
ADHD_NO_EXEC_cgis_diag<-ADHD_NO_EXEC_cgis_diagnosis%>%filter(as.Date(measurement_date)>=as.Date(first_diagnosis_date))%>%mutate(Baseline=as.numeric(as.Date(measurement_date)-as.Date(first_diagnosis_date)<=14))
ADHD_NO_EXEC_cgis_diag <- ADHD_NO_EXEC_cgis_diag[ADHD_NO_EXEC_cgis_diag$Baseline!=0, ]
ADHD_NO_EXEC_cgis_diag <- ADHD_NO_EXEC_cgis_diag[ADHD_NO_EXEC_cgis_diag$value!=0, ]
length(unique(ADHD_NO_EXEC_cgis_diag $person_id))

#######GET MEDIAN BASELINE CGIS VALUE PER PATIENT#################
ADHD_baseline_cgis_median_2<- aggregate(ADHD_NO_EXEC_cgis_diag$value, by = list(person_id = ADHD_NO_EXEC_cgis_diag $person_id, diagnosis_date=ADHD_NO_EXEC_cgis_diag $first_diagnosis_date), median)
ADHD_baseline_cgis_median_2$x<- ceiling(ADHD_baseline_cgis_median_2$x)
length(ADHD_baseline_cgis_median_2$person_id)

######SEPARATE CGIS 1 TO 7 VALUES INTO SEPARATE COLUMNS##############
#ADHD_CGIS_SEPARATED_2=ADHD_baseline_cgis_median_2%>%mutate(CGIS_1=as.numeric(x==1))%>%mutate(CGIS_2=as.numeric(x==2))%>%mutate(CGIS_3=as.numeric(x==3))%>%mutate(CGIS_4=as.numeric(x==4))%>%mutate(CGIS_5=as.numeric(x==5))%>%mutate(CGIS_6=as.numeric(x==6))%>%mutate(CGIS_7=as.numeric(x==7))
#head(ADHD_CGIS_SEPARATED_2
     
ADHD_CGIS_SEPARATED_2=ADHD_baseline_cgis_median_2%>%mutate(CGIS_1=as.character(x==1))%>%mutate(CGIS_2=as.character(x==2))%>%mutate(CGIS_3=as.character(x==3))%>%mutate(CGIS_4=as.character(x==4))%>%mutate(CGIS_5=as.character(x==5))%>%mutate(CGIS_6=as.character(x==6))%>%mutate(CGIS_7=as.character(x==7))
head(ADHD_CGIS_SEPARATED_2)

####DEMOGRAPHICS COHORT 2##############
all_demo <- getDemographics(ADHD_CGIS_SEPARATED_2$person_id)

######JOIN AGE,OTHER DEMOGRAPHICS AND CGIS (SEPARATED VALUE 1 TO 7)#####

DEMO_CGIS_2= join_all(list(all_demo,ADHD_CGIS_SEPARATED_2),by='person_id',type='inner')%>%dplyr::group_by(x)%>%mutate( age = as.numeric(substring(diagnosis_date,1,4)) - birth_year) 
length(DEMO_CGIS_2$person_id)



############### STRATIFYING THE DATA##########################

DEMO_CGIS_1$strata=1

DEMO_CGIS_2$strata=2

DEMO_CGIS_3 =rbind(DEMO_CGIS_1,DEMO_CGIS_2)

####################TEST STATISTICS#########################

TEST2=CreateTableOne(
vars=VARIABLES,
strata="strata",
data=DEMO_CGIS_3,
factorVars = CATEGORICAL,
test = TRUE,
testNormal =t.test,
argsNormal = list(var.equal = FALSE),
smd = TRUE,
addOverall = FALSE
)

tab_TEST2A=print(TEST2, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

## Save to a CSV file
write.csv(tab_TEST2A, file = "FINALTABLE.csv")

