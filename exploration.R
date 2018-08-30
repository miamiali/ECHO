#wipte the environment
rm(list = ls())
#unload package to just memory allocation
detach("package:xlsx", unload = TRUE)
#increase java heap size
options(java.parameters = "-Xmx8g")
#options(java.home="C:/Program Files (x86)/Java/jre7/")
install.packages(c("ggplot2","dplyr","stringr","scales","xlsx","data.table","tidyr","ggpubr","purrr", "gridExtra","broom","lazyeval", "Hmisc","readxl"))
r1<-c("ggplot2","dplyr","stringr","scales","xlsx","data.table","tidyr","ggpubr","purrr", "gridExtra","broom","lazyeval", "Hmisc","readxl")

lapply(r1,require,character.only=TRUE)

#cleaning/exploring the datasets###
#step A1: understand what datasets we have and what's in them
#step A2: understand the distributions of the key variables - do it variable by variable

#step A1####
#create a datafram contains the file names and variable names###

#get the name of all the workbooks
file_name<-list.files(path = "H:/ECHO_raw", all.files = TRUE, full.name = TRUE, recursive = TRUE) #all.files determines if the hidden file will show; full.name determines if the path or name of the document will show; recursive determines if the files in sub folders will show
file_name2<-file_name[-c(66,68)] #remove 65 and 67 which are txt and pdf files


#create a function to read each spreadsheet
read_excel_allsheets <- function(filename, tibble = FALSE) {
  #function to read every sheet in a workbook as a dataframe and name each dataframe according to the sheet name
    sheets <- readxl::excel_sheets(filename)
#    x <- lapply(sheets, function(X) xlsx::read.xlsx2(filename, sheetName = X))
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = "text"))
    if(!tibble) x <- lapply(x, as.data.frame)
    site_head_rm <- sub(".+/.+/.+/", "", filename)
    site_tail_rm <- sub("\\s-\\s.+", "", site_head_rm)
    site <- gsub("\\s", "", site_tail_rm) #gsub will match all, sub only match 1
    names(x) <- paste(site, sheets, sep = "_") 
    x
    
}

#create a function to read each spreadsheet
read_excel_allsheets2 <- function(filename, tibble = FALSE) {
  #function to read every sheet in a workbook as a dataframe and name each dataframe according to the sheet name
  sheets <- readxl::excel_sheets(filename)
  x <- map(sheets, function(X) xlsx::read.xlsx2(filename, sheetName = X))
  #    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = "text"))
  if(!tibble) x <- lapply(x, as.data.frame)
  site_head_rm <- sub(".+/.+/.+/", "", filename)
  site_tail_rm <- sub("\\s-\\s.+", "", site_head_rm)
  site <- gsub("\\s", "", site_tail_rm) #gsub will match all, sub only match 1
  names(x) <- paste(site, sheets, sep = "_") 
  x
  
}


#call the function to create a list contains all the workbooks and spreadsheets
myfiles <- map(file_name2, read_excel_allsheets2) #run the function on every file

names(myfiles) <- file_name2 #rename the list using file name

#find the spreadsheet with the most variables so we know how many variables the data_info set should contain
val_num <- c()
n <- 0
for (i in 1 : length(myfiles)){
  for (j in 1 : length(myfiles[[i]])){
    n <- n + 1
        val_num[n] <- ncol(myfiles[[i]][[j]])
  }
}
max_val_num <- max(val_num,na.rm = TRUE) #max number of variables is 268

#create a vector of variable names 
m <- 1 : max_val_num
val_name <- paste("Var", m, sep = "") 
#create a data frame with default value of 1 for each variable
data_info <- data.frame(Dataset = 1,
                        Sheet_name = 1)
for (i in 1 : length(val_name)){
  data_info[,i+2] <- 1
  colnames(data_info)[i+2] <- val_name [i]
}

#assign predefined variable names to the new dataframe
colnames(data_info[3:270]) <- val_name


#create a replicate dataset to work on
data_info_rep <- data_info  
#read the variables in each spreadsheet to the new dataframe
n <- 1
for (i in 1 : length(myfiles)){
  tryCatch({                                #tryCatch to skip error and run the next iteration
    for (j in 1 : length(myfiles[[i]])){
      data_info_rep[n,1] <- names(myfiles)[i]
      data_info_rep[n,2] <- names(myfiles[[i]])[j]
      for (h in 1 : ncol(myfiles[[i]][[j]])){
        data_info_rep[n, h + 2] <- colnames(myfiles[[i]][[j]])[h]
      }
    n = n + 1
    }
  }, error=function(e){})  
}

#export the data_info_Rep
write.xlsx(data_info_rep,file = "H:/ECHO/data_info.xlsx")

#step2####
#functions for step 2###

firstrow_to_colname <- function(df){
  #function to change the first row to column name
  col_name <- as.vector(t(df[1, ]))
  colnames(df) <- col_name
  df <- df[-1,] #remove the first row which is the column names
}

strip_name <- function(list, site){
  #remove the "E:/ECHO.....xlsx" part of the name
  head_rm <- gsub(".+Endo\\sEcho", "", names(list), ignore.case = TRUE)
  tail_rm <- gsub("\\.xlsx", "", head_rm)
  space_rm <- gsub("\\s", "", tail_rm)
  new_name <- paste(site, space_rm,  sep ="_") #add the abbr. of the site in front of the dataset name
  names(list) <- new_name
  list
}

#import file with consented patient's key##
patient_key <- read.xlsx2(file = "H:/ECHO_raw/ECHO Health/Endo ECHO Patient Key_mod.xlsx", sheetIndex = 1 )  #why xlsx2 is better than xlsx????
#patient_key <- firstrow_to_colname(patient_key)
patient_key$Site.ID.or.MRN <- as.character(patient_key$Site.ID.or.MRN)
patient_key$DOB_1 <- as.Date(patient_key$DOB)
  


patient_dem <- as.data.frame(myfiles[6])

date_col <- c("DOB", "Medicaid.Begin.Date", "Medicaid.End.Date", "Consent.Date" )
for (col in date_col){
  patient_key[ ,col]<-as.Date(as.numeric(as.character(patient_key[ ,col])), origin = "1899-12-30")
}

#check duplicates by ECHO ID and MRN#

patient_key %>%
  separate(col = `COE Location`, into =c("Site", "Sub-site"), sep = "-") %>%
  group_by(Site) %>%
  summarise(n_distinct(`ECHO ID`, na.rm = TRUE)) #count the distinct 



#step 2.1 a1c####
#step 2.1.1 El Centro####
#import dataset with patient key (ASSUMING ALL THE DATA FROM THE KEY FILE ARE FOR ENROLLED PARTICIPANTS)##
#patient account number is equivalent to MRN and is the only unique identifier 
el_central <- myfiles[14:41]
el_central <- strip_name(el_central,"ec")
#get the a1c table and demographics table
ec_A1c <- as.data.frame(el_central[1])
ec_dem <- as.data.frame(el_central[8])
#remove the last row of ec_dem because it's not data
ec_dem <- ec_dem[-103, ]


#count the unique patients in the el centro EMR
ec_dem %>%
  summarise(n_distinct(ec_Demographics.ElCentro_Page1_1.Patient.Account.Number))
#count the unique patients who have a1c and if any of them don't have demographic info
ec_A1c %>%
#  summarise(n_distinct(ec_A1c.ElCentro_Page1_1.Patient.Account.Number)) %>%
  anti_join(ec_dem, by = c("ec_A1c.ElCentro_Page1_1.Patient.Account.Number" = "ec_Demographics.ElCentro_Page1_1.Patient.Account.Number")) #anti-join shows the records in the primary dataset(left) that don't have a match in the secondary dataset(right), if won't matter if the "patient key" file doesn't have the demo. info for the patients in the a1c 
#check if patients who have a1c are not in the patient key file which might indicate that they are not program participants
ec_A1c %>%
  anti_join(patient_key, by = c("ec_A1c.ElCentro_Page1_1.Patient.Account.Number" = "Site.ID.or.MRN")) %>%
  select(ec_A1c.ElCentro_Page1_1.Patient.Account.Number) %>%
  distinct()
#check if patients with the same MRN are the same person in the EMR dataset
ec_A1c %>%
  group_by(ec_A1c.ElCentro_Page1_1.Patient.Account.Number,ec_A1c.ElCentro_Page1_1.Patient.Name) %>%
  summarise(n(),
            n_distinct(ec_A1c.ElCentro_Page1_1.Patient.Account.Number,ec_A1c.ElCentro_Page1_1.Patient.Name))
#check if the a1c values with the same date but different visit type are different
ec_A1c %>%
  group_by(ec_A1c.ElCentro_Page1_1.Patient.Account.Number,ec_A1c.ElCentro_Page1_1.Encounter.Date) %>%
  filter(n_distinct(ec_A1c.ElCentro_Page1_1.Lab.Attribute.Value) > 1)

#?decide if to remove any of the participants####

#spread the measurements dataset so 1 observation represent 1 patient

#assume it doesn't matter with the type of visit
ec_a1c_spread <- ec_A1c %>% #each patient is an oberservation good for calculation time difference
  group_by(ec_A1c.ElCentro_Page1_1.Patient.Account.Number, ec_A1c.ElCentro_Page1_1.Encounter.Date) %>%
  filter(row_number()==1) %>% #select the first row of each group
  spread(ec_A1c.ElCentro_Page1_1.Encounter.Date, ec_A1c.ElCentro_Page1_1.Lab.Attribute.Value) 

ec_a1c_united <- ec_A1c %>% #each visit is an obervation good for plotting
  group_by(ec_A1c.ElCentro_Page1_1.Patient.Account.Number, ec_A1c.ElCentro_Page1_1.Encounter.Date) %>%
  filter(row_number()==1)

#PMS####
#med_rec_nbr is equivalent to MRN, together with ECHO id are the unique identifiers
#import dataset with patient key (ASSUMING ALL THE DATA FROM THE KEY FILE ARE FOR ENROLLED PARTICIPANTS)##
pms <- myfiles[65]
#get the a1c table
pms_A1c <- as.data.frame(pms[[1]][3])
pms_dem <- as.data.frame(pms[[1]][5])

#count the unique patients in the pms EMR
pms_dem %>%
  summarise(n_distinct(PMS_Demographics.med_rec_nbr),
            n_distinct(PMS_Demographics.Echo_ID))
#count the unique patients who have a1c and if any of them don't have demographic info
pms_A1c %>%
#    summarise(n_distinct(PMS_A1c.med_rec_nbr)) #%>%
  anti_join(pms_dem, by = c("PMS_A1c.med_rec_nbr" = "PMS_Demographics.med_rec_nbr")) #anti-join shows the records in the primary dataset(left) that don't have a match in the secondary dataset(right), if won't matter if the "patient key" file doesn't have the demo. info for the patients in the a1c 
#check if patients who have a1c are not in the patient key file which might indicate that they are not program participants
pms_A1c %>%   
  mutate(PMS_A1c.med_rec_nbr = as.character(PMS_A1c.med_rec_nbr)) %>%
  anti_join(patient_key, by = c("PMS_A1c.med_rec_nbr" = "Site.ID.or.MRN")) %>%
  select(PMS_A1c.med_rec_nbr) %>%
  distinct()
#check if patients with the same MRN are the same person in the EMR dataset
pms_A1c %>%
  group_by(PMS_A1c.med_rec_nbr, PMS_A1c.last_name) %>%
  summarise(n(),
            n_distinct(PMS_A1c.med_rec_nbr, PMS_A1c.last_name))


#spread the measurements dataset so 1 observation represent 1 patient

#change date and time to date
pms_A1c$visit_date <- as.Date(pms_A1c$PMS_A1c.create_timestamp)

#assume it doesn't matter with the type of visit
pms_a1c_spread <- pms_A1c %>% #each patient is an oberservation good for calculation time difference
  group_by(PMS_A1c.med_rec_nbr, visit_date) %>%
  filter(row_number()==1) %>% #select the first row of each group
  spread(visit_date, round(PMS_A1c.obsvalue,1)) 

pms_a1c_united <- pms_A1c %>% #each visit is an obervation good for plotting
  group_by(PMS_A1c.med_rec_nbr, visit_date) %>%
  filter(row_number()==1) #select the first row of each group


#La Casa####
#pat person nbr is equivalent to MRN and is the only unique identifier
#import dataset with patient key (ASSUMING ALL THE DATA FROM THE KEY FILE ARE FOR ENROLLED PARTICIPANTS)##
  casa <- myfiles[63]
#get the a1c table
casa_A1c <- as.data.frame(casa[[1]][3])
casa_dem <- as.data.frame(casa[[1]][1])

#count the unique patients in the la casa EMR
casa_dem %>%
  summarise(n_distinct(LaCasa_Demographics.Pat.Person.Nbr))
#count the unique patients who have a1c and if any of them don't have demographic info
casa_A1c %>%
  #  summarise(n_distinct(LaCasa_A1c.Pat.Person.Nbr)) #%>%
  anti_join(casa_dem, by = c("LaCasa_A1c.Pat.Person.Nbr" = "LaCasa_Demographics.Pat.Person.Nbr")) #anti-join shows the records in the primary dataset(left) that don't have a match in the secondary dataset(right), if won't matter if the "patient key" file doesn't have the demo. info for the patients in the a1c 

#because the format of the MRN are different in the patient key file and the emr data, have to merge two and compare one by one
patient_key_short <- patient_key[ ,c(1,2,7,9,10,11)]
patient_dem_short <- patient_dem[ ,c(1,2,3,5,6,7,12)]
names_dem <- c("echoid", "site", "firstnam", "lastnam", "gender", "race", "dob")
colnames(patient_dem_short) <- names_dem

patient_master <- patient_dem_short %>%
  mutate(echoid = as.character(echoid)) %>%
  full_join(patient_key_short, by = c("echoid" = "ECHO ID"))

casa_dem_short <- casa_dem[ , c(1,2,5,6,9,10,11)]

casa_merged <- patient_master %>%
  filter(grepl("la casa", `COE Location`, ignore.case = TRUE)) %>%
  full_join(casa_dem_short, by = c("Site.ID.or.MRN" = "LaCasa_Demographics.Pat.Person.Nbr"))
write.xlsx(casa_merged, file = "H:/ECHO/La Casa/La Casa Merged.xlsx")

#spread the measurements dataset so 1 observation represent 1 patient

#assume it doesn't matter with the type of visit
casa_a1c_spread <- casa_A1c %>% #each patient is an oberservation good for calculation time difference
  group_by(LaCasa_A1c.Pat.Person.Nbr, LaCasa_A1c.Result.Report.Date) %>%
  filter(row_number()==1) %>% #select the first row of each group
  spread(LaCasa_A1c.Result.Report.Date, LaCasa_A1c.Result.Value..Numeric. ) 

casa_a1c_united <- casa_A1c %>% #each visit is an obervation good for plotting
  group_by(LaCasa_A1c.Pat.Person.Nbr, LaCasa_A1c.Result.Report.Date) %>%
  filter(row_number()==1) #select the first row of each group

casa_A1c %>% #each visit is an obervation good for plotting
  group_by(LaCasa_A1c.Pat.Person.Nbr, LaCasa_A1c.Result.Report.Date) %>%
  summarise(n=n()) %>%
  filter(n>1)  #4 patients have multiple a1c values on the same day 26017.0, 29774.0, 48277.1, 87926

#HMS####
#Account number is equivalent to MRN, together with ECHO id are the unique identifiers
#import dataset with patient key (ASSUMING ALL THE DATA FROM THE KEY FILE ARE FOR ENROLLED PARTICIPANTS)##
hms_key <- read.xlsx2(file = "H:/ECHO_raw/HMS/HMS - Endo Echo Data.xlsx", sheetIndex = 1)

hms_1 <- myfiles[62]
hms_2 <- myfiles[63]
#get the a1c table
hms_A1c <- as.data.frame(hms_1[[1]][1])
hms_dem <- as.data.frame(hms_2[[1]][1])
hms_key <- as.data.frame(hms_2[[1]][2])

#count the unique patients who have demographic info
hms_key %>%
  summarise(n_distinct(HMS_Sheet1.HMS_MRN))

#check if any key doesn't have demographic info
hms_key %>%
  group_by(HMS_Sheet1.HMS_MRN) %>%
  distinct(HMS_Sheet1.HMS_MRN) %>%
  anti_join(hms_dem, by = c("HMS_Sheet1.HMS_MRN" = "HMS_Demographics.AccountNo"))

#create a1c and demo dataset for hms participants 
hms_dem_par <- hms_dem %>%
  semi_join(hms_key, by = c("HMS_Demographics.AccountNo" = "HMS_Sheet1.HMS_MRN")) %>% #semi_join to use patient key as a filter to ge participants data
  #summarise(n_distinct(HMS_Demographics.AccountNo))
  group_by(HMS_Demographics.AccountNo) %>%
  filter(row_number() == 1)

hms_A1c_par <- hms_key %>%
  group_by(HMS_Sheet1.HMS_MRN) %>%
  filter(row_number() == 1) %>%
  inner_join(hms_A1c, by = c("HMS_Sheet1.HMS_MRN" = "HMS_A1C.AccountNo"))

#count the unique patients who have a1c and if any of them don't have demographic info
hms_A1c_par %>%
      summarise(n_distinct(HMS_Sheet1.HMS_MRN)) #%>%
  #anti_join(hms_dem_par, by = c("HMS_Sheet1.HMS_MRN" = "HMS_Demographics.AccountNo")) #anti-join shows the records in the primary dataset(left) that don't have a match in the secondary dataset(right), if won't matter if the "patient key" file doesn't have the demo. info for the patients in the a1c 

#check if patients who have a1c are not in the patient key file which might indicate that they are not program participants
hms_A1c_par %>%
  group_by() %>%
  anti_join(patient_key, by = c("HMS_Sheet1.HMS_MRN" = "Site.ID.or.MRN")) %>%
  select(HMS_Sheet1.HMS_MRN) %>%
  distinct()

#assume it doesn't matter with the type of visit
hms_a1c_spread <- hms_A1c_par %>% #each patient is an oberservation good for calculation time difference
  group_by(HMS_Sheet1.HMS_MRN, HMS_A1C.EncDate) %>%
  filter(row_number()==1) %>% #select the first row of each group
  spread(HMS_A1C.EncDate, HMS_A1C.LabValue) 

HMS_a1c_united <- hms_A1c_par %>% #each visit is an obervation good for plotting
  group_by(HMS_Sheet1.HMS_MRN, HMS_A1C.EncDate) %>%
  filter(row_number()==1) #select the first row of each group


#First Choice####
#MRN and ECHO ID are the unique identifiers
#import dataset with patient key##
FC <- myfiles[43]
FC_dem <- myfiles[42]
#get the a1c table and demographics table
fc_A1c <- as.data.frame(FC[[1]][1])
fc_A1c_unique <- as.data.frame(FC[[1]][2])
fc_A1c <- firstrow_to_colname(fc_A1c)
fc_A1c_unique <- firstrow_to_colname(fc_A1c_unique)
fc_A1c$DOB <- as.Date(as.numeric(fc_A1c$DOB), origin = "1899-12-30")
fc_A1c$DATE <- as.Date(as.numeric(fc_A1c$DATE), origin = "1899-12-30")

fc_dem <- as.data.frame(FC_dem)
fc_dem <- firstrow_to_colname(fc_dem)

#check if any key of study participants doesn't have demographic info
fc_A1c %>%
  #group_by(MRN) %>%
  #distinct(MRN) %>%
  anti_join(fc_dem, by = c("MRN" = "Mrn"))

#count the unique patients who have a1c and if any of them don't have demographic info
fc_A1c %>%
  filter(!is.na(`ECHO#`)) %>%
  summarise(n_distinct(MRN),
            n_distinct(`ECHO#`)) #%>%
#anti_join(hms_dem_par, by = c("HMS_Sheet1.HMS_MRN" = "HMS_Demographics.AccountNo")) #anti-join shows the records in the primary dataset(left) that don't have a match in the secondary dataset(right), if won't matter if the "patient key" file doesn't have the demo. info for the patients in the a1c 

#check the list of patients in the a1c unique id list but not in the full list, ECHO ID didn't get copied
fc_par_MRN <- fc_A1c_unique[!is.na(fc_A1c_unique$`ECHO#`), c(3,9)]
fc_A1c %>%
  semi_join(fc_par_MRN, by = "MRN") %>%
#  filter(is.na(`ECHO#`)) %>%
  summarise(n_distinct(MRN))

#check if patients who have a1c are not in the patient key file which might indicate that they are not program participants
fc_par_MRN %>%
  group_by() %>%
  anti_join(patient_key, by = c("MRN" = "Site.ID.or.MRN")) %>%
  select(MRN) %>%
  distinct()

#assume it doesn't matter with the type of visit
fc_a1c_spread <- fc_A1c %>% #each patient is an oberservation good for calculation time difference
  semi_join(fc_par_MRN, by = "MRN") %>%
  group_by(MRN, DATE) %>%
  filter(row_number()==1) %>% #select the first row of each group
  spread(DATE, Value) 

fc_a1c_united <- fc_A1c %>% #each visit is an obervation good for plotting
  semi_join(fc_par_MRN, by = "MRN") %>%
  group_by(MRN, DATE) %>%
  filter(row_number()==1) #select the first row of each group












#combine el_centro, pms, and la casa a1c values

colnames_a1c <- c("MRN", "site", "visit_date", "a1c")

a1c_ec <- ec_a1c_united %>%
    mutate(site = "EC") %>%
    select(ec_A1c.ElCentro_Page1_1.Patient.Account.Number, site, ec_A1c.ElCentro_Page1_1.Encounter.Date, ec_A1c.ElCentro_Page1_1.Lab.Attribute.Value)
colnames(a1c_ec) <- colnames_a1c
a1c_ec$visit_date <- as.POSIXlt(as.Date(a1c_ec$visit_date))

a1c_casa <- casa_a1c_united %>%
  mutate(site = "La Casa") %>%
  select(LaCasa_A1c.Pat.Person.Nbr, site, LaCasa_A1c.Result.Report.Date, LaCasa_A1c.Result.Value..Numeric.)
colnames(a1c_casa) <- colnames_a1c
a1c_casa$a1c <- as.character(a1c_casa$a1c)
a1c_casa$visit_date <- as.POSIXlt(as.Date(a1c_casa$visit_date))

a1c_pms <- pms_a1c_united %>%
  mutate(site = "pms") %>%
  select(PMS_A1c.med_rec_nbr, site, visit_date, PMS_A1c.obsvalue)
colnames(a1c_pms) <- colnames_a1c
a1c_pms$MRN <- as.character(a1c_pms$MRN)
a1c_pms$visit_date <- as.POSIXlt(as.Date(a1c_pms$visit_date))

a1c_3 <- rbind.data.frame(a1c_casa, a1c_ec, a1c_pms)  #do i have to convert date using posixlt? tried rbind and bind_rows all failed
a1c_3$visit_date <- as.Date(a1c_3$visit_date)
a1c_3_key <- unique(a1c_3$MRN)

#count no. of participants with no a1c data 

patient_a1c_missing <- patient_key %>%
  filter(grepl("pms",`COE Location`, ignore.case = TRUE) | grepl("la casa",`COE Location`, ignore.case = TRUE) | grepl("el centro",`COE Location`, ignore.case = TRUE) ) %>%
  filter(!`Site.ID.or.MRN` %in% a1c_3_key )
table(patient_a1c_missing$`COE Location`)

#count no. of participants with only 1 a1c
patient_a1c_1p <- a1c_3 %>%
  group_by(MRN) %>%
  filter(n()==1)

table(patient_a1c_1p$site)

# abnormal data points
a1c_3 %>%
  filter(a1c == "(Other)")

a1c_3_dup <- a1c_3

a1c_3_dup$a1c <- as.factor(a1c_3_dup$a1c)
table(a1c_3_dup$a1c)

a1c_3_dup$a1c <- round(as.numeric(as.character(a1c_3_dup$a1c)),1)

a1c_3_dup %>%
  group_by() %>%
  filter(a1c <100) %>%
  ggplot(aes(x=visit_date,y=a1c, col=as.factor(MRN), group = as.factor(MRN))) +
  geom_point()+
  geom_line()+
  theme(legend.position = "none") +
  facet_grid(.~as.factor(length))#the plot not so useful 

a1c_3_dup$length_fst_lst <- as.numeric(a1c_3_dup$lengh_fst_lst)
summary(a1c_3_dup$lengh_fst_lst)

a1c_3_dup <- a1c_3_dup %>%
  group_by(MRN) %>% 
  arrange(visit_date) %>% 
  mutate(length_fst_lst = last(visit_date) - first(visit_date)) %>%
  mutate(length = ifelse(length_fst_lst < 365, 1, ifelse(length_fst_lst < 712, 2, ifelse(length_fst_lst < 1068, 3, 4)))) %>%
  mutate(no_a1c = n(),
         diff_a1c = last(a1c) - first(a1c))


summary <- a1c_3_dup %>%
  group_by(MRN) %>%
  filter(row_number()==1)

summary %>%  
  ggplot(aes(x=as.factor(no_a1c))) +
  geom_bar(aes(y=..count../sum(..count..)))

test<-summary %>%  
  filter(a1c < 100) 

