#Data parsing script for a large set of differently formatted data files in XLS/CSV.

options(java.parameters = "- Xmx4g")

library(XLConnect) 
library(stringdist)
library(gdata) 
vector_regions<-""
vector_number<-NA

accumulator=1
vector_names<-""
vector_precincts<-""
vector_fileA<-""
vector_checkA<-0
CandidateA=""
dataresults<-as.data.frame(matrix(NA,10000,91))

o=0
kk=0

my_directory<-"C:/Users/Kirill Kalinin/Dropbox"
setwd(my_directory)
main_directory <- my_directory
directories_from_main_directory <- list.dirs(main_directory, full.names=FALSE)
MegaData <- data.frame(matrix(NA,300000,25))
mega_counter <- 1
DepartmentKnown <- FALSE
start.timer <- proc.time()

for(i in 2:length(directories_from_main_directory)){
  
  DonotDo=FALSE

  fpath=directories_from_main_directory[i]
  files1<-sort(list.files(fpath, pattern = ".csv"))
  files2<-files1[which(regexpr("Councilors|Name|missing", files1)<0)]
  
  if(fpath=="Mombasa Municipal Council"){DonotDo=TRUE}
  if(fpath=="Mombasa Municipal Council - Do not do"){DonotDo=TRUE}
  if(fpath=="Nairobi City Council"){DonotDo=TRUE}
  if(fpath=="Nairobi City Council  - Do not do"){DonotDo=TRUE}
  if(fpath=="XXX Council"){DonotDo=TRUE}
  
  SuperData<-data.frame(matrix(NA,10000,25))
  colnames(SuperData)<-c("file_source", "sheet", "j", 
                         "territory", "year", "level", "names", 
                         "department_name", "genderfm",
                         "salaryscale", "basic_salary", "houseallow", 
                         "leaveallow", "councilsns", "councilspf", 
                         "superfund", "others", "designation",
                         "total",  "appointment", "birth_year", 
                         "org_appoint", "org_birthyear", "duplicates")
  
  sheetcount<-length(files2)
  year=0
  super_counter=1
  if (DonotDo==FALSE){
    for(j in 1:sheetcount){
      name=NA
      csv_file<-paste(fpath, files2[j], sep="/")
      mydata = try(read.csv(csv_file, header=FALSE, stringsAsFactors = FALSE))
      if(dim(mydata)[1]==1&dim(mydata)[1]==1){istheredata=0}
      
      if(dim(mydata)[1]>1&dim(mydata)[1]>1){
        
        if(nchar(mydata[1,1])>100){mydata<-mydata[-1,]}
        
          mydataNA<-mydata
          mydataNA[mydataNA== " -   "|mydataNA== " - " |mydataNA==""|mydataNA=="0.0"|mydataNA==0.0| mydataNA=="na"]<-NA
          mydataNA<-mydataNA[,-1]
          istheredata<-length(mydataNA[!is.na(mydataNA)])
          }
      
      if(csv_file==("Mandera Town Council/ManderaTC_0809_Sheet_5.csv")){istheredata=0} #No Data on names -- exclude, override 
      
      if(csv_file==("Londiani Town Council - Done/Londiani_0506_Sheet_7.csv")){istheredata=0} #No Data on names -- exclude, override 
      
      if(csv_file==("Runyenjes Municipal Council - Done/Runyenjes_1011_Sheet_8.csv")){istheredata=0} #No Data on names -- exclude, override 
      
      if(istheredata>25){
        #transform_variables
        vector_names<-which(apply(mydata, 1, function(x) any(grepl("Name|name|date|post", x))))
        
        if(length(vector_names)>1){vector_names <- which(apply(mydata, 1, function(x) any(grepl("Birth|data|birth|data", x))))
        vector_names<-vector_names[1]}
        if(length(vector_names)>1){vector_names <- vector_names[1]}
        if(length(vector_names)==0){
          colnames(mydata) <- paste("V", 1:dim(mydata)[2], sep="_"); 
          mydata<-rbind(colnames(mydata),mydata); vector_names<-1}
        if(is.na(vector_names)){colnames(mydata)<-
          paste("V", 1:dim(mydata)[2], sep="_"); mydata<-rbind(colnames(mydata),mydata); vector_names<-1}
        
        mydata[vector_names,] <- tolower(gsub(" ", "", mydata[1,]))
        municipal <- grepl("Municipal|municipal|MUNICIPAL| MC| mc", fpath)
        city <- grepl("City|city|CITY", fpath)
        county <- grepl("County|county|COUNTY| CC| cc", fpath) 
        township <- grepl("town|Town|TOWN| TC| tc", fpath)
        name <- which(apply(mydata[vector_names,], 2, function(x) any(grepl("name", x))))
        
        if(grepl("Bomet Municipal Council - Done/Bomet_0910_Sheet_1.csv",csv_file)){name<-6}
        if(csv_file=="Busia City Council - Done (delete 0910 sheet 7--blank)/Busiacc_0405_Sheet_5.csv"){name<-3}     
        if(csv_file=="KiKuyu Town Council - 0506 councilor missing - Done/Kikuyu_0405_Sheet_1.csv"){name<-3}     
        if(csv_file=="Kitui Municipal Council - Done/Kituim_0910_Sheet_2.csv"){name<-6}    
        if(csv_file=="Kwale County Council - Done/Kwale_0506_Sheet_2.csv"){name<-6}
        if(csv_file=="Laikipia County Council - Done/Laikipia_0910_Sheet_4.csv"){name<-6} 
        if(csv_file=="Limuru Municipal Council - Done/Limuru_0405_Sheet_4.csv"){name<-3}
        if(csv_file=="Maralal Town Council/MaralalTC_0506_Sheet_3.csv"|csv_file=="Maralal Town Council/MaralalTC_0506_Sheet_4.csv"){name<-6}
        if(csv_file=="Meru County Council/MeruCC_0506_Sheet_5.csv"){name<-6}
        if(grepl("Suba County Council  - Done/Suba_0708_Sheet",csv_file)){name<-6}
        if(grepl("Vihiga Municipal Council - Done/Vihiga_0910_Sheet",csv_file)){name<-6}
        
        if(is.na(name)){name<-which(apply(mydata, 2, function(x) any(grepl("(\\w+)\\b.(\\w+)\\b\\..(\\w+)", x))))[1]}
        if(length(name)==0|sum(mydata[,name][-1]=="", na.rm=TRUE)==length(mydata[,name][-1])){name1<-name; name<-which(apply(mydata, 2, function(x) any(grepl("(\\w+)\\b.(\\w+)\\b\\..(\\w+)", x))))[1]; if(is.na(name)){name<-name1}}
        territory<-unlist(strsplit(fpath, " "))[1]
        department<-which(apply(mydata[vector_names,], 2, function(x) any(grepl("department", x))))[1]
        search_deptC<-which(apply(mydata, 2, function(x) any(grepl("department|DEPARTMENT|Department|Dept", x))))[1]
        search_deptR<-which(apply(mydata, 1, function(x) any(grepl("department|DEPARTMENT|Department|Dept", x))))[1]

        if(length(search_deptC)>0&length(search_deptR)>0){
          department<-mydata[search_deptR,search_deptC]
          if(!is.null(dim(department)[1])){ #more than 1 dimensions?
            if(dim(department)[1]>1){
              try_vector<-gsub("\\c|[[:punct:]]+|department|total|departmental| ", "", paste(tolower(department), collapse=":"))
              if(nchar(try_vector)<=3){department=NA}else{try_vector=department} 
            }}else{
              try_vector<-gsub("\\c|[[:punct:]]+|department|total|departmental| ", "", paste(tolower(department), collapse=":"))
              if(nchar(try_vector)<=3){department=NA}else{try_vector=department} 
            }
        }

        files2[j] <- gsub("-", "_",files2[j])
        s1 <- regmatches(files2[j], regexpr("[Ss]heet\\w+", files2[j]));s2 <- regmatches(s1, regexpr("[[:digit:]]+",s1))
        sheetN <- s2
        
        if (length(sheetN)==0){sheetN="Not Defined"}
        
        genderfm <- which(apply(mydata[vector_names,], 2, function(x) any(grepl("gender", x))))
        salaryscale <- which(apply(mydata[vector_names,], 2, function(x) any(grepl("scale|salscale|salaryscale", x))))
        designation <- which(apply(mydata[vector_names,], 2, function(x) any(grepl("designation|postdesignation", x))))
        councilspf <- which(apply(mydata[vector_names,], 2, function(x)any(grepl("employerspensioncontribution|pf|councilspfcontribution|councilspfcontribu|councilspfcontribution", x))))   
        councilsns <- which(apply(mydata[vector_names,], 2, function(x)any(grepl("nssf|councilsnssfcontribution|councilnssf", x))))                                                           
        houseallow <- which(apply(mydata[vector_names,], 2, function(x) any(grepl("house", x))))
        leaveallow <- which(apply(mydata[vector_names,], 2, function(x)any(grepl("leaveallow|leaveallowances|leave", x))))                                                           
        others <- which(apply(mydata[vector_names,], 2, function(x)any(grepl("others|otherearningegactingallowetc", x))))                                                           
        appointment <- which(apply(mydata[vector_names,], 2, function(x) any(grepl("appoint", x))))
        birth_year <- which(apply(mydata[vector_names,], 2, function(x) any(grepl("birth", x))))
        superfund <- which(apply(mydata[vector_names,], 2, function(x) any(grepl("super|councilssuperfundcontribution|superfund|councilssuperfund", x))))
        basicsalary <- which(apply(mydata[vector_names,], 2, function(x) any(grepl("basicsalary", x))))[1]
        total <- which(apply(mydata[vector_names,], 2, function(x) any(grepl("totalkshs|total", x))))[1]
        
        if(files2[j]==("Kikuyu_0405_Sheet_1.csv")){appointment <- 13; birth_year <- 15}
        
        year[grepl("1213",files2[j])] <- 2012
        year[grepl("1112",files2[j])] <- 2011
        year[grepl("1011",files2[j])] <- 2010
        year[grepl("910",files2[j])] <- 2009
        year[grepl("809",files2[j])] <- 2008
        year[grepl("708",files2[j])] <- 2007
        year[grepl("607",files2[j])] <- 2006
        year[grepl("506",files2[j])] <- 2005
        year[grepl("405",files2[j])] <- 2004
        
        file_source <- paste(fpath, files2[j], sep="")
        
        
        Name_converter <- function(ex_names=ex_names){
          ex_names <- gsub("\\\\","",ex_names)
          ex_names <- gsub("\\\\","/",ex_names)
          ex_names <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", ex_names, perl=TRUE)
          ex_names <- sub('^CLL', '', ex_names)[-1]
          ex_names <- sub('^(\\w+).*\\b(\\w+)$', '\\1 \\2', ex_names)
          ex_names <- sub("^(.)( )", "\\1.\\2", ex_names)
          ex_names <- sub("^(.)\\.([A-Z])", "\\1. \\2\\3",  ex_names)
          return(ex_names)}
        
        
        Year_converter <- function(date_ap=date_ap){
          dates_acc <- vector()
          date_ap_or <- date_ap
          date_ap <- gsub("l","1",date_ap)
          date_ap <- gsub("\\*","",date_ap)
          date_ap[gsub(" ","",date_ap) =="-"] <- NA
          date_ap <- gsub("[[:alpha:]]+| |`", "", date_ap)
          date_ap <- gsub("^ |YR","",date_ap)
          date_ap <- gsub("\\\\", "/", date_ap)
          date_ap <- gsub("\\|","/",date_ap)
          yearR = NA
          date_ap[!grepl("\\d+", date_ap)] <- NA
          date_ap[grep("\\d+[[:punct:]]+\\d+[[:punct:]]+$",date_ap)] <- NA
          date_ap[grep("^\\d+[[:punct:]]+\\d+$",date_ap)] <- NA
          isitnull <- date_ap[!is.na(date_ap)]
          isitnull <- as.numeric(gsub("[[:punct:]]+|[[:alpha:]]+", "",isitnull))
          isitnull <- isitnull[!is.na(isitnull)]
          
          if(length(isitnull) > 2){
            
            for(ii in 1:length(date_ap)){
              if(is.na(date_ap[ii])){yearR = NA}
              if(nchar(date_ap[ii]) == 4){yearR <- date_ap[ii]}
              if(nchar(date_ap[ii]) == 3){if(date_ap[ii]>=900 & date_ap[ii]<=999){
                yearR <- paste("1", date_ap[ii], sep="")
              }else{
                  yearR <- paste("2", date_ap[ii], sep="")}
              }
              
              if(nchar(date_ap[ii]) > 4){
                yearR<-unlist(strsplit(date_ap[ii], 
                                       gsub("(\\d{4})", "", date_ap[ii])))[!unlist(strsplit(date_ap[ii], 
                                       gsub("(\\d{4})", "", date_ap[ii])))==""]
              
                if(date_ap[ii]=="19  NOV.96"){yearR <- 1996}
              
              if(length(yearR)==2){yearR <- yearR[2]}
              }
              if(length(yearR)==0){
                
                lasty<-unlist(strsplit(date_ap[ii], "[^[:alnum:] ]"))[3]
                
                if(nchar(lasty)==2){yearR<-ifelse(lasty>=18&lasty<=99, 
                                                  paste(19,lasty, sep=""),
                                                  paste(20,lasty, sep=""))} 
                
                if(nchar(lasty)==1){yearR<-paste(200,lasty, sep="")}
                
              }
              
              if(length(yearR)==0){yearR<-paste("1", 
                                                unlist(strsplit(date_ap[ii], 
                                                      gsub("(\\d{3})", "", 
                                                      date_ap[ii])))[!unlist(strsplit(date_ap[ii], 
                                                      gsub("(\\d{3})", "", date_ap[ii])))==""], sep="")}
              
              if(!is.na(date_ap[ii])){
                if(length(yearR) != 0){
                  if(!is.na(yearR)){
                    yearR <- gsub("[^[:alnum:] ]", "",yearR)
                    yearR <- as.numeric(yearR)
                    if(length(yearR) != 0){
                      yearR <- yearR[which.max(yearR)]
                      if(yearR > 31 & yearR < 1920){
                        if(nchar(yearR) == 4){yearR<-paste("19", substring(yearR, 3,4), sep="")}
                        if(nchar(yearR) == 3){yearR<-paste("19", substring(yearR, 2,3), sep="")}
                      }}}  
                }}
              
              if(!is.na(date_ap[ii])&nchar(yearR) > 4){
                if(length(yearR)!=0){yearR <- substring(date_ap[ii], 
                                                        nchar(date_ap[ii])-3, 
                                                        nchar(date_ap[ii]))}
              }
              
              dates_acc <- c(dates_acc, as.numeric(gsub("[^[:alnum:]]", "", yearR)))}
          }else{
            dates_acc <- rep(NA, length(date_ap_or))
          }
          ex_appointment <- dates_acc
          
          return(ex_appointment)}
        
        ex_names<-Name_converter(mydata[,name])
        
        mydata[mydata==""]<-NA
        department_name=NA
        
        if(is.na(mydata[[length(mydata[,1])-1,1]]) & 
           !is.na(mydata[[length(mydata[,1]),1]])){department_name <- mydata[,1][length(mydata[,1])]}
        if(is.na(mydata[(length(mydata[,2])-1),2]) & 
           !is.na(mydata[[length(mydata[,2]),2]])){department_name <- mydata[,2][length(mydata[,2])]}
        
        vector_ex_names<-ex_names[!is.na(ex_names)]
        
        splOrnot <- sum(grepl(" ", vector_ex_names)) > 0
        if(splOrnot == TRUE){
          vector_ex_names2 <- sub("(\\w+). ", '\\2', ex_names)
          vector_ex_names <- tolower(vector_ex_names2)}
        ex_file_source <- rep(file_source, length(ex_names))
        ex_year <- rep(year, length(ex_names))
        
        if (length(genderfm)==0){ex_genderfm<-rep(NA, length(ex_names))}else{
          ex_genderfm<-ifelse(gsub(",", "", mydata[,genderfm][-1])=="M",1,ifelse(is.na(gsub(",", "", mydata[,genderfm][-1])),NA,0))}
        if (length(salaryscale)==0){ex_salaryscale<-rep(NA, length(ex_names))}else{
          ex_salaryscale<-as.numeric(gsub(",", "", mydata[,salaryscale][-1]))}
        if (length(houseallow)==0){ex_houseallow<-rep(NA, length(ex_names))}else{
          ex_houseallow<-as.numeric(gsub(",", "", mydata[,houseallow][-1]))}
        if (length(leaveallow)==0){ex_leaveallow<-rep(NA, length(ex_names))}else{
          ex_leaveallow<-as.numeric(gsub(",", "", mydata[,leaveallow][-1]))}
        if (length(councilsns)==0){ex_councilsns<-rep(NA, length(ex_names))}else{
          ex_councilsns<-as.numeric(gsub(",", "", mydata[,councilsns][-1]))}
        if (length(councilspf)==0){ex_councilspf<-rep(NA, length(ex_names))}else{
          ex_councilspf<-as.numeric(gsub(",", "", mydata[,councilspf][-1]))}
        if (length(designation)==0){ex_designation<-rep(NA, length(ex_names))}else{
          ex_designation<-gsub(",", "", mydata[,designation][-1])}
        if (length(appointment)==0){ex_appointment<-rep(NA, length(ex_names))}else{
          ex_appointment<-Year_converter(mydata[,appointment][-1])}
        if (length(birth_year)==0){ex_birth_year<-rep(NA, length(ex_names))}else{
          ex_birth_year<-Year_converter(mydata[,birth_year][-1])}
        if (length(others)==0){ex_others<-rep(NA, length(ex_names))}else{
          ex_others<-as.numeric(gsub(",", "", mydata[,others][-1]))}
        if (length(superfund)==0){ex_superfund<-rep(NA, length(ex_names))}else{
          ex_superfund<-as.numeric(gsub(",", "", mydata[,superfund][-1]))}
        if (length(basicsalary)==0|is.na(basicsalary)){ex_basicsalary<-rep(NA, length(ex_names))}else{
          ex_basicsalary<-as.numeric(gsub(",", "", mydata[,basicsalary][-1]))}
        if (length(total)==0|is.na(total)){ex_total<-rep(NA, length(ex_names))}else{
          ex_total<-as.numeric(gsub(",", "", mydata[,total][-1]))}
        if (length(appointment)==0){original_appointment<-rep(NA, length(ex_names))}else{
          original_appointment<-mydata[,appointment][-1]}
        if (length(birth_year)==0){original_year<-rep(NA, length(ex_names))}else{
          original_year<-mydata[,birth_year][-1]}
        
        if(is.na(department_name)&length(department)>0){department_name = department}
        if (department_name==""|is.na(department_name)){
          LookintoBudgetFiles<-budgetFinder(year, vector_ex_names, ex_names, municipal, city, county, township, DepartmentKnown=FALSE)
        ex_department_name<-rep(LookintoBudgetFiles[[1]], length(ex_names))}else{
          ex_department_name<-rep(department_name,length(ex_names)); 
          LookintoBudgetFiles<-budgetFinder(year, vector_ex_names, ex_names, municipal, city, county, township, DepartmentKnown=TRUE)}
        
        if(municipal==TRUE){ex_level <- rep("Municipal",length(ex_names))} 
        if(county==TRUE){ex_level <- rep("County",length(ex_names))}
        if(township==TRUE){ex_level <- rep("Township",length(ex_names))} 
        if(city==TRUE){ex_level <- rep("City",length(ex_names))} 
        if(municipal==FALSE&county==FALSE&township==FALSE&city==FALSE){ex_level<-rep("Unknown",length(ex_names))}
        
        ex_file_source <- rep(file_source,length(ex_names))
        ex_territory <- rep(territory,length(ex_names))
        ex_j <- rep(j,length(ex_names))
        ex_sheet <- rep(sheetN,length(ex_names))
        
        sheetdata<-data.frame(ex_file_source,ex_sheet, ex_j, ex_territory, ex_year, 
                              ex_level, ex_names, ex_department_name, 
                              ex_genderfm, ex_salaryscale, ex_basicsalary, ex_houseallow, 
                              ex_leaveallow, ex_councilsns, ex_councilspf, ex_superfund,
                              ex_others, ex_designation, ex_total, ex_appointment, 
                              ex_birth_year, original_appointment, original_year, stringsAsFactors = FALSE)
        
        
        sheetdata2<-sheetdata[!grepl("VACANT|Vacant|VACANCY|Vacancy|VACANCIES|
                                     Vacancies|PROPOSED|Proposed|APPROVED|approved|
                                     aproved|Approved|APROVED|CREATED|New", sheetdata$ex_names),]
        sheetdata2<-sheetdata2[!is.na(sheetdata2$ex_names),]
        sheetdata2<-sheetdata2[sheetdata2$ex_names!="",]
        
        if(dim(sheetdata2)[1]>0){
          SuperData [super_counter:(super_counter+dim(sheetdata2)[1]-1),1:23] <- sheetdata2
          super_counter=super_counter+dim(sheetdata2)[1]}
        
        gc()
        
        print(paste("Running... File:", unique(ex_file_source), "  Region:  ", 
                    territory,", i=",i, "        Sheet:  ", sheetN, ", j=",j, sep=""), quote = FALSE)
      }
      # rm(SuperData)
      gc()
    } #SheetEnd of s
    
    SuperData<-SuperData[!is.na(SuperData$names),]
    TestDuplicates<-HelpMeWithDuplicates(SuperData)
    DuplicatesNames<-TestDuplicates[[1]]
    DuplicatesBirth<-as.numeric(TestDuplicates[[2]])
    SuperData[,24]<-DuplicatesNames
    SuperData[,25]<-DuplicatesBirth
    MegaData [mega_counter:(mega_counter+dim(SuperData)[1]-1), ]<-SuperData
    mega_counter=mega_counter+dim(SuperData)[1]
  } 
}#Big loop ends  

colnames(MegaData)<-colnames(SuperData)<-c("file_source", "sheet", "j", "territory", "year", "level", 
                                           "names", "department_name", "genderfm",
                                           "salaryscale", "basic_salary", "houseallow", 
                                           "leaveallow", "councilsns", "councilspf", "superfund", "others", "designation",
                                           "total",  "appointment", "birth_year", "org_appoint", "org_birthyear", "substitute_name")

cat("The entire computation took ", proc.time()[1]-start.timer[1], "secs")

#write.csv(MegaData, "C:/Users/Kirill Kalinin/Desktop/Work for Mai/MegaData.csv")

index<-paste(MegaData$territory, MegaData$year, MegaData$level, MegaData$sheet, sep=":")