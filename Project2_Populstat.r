#Web scraper for http://www.populstat.info/

library(RCurl)
library(XML)
library(rvest)
library(dplyr)
library(scrapeR)

scraplinks <- function(url){
     webpage <- xml2::read_html(url)
     url_ <- webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
    link_ <- webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
    return(data_frame(link = link_, url = url_))
    }

base_url<-"http://www.populstat.info/"
Rurls<-cbind("http://www.populstat.info/", scraplinks(base_url)[2:7,2])
Rurls<-apply(Rurls,1,function(x) paste(x[1], x[2], sep=""))

Regions<-c("Africa", "Americas", "Asia", "Europe", "Oceania", "Americas")
country_links<-data.frame(matrix(NA,278,3))
pointer=1

for(i in 1:length(Rurls)){
  
  path_link <- paste("http://www.populstat.info/", Regions[i], "/", sep="")
  links <- as.data.frame(scraplinks(Rurls[i]))
  links <- links[grepl(".htm$", links[,2]),]
  
   links[,1]<-
     unlist(lapply(regmatches(links[,1], gregexpr("[A-Z ]+|[the A-Z ]+",links[,1])),
                   function(x){
                     if(is.na(x[1])){result=x[1]
                     }else{
                       if(nchar(x[1])==0|x[1]==" the "){result=""}else{result=x[1]}}
                     return(result)}))
   
   links[,1] <- gsub("^\\s*|\\s*$","", links[,1])
   
   rownames(links) <- 1:dim(links)[1]

  if(i==1){
    links[56,1]<-"COTE d'IVIORE";
    links[162,1]<-"REUNION";
    links<-links[-c(13,43,60,161,223),]
  }
 
  if(i==2){
    links[136,1]<-"UNITED STATES: EXTERNAL TERRITORIES";
    links<-links[-c(169),]
  }
   
  if(i==3){links<-links[-c(41, 77, 78, 95, 97),]}
   
  if(i==4){links<-links[-c(43, 48, 76, 77, 82, 91,
                           110, 146, 147, 152, 161, 171, 173, 209),]}
   
  if(i==5){
     links[59,1] <- "NAURU";
     links[72,1] <- "NIUE";
     links[75,1] <- "NORFOLK ISLAND"; 
     links<-links[-c(5, 6, 12, 13, 14, 15, 17, 19, 24, 33, 
                     38:43, 56, 57, 69, 70, 73, 84, 89, 92, 101:102, 115, 120),]}
   
  if(i==6){
     links[26,1]<-"DISTRICT OF COLUMBIA";
     links<-links[-c(25, 153:155),]}
   
  names_units<-links[,1][grepl("[A-Z]", links[,1])]
  links_towns<-paste(path_link, links[,2][grepl("*t.htm|brinoctp.htm|usaexttp.htm|usas-dep.htm",links[,2])], sep="")
  
  if(i==6){
     links_towns <- links_towns[-8]}

  dd2 <- data.frame(Regions[i], names_units, links_towns, stringsAsFactors = FALSE)
  country_links[pointer:(pointer+dim(dd2)[1]-1),] <- dd2
  pointer = pointer+dim(dd2)[1]
  
  }

#write.csv(country_links, "popstatlinks.csv")
popstatlinks<-read.csv("popstatlinks.csv", row.names=NULL, stringsAsFactors = FALSE)
popstatlinks<-popstatlinks[,-1]

#save city files
setwd("C:/Users/Kirill/popstat_files")
  for(i in 1:278){
     url_to_download <- popstatlinks[i,3]
     webpage <- tryCatch(xml2::read_html(url_to_download), error = function(e) e)
     if(inherits(webpage,  "error")) next
     
     html_file = htmlParse(webpage)
     file_name = file(paste(popstatlinks[i,1], "_", popstatlinks[i,2], ".htm", sep=""))
 
     sink(file_name)
     print(html_file)
     sink()
     close(file_name)}


#Assemble dataset
setwd("C:/Users/Kirill/popstat_files")
mylistfiles=list.files()

DColNames <- c("COUNTRY.NAME", "PLACE.NAME")
ResultsTable <- data.frame(matrix(NA,50000,700))
colnames(ResultsTable) <- DColNames
problfile = ""
pointer = 1

for(i in 1:length(mylistfiles)){
  
  myhtml = htmlParse(mylistfiles[i],  encoding="UTF-8")
  
  if(i==147) next
  
  myhtml_table <- readHTMLTable(myhtml,header=FALSE, stringsAsFactors = FALSE)
  howmany <- length(myhtml_table)
  
  if(howmany == 0) next
  mylast_table <- myhtml_table[[which.max(nchar(myhtml_table))]]
  
  if(any(grepl("muni|mun|agglo", c(unlist(mylast_table[1:3,-c(1:2)]))))){
    filter <- which(apply(mylast_table[1:3,],2,function(x) any(grepl("muni|mun|agglo", x))))
    filter[filter<2]<-NA; filter<-filter[!is.na(filter)]
    mylast_table <- mylast_table[,-filter]
  }
  
  if(any(is.na(mylast_table[1,]))){
    mylast_table<-mylast_table[,!is.na(mylast_table[1,])]}
  colnames(mylast_table)<-
    gsub("X|\\*\\?|", "", mylast_table[1,]); mylast_table<-mylast_table[-1,]
  if(any(colnames(mylast_table)=="")){
    mylast_table<-mylast_table[,!colnames(mylast_table)==""]}
  
  colnames(mylast_table)[1]<-"PLACE.NAME"; 
  mylast_table<-data.frame(apply(mylast_table,2,function(x) gsub(",", ".",x)))
  mylast_table<-mylast_table[!grepl("source|^\\s*$", mylast_table[,1]),]  #remove extra vertical space
  col.names<-gsub("X", "",colnames(mylast_table))

  col.names[grepl("\\d{4}", col.names)]<-paste("Y_",unlist(regmatches(col.names, gregexpr("([A-z]\\d{4})|\\d{4}", col.names))), sep="")
  
    if(any(duplicated(col.names))){
      dupl<-unique(col.names[duplicated(col.names)])
        for(j in 1:length(dupl))
               col.names[which(col.names%in%dupl[j])[-1]]<-paste(dupl[j],seq(1, sum(col.names%in%dupl[j])-1, 1), sep=".")
    }
  
    colnames(mylast_table)<-col.names  #standardize year-names
    mylast_table<-mylast_table[,!apply(mylast_table, 2, function(x) sum(is.na(x)|x==""|x==" "))==dim(mylast_table)[1]]
    col.names<-colnames(mylast_table)
    
    if(any(!col.names %in% colnames(ResultsTable))){  #any col names to add?
        colstoadd<-col.names[!col.names %in% colnames(ResultsTable)]
        colnames(ResultsTable)[which(is.na(colnames(ResultsTable)))[1:length(colstoadd)]]<-colstoadd}
    
    for(ind in 1:length(col.names)){
      ResultsTable[pointer:(pointer+dim(mylast_table)[1]-1), 
                   which(colnames(ResultsTable)%in%col.names[ind])] <- as.character(mylast_table[,ind])}
    
     ResultsTable[pointer:(pointer+dim(mylast_table)[1]-1),1] <- mylistfiles[i]
    
     pointer = pointer + dim(mylast_table)[1]
  }


#Explore dimensions    
ResultsTable2 <- ResultsTable[1:33606,
                            1:length(names(ResultsTable)[!is.na(names(ResultsTable))])] #for subscript e,c
ResultsTable2$V2[ResultsTable2$COUNTRY.NAME=="Africa_MAYOTTE.htm"] <- NA        
ResultsTable2 <- ResultsTable2[!grepl("see:|See:|also:|SEE:", ResultsTable2$province),]
years<-paste("Y_", unique(sort(unlist(regmatches(names(ResultsTable2), 
                                                 gregexpr("\\d{4}",
                                                          names(ResultsTable2)))))), sep="")

#Main Vector

mentioned_first <- ResultsTable2$year.first
year_of_city_charter <- ResultsTable2$year.of
#particulars
particulars <- apply(subset(ResultsTable2, 
                          select=c("particulars.of.the.data", 
                                   "particulars", "particulars...variants.of.the.name")), 1, 
                   function(x) paste(unique(x), collapse = "_")) 
particulars <- gsub("NA\\_+|_", "",  particulars)
particulars <- gsub("_", "",  particulars)
particulars[particulars==""|particulars=="NA"]<-NA

#found
found <- apply(subset(ResultsTable2, select = c("found.yr","found..yr", 
                                              "found..year", "foundation.year..or", 
                                              "foundation.year", "founded")), 
             1, function(x) paste(unique(x), collapse = "_")) 
found <- gsub("NA\\_+|_NA", "",  found)
found <- gsub("_", "",  found)
found <- gsub("^\\s*|\\s*$","", found)
found[found==""|found=="NA"]<-NA

#variants
variants<-apply(subset(ResultsTable2, 
                       select=c("variants.of.the.name",  "variants.of.the.place.name", 
                                "variants.of.the.name.1", 
                                "variants.of.the.name..mainly..founded.as...thus.a.historical.name..")), 1, 
                function(x) paste(unique(x), collapse = "_")) 
variants<-gsub("NA\\_+|_NA", "",  variants)
variants<-gsub("_", "",  variants)
variants<-gsub("^\\s*|\\s*$","", variants)
variants[variants==""|variants=="NA"]<-NA


#subnational_unit
colnames(ResultsTable2)[142]<-"pref2"  
colnames(ResultsTable2)[284]<-"region2"

# without c, i subscripts
subnational <- apply(
  subset(ResultsTable2, 
         select=c("prov.", "region", "district", "province",  "pref2",  
                  "island",  "dept.",  "state", "county",
                  "region2",   "parish", "depto.",  "province.1",  "dep.",  
                  "canton",  "prefecture", "zone..or.district", "atoll",
                  "emirate", "tinh..province.", "kraj", "maakon",  "arrond.",
                  "estado", "amt", "Zila", "muhaf.",  "div..state",  "vilayet", "marz",
                  "fylke", "wojw.", "judet", "country.", "V2", 
                  ".Land.", "county","municipality", "....Wilayat", "ostan")), 1, 
  function(x) paste(unique(x), collapse = "_"))

subnational <- gsub("NA\\_+|_NA", "",  subnational)
subnational <- gsub("_|> see\\:|\\*", "",  subnational)
subnational <- gsub("^\\s*|\\s*$","", subnational)
subnational[subnational==""|subnational=="NA"] <- NA


#Working with .1 years
ResultsTable3<-subset(ResultsTable2, 
                      select = c("COUNTRY.NAME", "PLACE.NAME", 
                               names(ResultsTable2)[grepl("\\d{4}",names(ResultsTable2))]))
dat.split<-split(ResultsTable3, ResultsTable3$COUNTRY.NAME)

res<-lapply(dat.split, function(x){
  subs<-names(x)[apply(x,2,function(x)sum(!is.na(x)))>0]
  subs2<-subs[grepl("\\d{4}\\.\\d",subs)];
  return(subs2)})

dat.split[[which(names(dat.split)%in%"Americas_GUATEMALA.htm")]]
res[[which(names(dat.split)%in%".htm")]]

#Put data in order
ResultsYears <- subset(ResultsTable2, 
                       select=c(names(ResultsTable2)[grepl("\\d{4}",names(ResultsTable2))]))
ResultsYears2 <- data.frame(apply(ResultsYears, 2, 
                                  function(x) as.numeric(gsub("[A-z]*|([A-z]+[[:punct:]]+)| ", 
                                                              "", as.character(x)))*1000))
colnames(ResultsYears2) <- colnames(ResultsYears)
File.name <- ResultsTable2$COUNTRY.NAME
COUNTRY.NAME <- gsub("^.*\\_|\\.htm", "", ResultsTable2$COUNTRY.NAME)
PLACE.NAME <- ResultsTable2$PLACE.NAME
elevation <- ResultsTable2$elevation
position <- ResultsTable2$position
surface.sq.km <- ResultsTable2$surface.sq.km
surface.sq.mi <- ResultsTable2$surface.sq.mi
Extracted_Names <- NA
 
Auxuliary_data<-data.frame(File.name, COUNTRY.NAME, PLACE.NAME, subnational, 
                           Extracted_Names, variants, 
                           particulars, 
                           mentioned_first, year_of_city_charter, found,  elevation,
                           position, surface.sq.km, surface.sq.mi,
                           stringsAsFactors = FALSE)
ResultsGeneral<-data.frame(Auxuliary_data, ResultsYears2, stringsAsFactors = FALSE)
ResultsGeneral2<-ResultsGeneral[, c(colnames(Auxuliary_data), sort(colnames(ResultsYears2)))]
ResultsGeneral2<-ResultsGeneral2[!(apply(ResultsGeneral2[,which(grepl("^Y\\_", names(ResultsGeneral2)))],1, function(x) sum(!is.na(x)))==0),]
ResultsGeneral2<-ResultsGeneral2[!grepl("Total|total|TOTAL|agglomeration|agglom\\.|
                                         agglom|municip|aglomeration|commune|subdistrict|canton|
                                         statistical district|municipal|some more parts|cantón|subdistrict|zona urbana|
                                        SEE REFERENCES|See References|NAME VARIANT|--- -|Name variant|
                                        SEE REFEENCES|VARIANT|inland water|NORTHERN IRELAND|SCOTLAND|
                                        GUERNSEY|JERSEY|ISLE OF MAN", 
                                        ResultsGeneral2$PLACE.NAME),]
ResultsTable2<-ResultsTable2[-which(grepl("cantón", ResultsTable2$PLACE.NAME)),]
sort(ResultsGeneral$PLACE.NAME[ResultsGeneral$COUNTRY.NAME=="COSTA RICA"])

#Data cleaning
# Al-, el- etc
AlCities1.1 <- ResultsGeneral2$PLACE.NAME  #32339
AlCities1.1 <- gsub("\\.\\.\\.", "",AlCities1.1)
AlCities1.1[!grepl("\\ -|(\\.*Al-$)|(\\.*El-$)|
                   (\\.*Et-$)|(\\.*As-)|(\\.*At-)|(\\.*L'-)|(\\.*Ash-)|
                   (\\.*El$)|(\\.*Al-\\.)|(\\.*Les-\\.)|(\\.*La-)|(\\.*Le-)", 
                   AlCities1.1)]<-"" #remove all except la-,le-
AlCities1.2 <- gsub("\\(|\\;", ".", AlCities1.1)
AlCities1.2 <- gsub("\\)", "", AlCities1.2)
AlCities1.2 <- paste(gsub(" ", "", gsub("^.*\\.", "\\1", AlCities1.2)), 
                   gsub("\\..*$", "\\1", AlCities1.2), sep="")
pol1 <- gsub("\\(|\\)|-", "", AlCities1.1[ResultsGeneral2$COUNTRY.NAME=="POLAND"]); 
AlCities1.2[ResultsGeneral2$COUNTRY.NAME=="POLAND"]<-gsub("\\s+", " ", pol1)
AlCities1.2[AlCities1.2==""]<-ResultsGeneral2$PLACE.NAME[AlCities1.2==""]
ResultsGeneral2$PLACE.NAME<-AlCities1.2

AlCities1.3 <- sub("\\(\\)", "", regmatches(ResultsGeneral2$PLACE.NAME, 
                                          gregexpr("\\(.*?\\)", ResultsGeneral2$PLACE.NAME)))
AlCities1.3 <- gsub("character\\(0\\)","",AlCities1.3)
AlCities1.3 <- gsub("\\(|\\)","",AlCities1.3)
ResultsGeneral2$PLACE.NAME <- sub("\\(.*?\\)", "", ResultsGeneral2$PLACE.NAME)

AlCities1.4 <- sub("\\[\\]", "", regmatches(ResultsGeneral2$PLACE.NAME, 
                                            gregexpr("\\[.*?\\]", ResultsGeneral2$PLACE.NAME)))
AlCities1.4 <- gsub("character\\(0\\)","", AlCities1.4)
AlCities1.4 <- gsub("\\[|\\]","", AlCities1.4)
ResultsGeneral2$PLACE.NAME <- sub("\\[.*?\\]", "", ResultsGeneral2$PLACE.NAME)


AlCities1.5 <- gsub("/|or\\:", "", regmatches(ResultsGeneral2$PLACE.NAME, 
                                            gregexpr("/.*?$", ResultsGeneral2$PLACE.NAME)))
AlCities1.5 <- gsub("character\\(0\\)", "", AlCities1.5)
AlCities1.5 <- gsub("\\[|\\]", "", AlCities1.5)
AlCities1.5 <- gsub("\\)", "", AlCities1.5)
AlCities1.5 <- gsub("^\\s*|\\s*$","", AlCities1.5)
ResultsGeneral2$PLACE.NAME <- gsub("/|or\\:", "", ResultsGeneral2$PLACE.NAME)

ResultsGeneral2$PLACE.NAME <- gsub("\\s+;\\s+", ". ", ResultsGeneral2$PLACE.NAME)
AlCities1.7 <- AlCities1.6<-ResultsGeneral2$PLACE.NAME
AlCities1.6 <- gsub("\\s+", " ", AlCities1.6)
AlCities1.7 <- gsub("\\s+", " ", AlCities1.7)
AlCities1.6[!grepl("\\.", AlCities1.6)] <- ""  
AlCities1.7[!grepl("\\.", AlCities1.7)] <- "" 
ResultsGeneral2$PLACE.NAME[grepl("[A-z]{3}\\. +[A-z]{1}", 
                                 ResultsGeneral2$PLACE.NAME)]<-
  unlist(lapply(strsplit(AlCities1.6[grepl("[A-z]{3}\\. +[A-z]{1}", 
                                           AlCities1.6)], "\\."), function(x)x[1]))

AlCities1.7[grepl("[A-z]{3}\\. +[A-z]{1}", AlCities1.7)]<-
  unlist(lapply(strsplit(
    AlCities1.7[grepl("[A-z]{3}\\. +[A-z]{1}", AlCities1.7)], "\\."), 
    function(x){
      if(length(x)==1){xR<-NA}else{xR<-paste(x[-1], collapse=",")}; 
      return(xR)}))

AlCities1.7 <- gsub("^\\s*|\\s*$", "", AlCities1.7)
AlCitiesG <- paste(AlCities1.3, AlCities1.4, AlCities1.5, AlCities1.7, sep="_")
AlCitiesG <- gsub("_{2}|_{3}|_{4}|_{5}|^_|_$", "", AlCitiesG)
AlCitiesG <- gsub("_", ", ", AlCitiesG)
ResultsGeneral2$Extracted_Names <- AlCitiesG

ResultsGeneral2$PLACE.NAME <- gsub("\\*|\\.", "", ResultsGeneral2$PLACE.NAME)
ResultsGeneral2 <- ResultsGeneral2[!grepl("SEE REFERENCES|> see:|agglom",ResultsGeneral2$PLACE.NAME),]

#remove agglomerations with particulars
ResultsGeneral3 <- ResultsGeneral2[!grepl("SEE REFERENCES|> see:|agglom",
                                          ResultsGeneral2$particulars),]
ResultsGeneral3 <- ResultsGeneral3[!grepl("SEE REFERENCES|> see:|agglom",
                                          ResultsGeneral2$variants),]
ResultsGeneral3$variants<-gsub("\\.", ",", ResultsGeneral3$variants)

#write.csv(ResultsGeneral3, "populstat_data.csv", fileEncoding="UTF-8")

#less than 20000 remove from data
dat.20 <- ResultsGeneral3[,15:dim(ResultsGeneral3)[2]]
dat.20[dat.20 < 20000]<-NA
dat.20m <- data.frame(ResultsGeneral3[,1:14],dat.20)
dat.20m <- dat.20m[!(apply(dat.20m[,which(grepl("^Y\\_", names(dat.20m)))], 1, 
                           function(x) sum(!is.na(x)))==0),]
#save special characters
getOption("encoding")
Encoding(dat.20m$PLACE.NAME) <- "UTF-8"
write.table(dat.20m$PLACE.NAME,"Var.Place.Name2.txt",
            row.names=F,col.names=F,quote=F,fileEncoding="UTF-8")

#variants
Encoding(dat.20m$variants) <- "UTF-8"
write.table(dat.20m$variants,"Var.Variants2.txt",
            row.names=F,col.names=F,quote=F,fileEncoding="UTF-8")

#particulats
getOption("encoding")
Encoding(dat.20m[,3]) <- "UTF-8"
write.table(dat.20m$particulars,"Var.Particulars2.txt",
            row.names=F,col.names=F,quote=F,fileEncoding="UTF-8")

#dat1<-read.csv("populstat_data20000.csv", stringsAsFactors = FALSE, encoding="UTF-8")
dat1 <- dat.20m

usstatesU<-toupper(c("Alabama",	"Alaska",	"Arizona",	"Arkansas",	"California", "Colorado",	"Connecticut",	"Delaware",
            "Florida",	"Georgia",	"Hawaii",	"Idaho",	"Illinois", "Indiana",	"Iowa",	"Kansas",	"Kentucky",	"Louisiana",
            "Maine",	"Maryland",	"Massachusetts",	"Michigan",	"Minnesota",	"Mississippi",	"Missouri",	"Montana",	"Nebraska",	
            "Nevada",	"New Hampshire",	"New Jersey",	"New Mexico",	"New York",	"North Carolina",	"North Dakota",	"Ohio",
            "Oklahoma",	"Oregon",	"Pennsylvania",	"Rhode Island",	"South Carolina",	"South Dakota",	"Tennessee",	"Texas",
            "Utah",	"Vermont",	"Virginia",	"Washington",	"West Virginia",	"Wisconsin",	"Wyoming", "DISTRICT OF COLUMBIA"))

dat1$subnational[dat1$COUNTRY.NAME%in%usstatesU] <- state2abbr(dat1$COUNTRY.NAME[dat1$COUNTRY.NAME%in%usstatesU])

dat1$COUNTRY.NAME[(dat1$COUNTRY.NAME%in%usstatesU)&(!dat1$File.name=="Europe_GEORGIA.htm")] <- "USA"
dat1$File.name == "Europe_GEORGIA.htm"
RegionsK <- toupper(dat1$subnational)
CitiesK <- toupper(dat1$PLACE.NAME)

dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="CENTRAL AFRICAN REPUBLIC"]<-"CENTRAL AFRICAN REP."
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="CONGO"]<-"CONGO DR" 
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="COTE d'IVIORE"]<-"COTE D'IVOIRE" 
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="EQUATORIAL GUINEA"]<- "EQ. GUINEA"
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="SWAZILAND"]<- "SWAZILA"
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="THE GAMBIA"]<- "GAMBIA" 
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="DOMINICAN REPUBLIC"]<- "DOMINICAN REP."                      
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="UNITED STATES OF AMERICA"]<- "USA"
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="SOUTH KOREA"]<- "KOREA" 
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="NORTH KOREA"]<- "KOREA NORTH"
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="The MALDIVES"]<- "MALDIVES" 
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="The PHILIPPINES"]<- "PHILIPPINES"
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="UNITED KINGDOM"]<- "UK"
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="CZECH REPUBLIC"]<- "CZECH REP."
dat1$COUNTRY.NAME[dat1$COUNTRY.NAME=="LUXEMBOURG"]<- "LUXEMBURG"
CountryK<-dat1$COUNTRY.NAME

#Check duplicates with Index
indexKCC <- paste(RegionsK, CitiesK, CountryK, sep="_")
indexKCC[duplicated(indexKCC)]
#remove
dat1<-dat1[!duplicated(indexKCC),]
indexKC<-paste(CitiesK, CountryK, sep="_")
indexKC[duplicated(indexKC)]
#Work with duplicates
KC_duplicates<-indexKC[duplicated(indexKC)]
#in indexRC, <region identifier> is added to all duplicates 
for(i in 1:length(KC_duplicates)){
  indexKC[indexKC%in%KC_duplicates[i]] <- 
    paste(CitiesK[indexKC%in%KC_duplicates[i]], 
    RegionsK[indexKC%in%KC_duplicates[i]], 
    CountryK[indexKC%in%KC_duplicates[i]], sep="_")
  }

dat2.3<-data.frame(RegionsK, CitiesK, CountryK, indexKC)

#Save all the data with index
Regions_popst<-RegionsK[!duplicated(indexKCC)]
Cities_popst<-CitiesK[!duplicated(indexKCC)]
Countries_popst<-CountryK[!duplicated(indexKCC)]
Index_popst<-indexKC[!duplicated(indexKCC)]
Index_popst2<-Index_popst

#index without countries
dat1.2 <- 
  data.frame(Regions_popst, Cities_popst, Countries_popst, Index_popst, Index_popst2, dat1)
ListCountries <- unique(dat1.2$Countries_popst)
   
for(j in 1:length(ListCountries)){  
     if(any(grepl(ListCountries[j],Index_popst2))){ 
       vv <- 
         gsub(paste("_+",ListCountries[j], sep=""), "", 
              Index_popst2[grepl(ListCountries[j],Index_popst2)])
        Index_popst2[grepl(ListCountries[j],Index_popst2)] <- as.character(vv)
     }}

dat1.2 <- data.frame(Regions_popst, Cities_popst, Countries_popst, 
                     Index_popst, Index_popst2, dat1, stringsAsFactors = FALSE)

dat1.2$Index_popst[dat1.2$Countries_popst=="USA"]<-
  paste(dat1.2$Cities_popst[dat1.2$Countries_popst=="USA"], 
        dat1.2$Regions_popst[dat1.2$Countries_popst=="USA"], 
        dat1.2$Countries_popst[dat1.2$Countries_popst=="USA"], sep="_")

dat1.2$Index_popst2[dat1.2$Countries_popst=="USA"]<-
  paste(dat1.2$Cities_popst[dat1.2$Countries_popst=="USA"], 
        dat1.2$Regions_popst[dat1.2$Countries_popst=="USA"], sep="_")

dat1.2$Index_popst<-gsub("NA_NA", "", dat1.2$Index_popst)
dat1.2$Index_popst2<-gsub("_NA", "", dat1.2$Index_popst2)

#YUGOSLAVIA ISSUE
Yug <- read.csv("Yugoslavia.csv", stringsAsFactors = FALSE)
Yug$Index2[Yug$Countries=="YUGOSLAVIA"][!Yug$Index2[
  Yug$Countries=="YUGOSLAVIA"]%in%Yug$Index2[Yug$Countries!="YUGOSLAVIA"]]
Yug$PC <- unlist(lapply(strsplit(Yug$MV1, "_"), function(x) x[2]))

  for(i in 1:dim(Yug)[1]){
    dat1.2$Index_popst2[dat1.2$Index_popst%in%Yug$Index[i]]<-Yug$Index2[i]
    dat1.2$Countries_popst[dat1.2$Index_popst%in%Yug$Index[i]]<-Yug$PC[i]
    dat1.2$COUNTRY.NAME[dat1.2$Index_popst%in%Yug$Index[i]]<-Yug$PC[i]
    dat1.2$Index_popst[dat1.2$Index_popst%in%Yug$Index[i]]<-paste(Yug$Index2[i],Yug$PC[i], sep="_")
  }

dat1.2<-dat1.2[order(dat1.2$Countries_popst, dat1.2$Index_popst),] #order the data

#write.csv(dat1.2, "popst_data2_indexed.csv", fileEncoding="UTF-8")

getOption("encoding")
var1<-as.character(dat1.2$Cities_popst)
Encoding(var1) <- "UTF-8"
write.table(var1,"popst_data_Cities_popst2.txt",row.names=F,col.names=F,quote=F,fileEncoding="UTF-8")

var1<-as.character(dat1.2$Index_popst)
Encoding(var1) <- "UTF-8"
write.table(var1,"popst_data_Index_popst2.txt",row.names=F,col.names=F,quote=F,fileEncoding="UTF-8")

var1<-as.character(dat1.2$Index_popst2)
Encoding(var1) <- "UTF-8"
write.table(var1,"popst_data_Index_popst22.txt",row.names=F,col.names=F,quote=F,fileEncoding="UTF-8")

var1<-as.character(dat1.2$PLACE.NAME)
Encoding(var1) <- "UTF-8"
write.table(var1,"popst_data_Place.Name2.txt",row.names=F,col.names=F,quote=F,fileEncoding="UTF-8")

var1<-as.character(dat1.2$Extracted_Names)
Encoding(var1) <- "UTF-8"
write.table(var1,"popst_data_Extracted_Names2.txt",row.names=F,col.names=F,quote=F,fileEncoding="UTF-8")

var1<-as.character(dat1.2$variants)
Encoding(var1) <- "UTF-8"
write.table(var1,"popst_data_variants2.txt",row.names=F,col.names=F,quote=F,fileEncoding="UTF-8")

var1<-as.character(dat1.2$Regions_popst)
Encoding(var1) <- "UTF-8"
write.table(var1,"popst_data_Regnions.txt",row.names=F,col.names=F,quote=F,fileEncoding="UTF-8")

var1<-as.character(dat1.2$particulars)
Encoding(var1) <- "UTF-8"
write.table(var1,"popst_data_Particulars.txt",row.names=F,col.names=F,quote=F,fileEncoding="UTF-8")