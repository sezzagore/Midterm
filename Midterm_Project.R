## ----code=readLines(knitr::purl('Midterm_Project.Rmd', documentation = 1)), eval = FALSE, include=FALSE----
## # the code in the description of the chunck automatically creates/saves a R script from this .Rmd
## # for some reason, this seems to disallow naming further chuncks

## ---- message=FALSE------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE, cache = TRUE)
require(ggplot2) 
require(tidyr)
require(dplyr)
require(readr) # to read text
require(foreign) # library used to import the dbfs
require(lubridate) # library used for dates/times

## ---- warning=FALSE------------------------------------------------------
# read the data and transform in tibble (for speed and better behaviour of the data frame)
osha <-  read.dbf("osha.dbf", as.is = TRUE) %>%
  tbl_df 
accid <- read.dbf("accid.dbf", as.is = TRUE) %>%
  tbl_df
viol <- read.dbf("viol.dbf", as.is = TRUE) %>%
  tbl_df

join_1 <- left_join(osha, accid, by = "ACTIVITYNO")
osha <- left_join(join_1, viol, by = "ACTIVITYNO")
rm(list = setdiff(ls(), "osha")) # I erase my environment and now only have osha in my environment 

## ------------------------------------------------------------------------
osha <-  read.dbf("osha.dbf", as.is = TRUE) %>%
  tbl_df 
accid <- read.dbf("accid.dbf", as.is = TRUE) %>%
  tbl_df
viol <- read.dbf("viol.dbf", as.is = TRUE) %>%
  tbl_df

## ------------------------------------------------------------------------
clean.from.one.value <- function(df){
  if(!is.data.frame(df)){
    print("Not executed: the function requires a data frame object.")
    return()
  }
  one.value <- function(vec){length(unique(vec))==1}
  clean.df <- df[,!sapply(df, one.value)]
  if(length(df)-length(clean.df)==0) {
    print("No variable removed.")
  } else {print(paste(length(df)-length(clean.df),"variables removed:",  length(df),"->",length(clean.df) ))
    }
  return(clean.df)
}

## ---- results= "hold"----------------------------------------------------
osha <- clean.from.one.value(osha)
accid <- clean.from.one.value(accid)
viol <- clean.from.one.value(viol)

## ------------------------------------------------------------------------
irrelavant_variables <- c("OSHA1MOD", "OPENDATE", "CLOSEDATE", "CLOSEDATE2",  "FRSTDENYN", "LSTREENTRN",
 "PENDUDATE", "FTADUDATE", "FRSTCONTST")

osha <- osha[,!(names(osha) %in% irrelavant_variables)]
rm(irrelavant_variables)

## ------------------------------------------------------------------------
irrelavant_variables <- c("PROG_", "RELACT_","OPTINFO_", "DEBT_", "EVENT_", "HAZSUB_", "ADMPAY_")

osha <- osha[,!(names(osha) %in% irrelavant_variables)]
rm(irrelavant_variables)

## ------------------------------------------------------------------------
irrelavant_variables <- c("PAPREP" ,"PATRAVEL" ,"PAONSITE", "PATECHSUPP" ,"PARPTPREP","PAOTHRCNF","PALITIGTN","PADENIAL" ,"PASUMHOURS")

osha <- osha[,!(names(osha) %in% irrelavant_variables)]
rm(irrelavant_variables)

## ------------------------------------------------------------------------
irrelavant_variables <- c("PENDUDATE", "PENDUDT", "FTADUDATE", "FTADUDT", "DUECODE")

osha <- osha[,!(names(osha) %in% irrelavant_variables)]
rm(irrelavant_variables)

## ------------------------------------------------------------------------
irrelavant_variables <- c("PENREMIT", "FTAREMIT", "TOTPENLTY", "TOTALFTA")

osha <- osha[,!(names(osha) %in% irrelavant_variables)]
rm(irrelavant_variables)

## ------------------------------------------------------------------------
irrelavant_variables <- c("DUNSNO", "HOSTESTKEY")

osha <- osha[,!(names(osha) %in% irrelavant_variables)]
rm(irrelavant_variables)

## ------------------------------------------------------------------------
irrelavant_variables <- c("SITESTATE", "NAME", "RELINSP", "SOURCE", "ENVIRON", "EVENT", "TASK", "HAZSUB", "OCC_CODE")

accid <- accid[,!(names(accid) %in% irrelavant_variables)]
rm(irrelavant_variables)

## ------------------------------------------------------------------------
irrelavant_variables <- c("SITESTATE", "DELETE", "ISSUANCE", "ISSUEDATE", "CITATION", "ITEMNO", "ITEMGROUP", "EMPHASIS", "PENCURRENT", "PENINITIAL", "STD_LOOKUP", "STD", "ABATE", "DATE_ABATE", "ABATEDT", "ABATEDT2", "REC", "ABATEDONE")

viol <- viol[,!(names(viol) %in% irrelavant_variables)]
rm(irrelavant_variables)

## ------------------------------------------------------------------------
irrelavant_variables <- c("ERCONTDT", "ERCONDATE", "VIOLCONT", "PENCONT", "EMPRCONT", "EMPECONT", "FINORDT", "FINORDATE", "PMA", "AMENDED", "ISA", "DISPEVT" )

viol <- viol[,!(names(viol) %in% irrelavant_variables)]
rm(irrelavant_variables)

## ------------------------------------------------------------------------
irrelavant_variables <- c("FTAINSP", "FTAPEN", "ISSUDT", "FTA_ISDT", "CONTDT", "CONTDATE", "FTA_AMN", "FTA_ISA", "FTA_DISP", "FTA_FIN", "FTAFINDT", "HAZCAT")

viol <- viol[,!(names(viol) %in% irrelavant_variables)]
rm(irrelavant_variables)

## ---- warning = FALSE----------------------------------------------------
# I load the lookup from scc 

scc <-  read.dbf("lookups/scc.dbf") %>%
  tbl_df %>%
  filter(STATE=="MA")
counties <-  read.dbf("lookups/scc.dbf") %>%
  tbl_df %>% filter(STATE=="MA") %>%
  filter(CITY == "0000", COUNTY!="000") %>%
  select(COUNTY, NAME) %>%
  rename(COUNTY_CODE =COUNTY, COUNTY_NAME= NAME) 
osha <- left_join(osha,counties, by=c("SITECNTY"="COUNTY_CODE"))
cities <- read.dbf("lookups/scc.dbf") %>%
  tbl_df %>%
  filter(STATE=="MA") %>%
  filter(CITY != "0000", COUNTY!="000") %>%
  select(CITY, NAME) %>%
  rename(CITY_CODE =CITY, CITY_NAME= NAME)
osha <- left_join(osha,cities, by=c("SITECITY"="CITY_CODE"))
rm(scc, counties, cities)

## ---- warning = FALSE----------------------------------------------------
# I load the lookup from acc 

acc <-  read.dbf("lookups/acc.dbf") %>%
  tbl_df %>%
  filter(CATEGORY=="PART-BODY") %>%
  rename(BODYPART_NAME = VALUE)
accid <- left_join(accid,acc, by=c("BODYPART"="CODE"))

acc <-  read.dbf("lookups/acc.dbf") %>%
  tbl_df %>%
  filter(CATEGORY=="NATURE-INJ") %>%
  rename(NATURE_NAME = VALUE)
accid <- left_join(accid,acc, by=c("NATURE"="CODE"))

acc <-  read.dbf("lookups/acc.dbf") %>%
  tbl_df %>%
  filter(CATEGORY=="HUMAN-FAC") %>%
  rename(HUMAN_NAME = VALUE)
accid <- left_join(accid,acc, by=c("HUMAN"="CODE"))
rm(acc)

## ------------------------------------------------------------------------
blank_variables <- c("CONTFLAG", "STFLAG", "PREVCTTYP", "WALKAROUND", "INTRVIEWD", "CLOSECASE", "SAFETYMANF", "SFTYCONST", "STFYMARIT", "HELTHMANF", "HELTHCONST", "HELTHMARIT", "MIGRANT", "ANTCSRVD")
blank_variables <- blank_variables[blank_variables %in% colnames(osha)]
osha[,blank_variables][is.na(osha[blank_variables])] <- "NO"
rm(blank_variables)

## ------------------------------------------------------------------------
accid %>%
  ggplot(aes(x = AGE)) +
  geom_bar(alpha = 0.7) +
  labs(title = "Distribution of Age of Workers in Accidents Prior to Correction", x = "Age", y = "Number of Workers")

## ------------------------------------------------------------------------
accid$AGE[accid$AGE == 0] <- NA

## ---- warning=FALSE------------------------------------------------------
accid %>%
  ggplot(aes(x = AGE)) +
  geom_bar(alpha = 0.7) + 
  labs(title = "Distribution of Age of Workers in Accidents After Correction", x = "Age", y = "Number of workers")

## ------------------------------------------------------------------------
osha %>%
  group_by(COUNTY_NAME) %>%
  summarise(accidents = sum(ACCID_)) %>%  
  filter(!is.na(COUNTY_NAME)) %>%
  arrange(-accidents) %>%
  mutate(COUNTY_NAME = factor(COUNTY_NAME, COUNTY_NAME)) %>%
  ggplot(aes( x = COUNTY_NAME, y = accidents)) +
  geom_bar(alpha = 0.7, stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of Accidents Per County (1972-2006)", x = "County", y = "Number of Accidents")

## ------------------------------------------------------------------------
osha %>%
  group_by(CITY_NAME) %>%
  summarise(accidents = sum(ACCID_)) %>%  
  filter(!is.na(CITY_NAME)) %>%
  arrange(-accidents) %>%
  top_n(n = 15, accidents) %>%
  mutate(CITY_NAME = factor(CITY_NAME, CITY_NAME)) %>%
  ggplot(aes( x = CITY_NAME, y = accidents)) +
  geom_bar(alpha = 0.7, stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of Accidents in the Top 15 Cities (1972-2006)", x = "City", y = "Number of Accidents")

## ------------------------------------------------------------------------
osha %>%
  group_by(year(OPENDT)) %>%
  summarise(accidents = sum(ACCID_), year = mean(year(OPENDT))) %>%
  filter(year >1972, year < 2006) %>%
  ggplot(aes(x = year, y = accidents)) +
  geom_line() + labs(title = "Number of Accidents over Time (1973-2005)", x = "Year", y = "Number of Accidents")

## ------------------------------------------------------------------------
osha %>%
  group_by(year(OPENDT), COUNTY_NAME) %>%
  filter(!is.na(COUNTY_NAME)) %>%
  summarise(accidents = sum(ACCID_), year = mean(year(OPENDT))) %>%
  filter(year >1972, year < 2006) %>%
  ggplot(aes(x = year, y = accidents, col = COUNTY_NAME, group = COUNTY_NAME)) +
  geom_line() +
  labs(title = "Number of Accidents over Time (1973-2005)", x = "Year", y = "Number of Accidents", col = "County" )

## ------------------------------------------------------------------------

osha %>% 
  group_by(year(OPENDT)) %>%
  summarise(violations = sum(VIOLS_), year = mean(year(OPENDT))) %>%
  filter(year >1972, year < 2006) %>%
  ggplot(aes(x = year, y = violations)) +
  geom_line() +
  labs(title = "Number of Violations over Time (1973-2005)", x = "Year", y = "Number of Violations")

## ------------------------------------------------------------------------

accid %>%
  filter(!is.na(SEX)) %>%
  ggplot(aes(x = SEX, y= AGE, fill = SEX)) +
  geom_boxplot() +
  labs(title = "Distribution of Age of Victims by Gender", x = "Sex", y = "Age") + guides(fill = FALSE)

## ------------------------------------------------------------------------
accid %>%
  group_by(BODYPART_NAME) %>%
  summarise(cases = n())  %>%
  arrange(-cases)%>% top_n(n = 15, cases) %>%
  mutate(BODYPART_NAME = factor(BODYPART_NAME, BODYPART_NAME)) %>%
  filter(!is.na(BODYPART_NAME))  %>%
  ggplot(aes( x = BODYPART_NAME, y = cases)) +
  geom_bar(alpha = 0.7, stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Breakdown of Top 15 Body-Parts Harmed", x = "Body-Part Harmed in the Accident", y = "Number of Cases")

## ------------------------------------------------------------------------
text <- read_lines("layouts/OSHA.txt", skip = 10)
text1 <- gsub("\\t\t\t.*","", text )
text2 <- text1[grepl("\t", text1)]
text3 <- gsub("\\t\t","\t", text2) %>%
  strtrim(100) %>%
  tbl_df %>%
  separate(value, c("variable", "type", "length", "description"), sep = "\t" )
text3$variable <- gsub(" ", "", text3$variable, fixed = TRUE) 
text3 <- text3  %>%
  select(variable, description) %>%
  arrange(variable) 
osha_descriptions <- text3 %>%
  filter( variable %in% names(osha))
knitr::kable(osha_descriptions, caption = "Description of Variables in `osha`")

## ------------------------------------------------------------------------
text <- read_lines("layouts/ACCID.txt", skip = 9)
text1 <- gsub("\\t\t\t\t.*","", text )
text2 <- gsub("\\t\t","\t", text1 )
text2 <- gsub("\\t\t","\t", text2 )
text2 <- gsub("\t$","", text2 )
text3 <- text2[grepl("\t", text2)]
text3 <-  text3 %>% tbl_df %>% separate(value, c("variable", "type", "length", "description"), sep = "\t" ) %>% select(variable, description) %>% arrange(variable) 
accid_descriptions <- text3 %>% filter( variable %in% names(accid)) 
knitr::kable(accid_descriptions, caption = "Description of Variables in `accid`")

## ------------------------------------------------------------------------
text <- read_lines("layouts/VIOL.txt", skip = 7)
text1 <- gsub("\\t\t\t\t.*","", text )
text2 <- gsub("\\t\t","\t", text1 )
text2 <- gsub("\\t\t","\t", text2 )
text2 <- gsub("\t$","", text2 )
text3 <- text2[grepl("\t", text2)]
text3 <-  text3 %>% tbl_df %>% separate(value, c("variable", "type", "length", "description"), sep = "\t" ) %>% select(variable, description) %>% arrange(variable) 
viol_descriptions <- text3 %>% filter( variable %in% names(viol)) 
knitr::kable(viol_descriptions, caption = "Description of Variables in `viol`")
rm(text, text1, text2, text3)

