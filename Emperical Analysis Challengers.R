#This edition of the Script adds histrical boarders, and am working on better weighting of MII and adding India and Brazil
#CCodes: Russia 365, China 710, India 750, South Africa 560, Brazil 140, Iran 645, Iraq 630, Saudi Arabia 670, Japan 740, 770 Pakistan
#Icow for W? Prediction Market?
#zero-inflated negative binomial

#Add UK and France? 200 and 220 ROK is 732
#Add Chile, Argentina, Australia
#put in some numbers for W? sqrt((NukesA+1)*(NukesB+1))-1
#Create China POV


rm(list=ls())

#Libraries needed
library(MASS)
library(data.table)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(rsq)
library(pdftools)
library(tesseract)
library(conflicted)
library(stargazer)
library(AER)

#Resolving conflicts should packages be loaded in the wrong order
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("recode", "dplyr")
require(pscl)

#Location of data files
setwd("F:/data")

#Initializes analysis years, and desired rising powers
Start_Year = 1950
End_Year = 2010
years = Start_Year:2010
RisingCodes = c(365,710,750,560,140,740,630,645,670,770,200,220,732)

#adding troop data
Troops=fread("TroopDep2.csv",header=T,data.table=F,na.strings=c("-","unknown"),select=c("Country",as.character(c(1950:2005))))%>%#Unknowns could be non-zero
  pivot_longer(-Country,names_to = "Year", values_to = "us_troops")
Troops$ccode = countrycode(Troops$Country,"country.name","cown")

#appending recent troop data
additionaltroops = fread("Troop_placements_2016.csv", header = T,select=c("country","ccode","Year","us_troops"),col.names = c("Country","ccode","Year","us_troops"))
Troops = rbind(Troops,additionaltroops)%>%replace_na(list(us_troops = 0))%>%arrange(Country)
Troops = Troops%>%arrange(ccode)%>%fill(us_troops, .direction = c("down")) #Only imputed case is Germany in 2009

#loading naval data for effectiveness at containing
naval = get(load("crisher-souva_navaldata_II.RData"))
USNavy = naval[((naval$cowcode==2)&(naval$year>=Start_Year&naval$year<=End_Year)),]

#Loading military Capacity data
MilCap_data=fread("NMC_5_0.csv", header=T, na.strings = "-9")
#Filtering to relevant years and countries, replacing NA with $200,000 since NA countries tend to be small or from 1992
MilCap_data = MilCap_data%>%filter((year>=Start_Year-10)&(year<=End_Year))#%>%replace_na(list(milex = 200000))

#Code for making Challengers top cinc scores
Challengers = MilCap_data%>%filter(year>1949)%>%group_by(ccode)%>%summarize(max(cinc))%>%arrange(desc(`max(cinc)`))
Challengers$Country = countrycode(Challengers$ccode,"cown","country.name")
Challengers = Challengers[1:32,]
RisingCodes = Challengers[[1]]
RisingCodes = RisingCodes[-c(1,19)]#Removing U.S., Canada

#Code for Included Countries Table
#Challengers = Challengers[2:3]
#colnames(Challengers) = c("Max CINC","Country")
#Challengers[1]=round(Challengers[1],4)
#Challengers = Challengers[-c(1,19),]
#stargazer(Challengers, summary = F)

#Interpolating military capacity data: if 2010 is missing it is given the average, then years filled down from newest, closest point of data
MilCap_data[MilCap_data$milex==0] = NA #Recoding 0 to missing
MilCap_data = MilCap_data%>%select(c("ccode","year","milex","cinc"))
MilCap_data = na.omit(MilCap_data)
interpolation = MilCap_data%>%group_by(ccode)%>%summarize(mean(milex),mean(cinc))#Mean of all known expenditure data by country
colnames(interpolation) = c("ccode","milex","cinc")
interpolation$year = 2010

MilCap_data = MilCap_data%>%complete(year,ccode)%>%arrange(ccode,year)
interpolation = interpolation%>%filter(ccode %in% MilCap_data[is.na(MilCap_data$milex)&(MilCap_data$year==2010),]$ccode)#Selecting only unknown countries
MilCap_data = na.omit(MilCap_data)
MilCap_data = rbind(MilCap_data,interpolation)#Adding Interpolations
MilCap_data = MilCap_data%>%complete(year,ccode)%>%arrange(ccode,year)%>%fill(c(milex,cinc),.direction=c("up"))#filling unknowns

#Importing Protege score data
Protegescore = fread("Major Protege Dataset.csv",select=c("ccode1","ccode2","year","mean"))
#Cleaning
Protegescore = Protegescore[(Protegescore$year>= Start_Year&Protegescore$year <= End_Year)&as.numeric(Protegescore$ccode1==2)]

#so we can modify the milex of aggressed countries by US_Interest
MilCap_data_2 = MilCap_data
MilCap_data_2 = left_join(MilCap_data_2,Protegescore, by = (c("ccode"="ccode2","year")))%>%select(-c("ccode1"))
colnames(MilCap_data_2) = c(colnames(MilCap_data_2)[1:4],"US_Interest")
interpolation = na.omit(MilCap_data_2)%>%group_by(ccode)%>%summarize(mean(US_Interest))
MilCap_data_2$US_Interest[MilCap_data_2$ccode == 2] = max(na.omit(MilCap_data_2$US_Interest))
MilCap_data_2$US_Interest[MilCap_data_2$ccode %in% c(710,365)] = min(na.omit(MilCap_data_2$US_Interest))
ryears = MilCap_data_2%>%filter(((year==2010) & (is.na(US_Interest))))%>%select(ccode)
interpolation = interpolation%>%filter(ccode %in% ryears$ccode)
ryears = MilCap_data_2%>%filter(((year==2010) & (is.na(US_Interest))))%>%mutate(US_Interest = interpolation[[2]])
MilCap_data_2 = rbind(MilCap_data_2,ryears)%>%arrange(ccode,year)%>%fill(c(US_Interest),.direction=c("up"))
MilCap_data_2$US_Interest[MilCap_data_2$US_Interest<.01] = 0.01 #making negative interests into .01 because it doesn't seem like the option to attack US enemies should lower aggressive capacity vs US interests, and 0 breaks math
MilCap_data_2 = MilCap_data_2%>%mutate(US_Val = US_Interest/milex)

#Calculating A:power of Rising Power vs surrounding countries
#Calculating aggregate chinese border military spending
ContiguityData = fread("contdird.csv",header=T,data.table=F)#Loading in Contiguity Data
ContiguityData = ContiguityData %>% filter((year>=Start_Year)&(year<=End_Year)) %>% filter(state1no %in% RisingCodes) %>% filter(state2no != 2)#subsetting to only countries in RisingCodes vector historical boarders between Start and 2010 and not including US


#**************Calculating A and E******************
#*#Downloading and merging Protege data, US troop data, and US naval data into one dataframe
#Merging
Troops = Troops%>%mutate(Year = as.numeric(Year))%>%right_join(Protegescore, by = c("Year"="year","ccode"="ccode2"))%>%arrange(ccode,Year)%>%mutate(us_troops = as.numeric(us_troops))
Troops = Troops%>%fill(c(Country,us_troops), .direction = c("down"))#imputing incomplete data
Troops = merge(Troops,USNavy,by.x = "Year",by.y = "year")
#Cleaning
Troops = Troops%>%select(c(ccode,Country,Year,us_troops,mean,tonn_prop))
colnames(Troops) = c("ccode","Country","Year","US_Troops","protegelevel","USnavySize")
#Creating defense score from by multiplying these measures together
#Troops are logged as diminishing marginal utility is assumed, Protege level can be negative so it's given exponential transform, and navy size stays the same as it has no problems.
Troops = Troops%>%mutate(defensescore = log(as.numeric(Troops$US_Troops)+1)*exp(Troops$protegelevel)*Troops$USnavySize)

#Downloading and cleaning trade data 
##Take out US GDP
Trade_data=fread("Dyadic_COW_4.0.csv", header=T, select=c())
Trade_data = Trade_data %>% filter(year %in% Start_Year:End_Year) %>% filter(ccode1 == 2) %>% filter(ccode2 %in% RisingCodes)#cutting to be the length of only chosen years and US dyads
Trade_data[Trade_data==-9] = NA #Recoding -9 to missing
GDP_data=fread("National-GDP-Penn.csv", header=T,col.names = c("Entity","Code","year","GDP"))%>%filter((year>= Start_Year&year <= End_Year))%>%filter(Code == "USA")#Adding US GDP data to create proportion of trade variable
Trade_data = Trade_data%>%left_join(GDP_data, by ="year")%>%mutate(TradeOverUSGDP = smoothtotrade/GDP) %>% select(TradeOverUSGDP,year,ccode2)
#Trade_data[is.na(Trade_data$TradeOverUSGDP),]
#Imputing missing data
Trade_data$TradeOverUSGDP[(Trade_data$ccode2==260)&(Trade_data$year==1990)] = Trade_data$TradeOverUSGDP[(Trade_data$ccode2==260)&(Trade_data$year==1959)]#Setting West Germany's missing trade to the last of known year
Trade_data$TradeOverUSGDP[(Trade_data$ccode2==265)&(Trade_data$year==1990)] = Trade_data$TradeOverUSGDP[(Trade_data$ccode2==265)&(Trade_data$year==1989)]#Setting East Germany's missing trade to the last of known year
Trade_data = Trade_data%>%fill(TradeOverUSGDP, .direction = c("up")) #Imputed cases and given values are DPRK 1950-1985 (0), Vietnam 1950-1951 (3.558523e-11) 1953-1954 (3.558523e-11) 1956-1959 (3.571130e-11), Ukraine in 1991 (4.707993e-11), Taiwan 1954-1959 (2.049746e-11)

#Downloading and cleaning inflation data
Inflation_data=fread("OECDInflation.csv", header=T, select=c())
Inflation_data$ccode=countrycode(as.vector(Inflation_data[[1]]),"iso3c","cown")
Inflation_data = Inflation_data%>%select(c("ccode","TIME","Value"))%>%filter(is.na(ccode) == 0)%>%filter(ccode %in% RisingCodes)
colnames(Inflation_data) = c("ccode","year","Inflation")

#For interpolating
#Inflation_data%>%complete(year,ccode)%>%arrange(year,ccode)
#mean(Inflation_data$Inflation)
#which.max(Inflation_data$Inflation)
#Inflation_data[is.na(Inflation_data$ccode),]

#***************initializing variables
Border.value=as.numeric()
Ag.Util = data.table()
Aggressive.Utility = as.numeric()
Defense.Effectiveness = as.numeric()
Defense.Effectiveness1 = as.numeric()
Delta.Troops1 = as.numeric()

#Runs two loops, inner loop is every year, outer loop is each country
#at the completion all years for a country, all variables are added to the Aggressive.Utility frame
for(j in 1:length(RisingCodes)){
  #Calculating A: for China- the balance of power between rising power's spending and all boarder spending
  for(i in years){
    #Boarder value is equal to the  sum of (US protege score / military expenditures of border states(of any type given by COW) in the given year through the past 10 years to give current strength of the military
    Border.value[i-Start_Year+1] = sum(MilCap_data_2$US_Val[(MilCap_data_2$ccode %in% as.vector(t(select(filter(ContiguityData,((ContiguityData$state1no == RisingCodes[j])&(ContiguityData$year==i))),state2no))))&(MilCap_data_2$year %in% (i-10):i)])
    
    #A: Aggressive.Utility is the ratio of the 10 year rolling military expenditures of the Rising power divided by those of their boarder states(divided by border length) giving the average ratio of military power between the rising power and it's boarders
    #"as.vector(t(select(filter(ContiguityData,((ContiguityData$state1no = 365)&(ContiguityData$year==i))),state2no)))" is the vector of countries along the boarder at given year
    Aggressive.Utility[i-Start_Year+1] = sum(MilCap_data$milex[(MilCap_data$ccode == RisingCodes[j])&(MilCap_data$year %in% (i-10):i)]*Border.value[i-Start_Year+1])
    
    #E: Defense.Effectiveness is the sum of defense scores divided by number of countries
    Defense.Effectiveness1[i-Start_Year+1] = sum(Troops$defensescore[(Troops$ccode%in% as.vector(t(select(filter(ContiguityData,((ContiguityData$state1no == RisingCodes[j])&(ContiguityData$year==i))),state2no))))&(Troops$Year == i)])/length(as.vector(t(select(filter(ContiguityData,((ContiguityData$state1no == RisingCodes[j])&(ContiguityData$year==i))),state2no))))
    #Sum of US troops in rising power border states at that time divided by length of border at the time
    if(i>1950){
      Delta.Troops1[i-Start_Year+1] = sum(Troops$US_Troops[(Troops$ccode %in% as.vector(t(select(filter(ContiguityData,((ContiguityData$state1no == RisingCodes[j])&(ContiguityData$year==i))),state2no))))&(Troops$Year == i-1)])-
        sum(Troops$US_Troops[(Troops$ccode %in% as.vector(t(select(filter(ContiguityData,((ContiguityData$state1no == RisingCodes[j])&(ContiguityData$year==i))),state2no))))&(Troops$Year == i)])
    }
    else{
      Delta.Troops1[i-Start_Year+1] = NA
    }
  }
  Aggressive.Utility1 = data.table(Aggressive.Utility)
  Aggressive.Utility1$ccode = rep(RisingCodes[j],length(years))
  Aggressive.Utility1$year = years
  Aggressive.Utility1$Defense.Effectiveness = Defense.Effectiveness1
  Aggressive.Utility1$Delta.Troops = Delta.Troops1
  Ag.Util = rbind(Ag.Util,Aggressive.Utility1)
}
Ag.Util[is.na(Ag.Util$Defense.Effectiveness),]
Ag.Util = Ag.Util%>%merge(Trade_data, by.x = c("ccode","year"), by.y = c("ccode2","year"))#adding on trade
Ag.Util = Ag.Util%>%merge(MilCap_data, by.x = c("ccode","year"), by.y = c("ccode","year"))%>%select(-c(milex))#adding on cinc

#****************Adding in democracy data
Polity_data = readxl::read_xls("Polity5.xls")%>%
  select(ccode,year,polity2)%>% #Cleaning data
  filter((year >= Start_Year&year <= End_Year)&(ccode %in% c(RisingCodes,364)))%>%#subsetting to rising powers (USSR is 364) and years
  mutate(ccode=recode(ccode, "364"=365))%>%#Coding USSR to Russia ccode
  arrange(ccode,year)

#****************Using NSS data to determine how realist the US is at different times
NSSyears = c(1987,1988,1990,1991,1993,1994,1995,1996,1997,1998,2000,2001,2003,2006,2010)
RealWords = c("alone", "attack", "attacks", "attacked", "attacking", "annihilate", "annihilation", "conflict", "conflicts", "dark", "destroy",
              "destroyed", "destruction", "enemy", "enemies", "evil", "evils", "force", "forced", "forcing", "hegemony", "hegemon", "hegemonic",
              "lose", "military","militant", "militaristic", "power", "powerful", "realism", "realist", "survival", "survive", "strong", "strength",
              "strongest", "strike", "strikes", "unipolar", "unipolarity", "unilateral", "unilateralism", "war", "warfare", "wars", "win", "zero-sum")
LibWords = c("aid", "aided", "aiding", "agree", "agreed", "agreement", "ally", "allied", "allies", "alliance", "assist", "assisted", "assistance",
             "assisting", "benefit", "benefited", "beneficial", "cooperate", "cooperated", "cooperation", "collaborate", "collaborating",
             "collaborated", "collaboration", "convention", "democracy", "disaster", "democratic", "economic", "economy", "global", "help", 
             "international", "law", "liberalism", "liberal", "multilateral", "multilateralism", "mutual", "peace", "peaceful", "right", "rights",
             "together", "trade", "United Nations", "UN")
RisingWords = data.frame("ccode" = RisingCodes, "cname" = c(countrycode(RisingCodes,"cown","country.name")))
RisingWords$cname[1] = "China|Chinese|PRC|People's Republic of China|CCP|Chinese Communist Party|Beijing"
RisingWords$cname[2] = "USSR|Union of Soviet Socialist Republics|Soviet Union|Soviet|Russia|Russian Federation|Russian|Moscow"
RisingWords$cname[3] = "India|Indian|New Delhi"
RisingWords$cname[4] = "U.K.|United Kingdom|England|Britain|English|British|London"
RisingWords$cname[5] = "Japan|Japanese|Tokyo"
RisingWords$cname[6] = "Federal Republic of Germany|FRG|BRD|West Germany|Bonn"
RisingWords$cname[7] = "France|French|Paris"
RisingWords$cname[8] = "Germany|German|Berlin"
RisingWords$cname[9] = "Brazil|Brasil|Brazilian|Brasilian"
RisingWords$cname[10] = "South Korea|Republic of Korea|ROK|South Korean|Seoul"
RisingWords$cname[11] = "Ukraine|Ukrainian|Kiev|Kyiv"
RisingWords$cname[12] = "Italy|Italian|Rome"
RisingWords$cname[13] = "Indonesia|Indonesian|Jakarta"
RisingWords$cname[14] = "Poland|Polish|Warsaw"
RisingWords$cname[15] = "Turkey|Turkish|Ankara"
RisingWords$cname[16] = "Mexico|Mexican|Mexico City"
RisingWords$cname[17] = "Pakistan|Pakistani|Islamabad"
RisingWords$cname[18] = "Iran|Iranian|Tehran"
RisingWords$cname[19] = "North Korea|North Korean|Democratic People's Republic of Korea|Pyongyang"
RisingWords$cname[20] = "Saudi Arabia|Arabian|Riyadh"
RisingWords$cname[21] = "Spain|Spanish|Madrid"
RisingWords$cname[22] = "Vietnam|Vietnamese|Ho Chi Minh City|Saigon"
RisingWords$cname[23] = "Iraq|Iraqi|Baghdad"
RisingWords$cname[24] = "Czechoslovakia|Czechoslovak|Czech|Slovak|Prague"
RisingWords$cname[25] = "German Democratic Republic|GDR|DDR|East Germany|East Berlin|East German"
RisingWords$cname[26] = "Taiwan|Taiwanese|ROC|Republic of China|Taipei"
RisingWords$cname[27] = "Bangladesh|Bangladeshi|Dhaka"
RisingWords$cname[28] = "Thailand|Thai|Bangkok"
RisingWords$cname[29] = "Egypt|Egyptian|Cairo"
RisingWords$cname[30] = "Romania|Romanian|Bucharest"
#Implementing OCR to determine frequency of words used in each report (Takes awhile to run)

HawkDove = data.frame(year = rep(NSSyears,each=length(RisingCodes)), ccode = rep(RisingCodes,length(NSSyears)*length(RisingCodes)), totnum = rep(0,length(NSSyears)*length(RisingCodes)),hawknum = rep(0,length(NSSyears)*length(RisingCodes)),dovenum = rep(0,length(RisingCodes)*length(NSSyears)))
for(j in 1:length(NSSyears)){
  nssstxt = pdf_text(paste0("NSS/nss", as.character(NSSyears[j]),".pdf"))
  nssstxt = gsub("\r\n", " ", nssstxt)
  nssstxt = str_c(nssstxt, sep = " ", collapse = T)
  nssstxt = strsplit(nssstxt, " ")
  nssstxt = nssstxt[[1]]
  nssstxt = str_replace_all(nssstxt,"[^a-zA-Z]", "")
  nssstxt = nssstxt[nssstxt != ""]
  
  HawkDove$totnum[((j-1)*length(RisingCodes)+1):(j*length(RisingCodes))] = length(nssstxt)
  HawkDove$hawknum[((j-1)*length(RisingCodes)+1):(j*length(RisingCodes))] = length(grep(paste(RealWords,collapse="|"), nssstxt,ignore.case=TRUE))
  HawkDove$dovenum[((j-1)*length(RisingCodes)+1):(j*length(RisingCodes))] = length(grep(paste(LibWords,collapse="|"), nssstxt,ignore.case=TRUE))
  for(i in 1:length(RisingCodes)){
    HawkDove$risingnum[(j-1)*length(RisingCodes)+i] = length(grep(RisingWords$cname[i], nssstxt,ignore.case=TRUE))
  }
  if(length(nssstxt) == 1){
    nssstxt = pdf_ocr_text(paste0("NSS/nss", as.character(NSSyears[j]),".pdf"),language = "eng",dpi = 600)
    nssstxt = str_c(nssstxt, sep = " ", collapse = T)
    nssstxt = strsplit(nssstxt, " ")
    nssstxt = nssstxt[[1]]
    nssstxt = str_replace_all(nssstxt,"[^a-zA-Z]", "")
    nssstxt = nssstxt[nssstxt != ""]
    HawkDove$totnum[((j-1)*length(RisingCodes)+1):(j*length(RisingCodes))] = length(nssstxt)
    HawkDove$hawknum[((j-1)*length(RisingCodes)+1):(j*length(RisingCodes))] = length(grep(paste(RealWords,collapse="|"), nssstxt,ignore.case=TRUE))
    HawkDove$dovenum[((j-1)*length(RisingCodes)+1):(j*length(RisingCodes))] = length(grep(paste(LibWords,collapse="|"), nssstxt,ignore.case=TRUE))
    for(i in 1:length(RisingCodes)){
      HawkDove$risingnum[(j-1)*length(RisingCodes)+i] = length(grep(RisingWords$cname[i], nssstxt,ignore.case=TRUE))
    }
  }
}

HawkDove = HawkDove%>%mutate(hawkrate = hawknum/(hawknum+dovenum)) #Calculates proportion of Hawk to dove words in each National Security strategy
HawkDove = HawkDove%>%mutate(risingrate = risingnum/totnum*100) #Calculates country mentions per 100 words
HawkDove = HawkDove[1:(length(NSSyears)*length(RisingCodes)),]#Trims off end, don't know where it came from

#***************Code to save data so it doesn't need to be recompiled later
write_csv(HawkDove, file = "F:/data/NSS/NSSdatChallengers.csv", na = "NA", append = FALSE)
#Code to read in already compiled data
HawkDove = read.csv("F:/data/NSS/NSSdatChallengers.csv")
#***************
#Interpolating Data: takes the average before 1987, then extrapolates last measurement after 1987
HawkDove = left_join(Ag.Util,HawkDove, by = c("year","ccode"))%>%select(c("year","ccode","hawkrate","risingrate"))%>%complete(year,ccode)%>%arrange(year,ccode) #adding hawkishness
HawkDove$hawkrate[1:((1987-Start_Year)*length(RisingCodes))] = replace_na(HawkDove$hawkrate,mean(HawkDove$hawkrate[!is.na(HawkDove$hawkrate)]))  #or the mean if before 1987
HawkDove = HawkDove%>%fill(hawkrate, .direction = "down") #filling in missing data with most up to date NSS data at that time

#Interpolating rising power mentions, hopefully this code doesn't break because its garbage
x = HawkDove%>%filter(!is.na(risingrate))%>%group_by(ccode)%>%summarize(mean(risingrate))
colnames(x) = c("ccode","risingrate2")
x = as.data.table(HawkDove[1:((1987-Start_Year)*length(RisingCodes)),])%>%left_join(x, by="ccode")%>%select("risingrate2")
HawkDove$risingrate[1:((1987-Start_Year)*length(RisingCodes))] = x$risingrate2[1:((1987-Start_Year)*length(RisingCodes))]
HawkDove = HawkDove%>%arrange(ccode,year)%>%fill(risingrate, .direction = "down")

#**********************Downloading Incident data for the dependent variable: How often Rising Powers have MIIs
MII_data=fread("GML-DirectedDyadYear-v2.2.1.csv", header=T)%>%
  select(ccode1,ccode2,year,hostlev1,hostlev2,hostlev)%>% #Cleaning data
  filter((year >= Start_Year&year <= End_Year)&(ccode1 %in% RisingCodes))%>%#subsetting to the 2 rising powers and years
  filter(hostlev1 >1)#removing cases where country was aggressed and didn't respond
#Adding on protege level
MII_data = left_join(MII_data,Troops, by = c("year"="Year","ccode1"="ccode"))
MII_data = MII_data%>%select(colnames(MII_data)[c(1:6,9)])
MII_data = left_join(MII_data,Troops, by = c("year"="Year","ccode2"="ccode"))
MII_data = MII_data%>%select(colnames(MII_data)[c(1:7,10)])
MII_data$protegelevel.y[MII_data$ccode2 == 2] = max(na.omit(MII_data$protegelevel.y))#US is given the max protege score
#China and Russia are given the minimum protege score
MII_data = replace_na(MII_data, list(protegelevel.y = min(na.omit(MII_data$protegelevel.y)), protegelevel.x = min(na.omit(MII_data$protegelevel.y))))
MII_data = MII_data%>%mutate(protege.diff = protegelevel.y-protegelevel.x)
MII_data$protege.diff[MII_data$protege.diff<0] = 0 #If protege.diff is less than 0, then set to zero because they US isn't interested but negative interest here screws up sums later
#Could have issue where 0 is inflated by non-events

#Finding number of MII events for each year
MII.Num = aggregate(MII_data, by = list(MII_data$year,MII_data$ccode1), FUN=length)%>%select(c("Group.1","Group.2","hostlev"))%>%complete(Group.1,Group.2)%>%replace_na(list(hostlev=0))#adding in no event years
colnames(MII.Num) = c("year","ccode","MII_Nums")#fixing column names broken by aggregate

#Finding sum of MII event hostilities for each year
MII.Host = aggregate(MII_data, by = list(MII_data$year,MII_data$ccode1), FUN="sum")%>%select(c("Group.1","Group.2","hostlev","protege.diff"))%>%complete(Group.1,Group.2)%>%replace_na(list(hostlev=0))#adding in no event years
colnames(MII.Host) = c("year","ccode","MII_Host","protege.diff")#fixing column names broken by aggregate
MII.Host = left_join(MII.Host,MII.Num, by=c("year","ccode"))
MII.Host$protege.diff = MII.Host$protege.diff/MII.Host$MII_Nums
MII.Host$protege.diff[is.na(MII.Host$protege.diff)] = mean(na.omit(MII.Host$protege.diff))#giving Years with no conflict the mean value

#***********************Adding nuke data
Nuclear = read.csv("nuclear_warheads_1945_2016.csv", sep=";", dec=",")#
Nuclear$United.States = as.numeric(str_replace_all(Nuclear$United.States,"[.]", ""))
Nuclear$Russia = as.numeric(str_replace_all(Nuclear$Russia,"[.]", ""))
Nuclear = pivot_longer(Nuclear,cols = colnames(Nuclear)[2:10], names_to = "country", values_to = "bombs")
Nuclear$Nuclear_Power = 1-as.numeric(is.na(Nuclear$bombs))            #Indicates if nuclear power
Nuclear$bombs[is.na(Nuclear$bombs)] = 0                               #No bombs now 0 not NA
Nuclear$ccode = countrycode(Nuclear$country,"country.name","cown")    #Adding ccode
Nuclear = Nuclear%>%select(c("ccode","Year","Nuclear_Power","bombs"))

#Creating us_bombs variable that tracks how many nukes the US has
USNukes = Nuclear%>%filter(ccode==2)%>%select("Year","bombs")
colnames(USNukes) = c("Year","us_bombs")
Nuclear = Nuclear%>%left_join(USNukes, by = "Year")

#*********************MERGING DATA****************************
#Merging MII aggregations with the 3 model variables
Reg_Data = data.frame(Ag.Util)#renaming

#Merging DVs
Reg_Data = merge(Reg_Data,MII.Host, by = c("year","ccode")) #adding number of MIIs*Hostility

#Merging IVs
Reg_Data = left_join(Reg_Data,ContiguityData%>%group_by(state1no,year)%>%summarise(n_border=n()), by = c("ccode"="state1no","year"))#Determining the number of borders in each year
Reg_Data = merge(Reg_Data,Polity_data, by = c("year","ccode"))             #adding democracy score
Reg_Data = left_join(Reg_Data,Nuclear, by = c("year"="Year","ccode"))      #adding if nuclear power
Reg_Data$Nuclear_Power[is.na(Reg_Data$Nuclear_Power)] = 0                  #States not in data are not nuclear powers
Reg_Data$bombs[is.na(Reg_Data$bombs)] = 0                                  #States not in data are not nuclear powers
Reg_Data = Reg_Data%>%fill(us_bombs, .direction = "up")
Reg_Data = left_join(Reg_Data,HawkDove, by = c("year","ccode"))            #adding hawkishness

colnames(Reg_Data) = c("Year","ccode","Aggressive.Utility","Defense.Effectiveness","Delta.Troops","Trade","CINC","MII_HLev","US_Interest","MII_Nums","N_Borders","Democracy","Nuclear","Rising_Bombs","US_Bombs","Hawkish","Mentions") #renaming columns
Reg_Data = Reg_Data%>%mutate(Containment = max(Defense.Effectiveness)-Defense.Effectiveness)%>%select(-c("Defense.Effectiveness")) # reversing from containment effectiveness to aggressive effectiveness given containment
Reg_Data$YearAdj = Reg_Data$Year-Start_Year 
Reg_Data$Democracy = Reg_Data$Democracy+11                                 #Making all polity scores positive
Reg_Data$Trade = Reg_Data$Trade*100000000                                  #Trade data was in millions but GDP in dollars, now a percent of GDP
Reg_Data$Defense.Effectiveness = Reg_Data$Defense.Effectiveness*.1         #Bringing Defense in line with scale of trade
Reg_Data = Reg_Data%>%mutate(After1991=Year%/%1992)
Reg_Data = Reg_Data%>%mutate(Country = countrycode(ccode,"cown","country.name"))
Reg_Data = Reg_Data%>%mutate(US_Bombs = log(US_Bombs+2))%>%mutate(Rising_Bombs = log(Rising_Bombs+2))
Reg_Data_Copy = Reg_Data
Reg_Data = Reg_Data%>%filter((ccode !=732)|(Year>1951))#removing ROK before 1952, no border data
Reg_Data = Reg_Data%>%filter((ccode !=750)|(Year>1952))#removing India before 1953, no democracy data
Reg_Data = Reg_Data%>%fill(US_Bombs)#For some reason there's no data on the last three entries for US_Bombs
Reg_Data$Country[is.na(Reg_Data$Country)]="Federal Republic of Germany"#FRG not programmed into package for cown
Reg_Data = Reg_Data%>%mutate(Aggressive.Utility = Aggressive.Utility*US_Interest)
Reg_Data = Reg_Data%>%mutate(RiskofWar = US_Bombs*Rising_Bombs)#need a single value for war to adjust relative values

#Scaling Values
Scl_Data = Reg_Data%>%mutate(Aggressive.Utility = scale(Aggressive.Utility))%>%mutate(Trade = scale(Trade))%>%mutate(Rising_Bombs = scale(Rising_Bombs))%>%mutate(US_Interest = scale(US_Interest))%>%
  mutate(US_Bombs = scale(US_Bombs))%>%mutate(RiskofWar = scale(RiskofWar))%>%mutate(Hawkish = scale(Hawkish))%>%mutate(Mentions = scale(Mentions))%>%mutate(Containment = scale(Containment)) #Scaling all but Years, Democracy, Boarders
#Making T,A,W all positive so relative formula doesn't think negative numbers are big
x = -min(c(Scl_Data$Aggressive.Utility,Scl_Data$Trade,Scl_Data$RiskofWar))+.001
Scl_Data = Scl_Data%>%mutate(Aggressive.Utility = Aggressive.Utility+x)%>%
  mutate(Trade = Trade+x)%>%
  mutate(RiskofWar = RiskofWar+x)

#Adjusting T, A, W to be relative values
Rel_Data = Scl_Data%>%mutate(Aggressive.Utility=Aggressive.Utility/(Aggressive.Utility+Trade+RiskofWar))%>%
  mutate(Trade=Trade/(Aggressive.Utility+Trade+RiskofWar))%>%
  mutate(RiskofWar=RiskofWar/(Aggressive.Utility+Trade+RiskofWar))

#Rescaling Data relative data
Scl_Data = Rel_Data%>%mutate(Aggressive.Utility = scale(Aggressive.Utility))%>%mutate(Trade = scale(Trade))%>%mutate(RiskofWar = scale(RiskofWar))

#Creating Subsets of rising powers which reach 50% of US CINC in a dyad year and those that don't
MilCap_data = MilCap_data%>%filter(ccode == 2)%>%filter(year %in% 1950:2010)%>%select(c("year", "cinc"))
colnames(MilCap_data) = c("year","UScinc")
Scl_Data = Scl_Data%>%left_join(MilCap_data, by = c("Year" = "year"))%>%mutate(UScinc = CINC/UScinc)

Scl_Data_1 = Scl_Data%>%filter(UScinc >= (1/3)) #Data of dyads with rising CINC at least 1/3rd of US CINC
Scl_Data_2 = Scl_Data%>%filter(UScinc < (1/3)) #Data of dyads with rising CINC lower than 1/3rd of US CINC

#************************GRAPHING/SUMMARIZING VARIABLES*****************************
ggplot(Scl_Data) + theme_classic() + geom_line(aes(x=Scl_Data$Year,y=Scl_Data$Aggressive.Utility,color=Country), size = 1) + labs(x="Year", y="Aggressive Utility")
ggplot(Scl_Data) + theme_classic() + geom_line(aes(x=Scl_Data$Year,y=Scl_Data$Trade,color=Country), size = 1) + labs(x="Year", y="Trade")
ggplot(Scl_Data) + theme_classic() + geom_line(aes(x=Scl_Data$Year,y=Scl_Data$RiskofWar,color=Country), size = 1) + labs(x="Year", y="Risk of War")

ggplot(Scl_Data) + theme_classic() + geom_line(aes(x=Scl_Data$Year,y=Scl_Data$Containment,color=Country), size = 1) + labs(x="Year", y="Contain Effectiveness")
x = Scl_Data%>%group_by(Country)%>%summarise(mean(Containment))
ggplot(Reg_Data) + theme_classic() + geom_line(aes(x=Reg_Data$Year,y=Reg_Data$Democracy,color=Reg_Data$Country), size = 1) + labs(title="Distribution of Democracy Scores", x="Year", y="Polity Score + 11")
ggplot(Reg_Data) + theme_classic() + geom_line(aes(x=Reg_Data$Year,y=Reg_Data$Hawkish), size = 1) + labs(x="Year", y="Hawkishness")

ggplot(Reg_Data) + theme_classic() + geom_line(aes(x=Reg_Data$Year,y=Reg_Data$MII_Nums,color=Country), size = 1) + labs(title="Distribution of Incedents", x="Year", y="Incedents")
ggplot(Reg_Data) + theme_classic() + geom_line(aes(x=Reg_Data$Year,y=Reg_Data$MII_HLev,color=Country), size = 1) + labs(x="Year", y="Incedents")
ggplot(Reg_Data) + theme_classic() + geom_line(aes(x=Reg_Data$Year,y=sqrt(Reg_Data$MII_HLev),color=Country), size = 1) + labs(title="Distribution of Incedents weighted by Intensity", x="Year", y="Incedents")

#Graphing only select countries vs average in HLev
Average_Data = Reg_Data%>%group_by(Year)%>%summarise(mean(MII_HLev))
colnames(Average_Data) = c("Year","MII_HLev")
Graph_Data = Reg_Data%>%filter(ccode %in% c(365,710,740))
x = ggplot(Graph_Data) + theme_classic() + geom_line(aes(x=Graph_Data$Year,y=Graph_Data$MII_HLev,color=Country), size = 1) + labs(x="Year", y="Weighted Incedents") 
x + geom_line(data = Average_Data, aes(x=Average_Data$Year,y=Average_Data$MII_HLev, color="Average"), size = 1.5)

#Graphing only select countries vs RiskofWar
x = Scl_Data%>%group_by(ccode)%>%summarize(mean(RiskofWar))
Average_Data = Scl_Data%>%group_by(Year)%>%summarise(mean(RiskofWar))
colnames(Average_Data) = c("Year","RiskofWar")
Graph_Data = Scl_Data%>%filter(ccode %in% c(365,710,740,750))
x = ggplot(Graph_Data) + theme_classic() + geom_line(aes(x=Graph_Data$Year,y=Graph_Data$RiskofWar,color=Country), size = 1) + labs(x="Year", y="Risk Of War") 
x + geom_line(data = Average_Data, aes(x=Average_Data$Year,y=Average_Data$RiskofWar, color="Average"), size = 1.5)


#Graphing only select countries vs average in Trade
x = Reg_Data%>%group_by(ccode)%>%summarize(mean(Trade))
Average_Data = Reg_Data%>%group_by(Year)%>%summarise(mean(MII_HLev))
colnames(Average_Data) = c("Year","MII_HLev")
Graph_Data = Reg_Data%>%filter(ccode %in% c(365,710,740))
x = ggplot(Graph_Data) + theme_classic() + geom_line(aes(x=Graph_Data$Year,y=Graph_Data$MII_HLev,color=Country), size = 1) + labs(x="Year", y="Weighted Incedents") 
x + geom_line(data = Average_Data, aes(x=Average_Data$Year,y=Average_Data$MII_HLev, color="Average"), size = 1.5)

#Graphing Relative values
#USSR
Graph_Data=Scl_Data%>%filter(ccode == 365)%>%select(Trade,RiskofWar,Aggressive.Utility,Year)%>%pivot_longer(cols = c(Trade,RiskofWar,Aggressive.Utility), names_to = "Variable")
ggplot(Graph_Data) + theme_bw() + geom_line(aes(x=Graph_Data$Year,y=Graph_Data$value,color=Variable), size = 1) + labs(x="Year", y="Standardized Value") 
#Japan
Graph_Data=Scl_Data%>%filter(ccode == 740)%>%select(Trade,RiskofWar,Aggressive.Utility,Year)%>%pivot_longer(cols = c(Trade,RiskofWar,Aggressive.Utility), names_to = "Variable")
ggplot(Graph_Data) + theme_classic() + geom_line(aes(x=Graph_Data$Year,y=Graph_Data$value,color=Variable), size = 1) + labs(x="Year", y="Standardized Value") 
#China
Graph_Data=Scl_Data%>%filter(ccode == 710)%>%select(Trade,RiskofWar,Aggressive.Utility,Year)%>%pivot_longer(cols = c(Trade,RiskofWar,Aggressive.Utility), names_to = "Variable")
ggplot(Graph_Data) + theme_classic() + geom_line(aes(x=Graph_Data$Year,y=Graph_Data$value,color=Variable), size = 1) + labs(x="Year", y="Standardized Value") 
#India
Graph_Data=Scl_Data%>%filter(ccode == 750)%>%select(Trade,RiskofWar,Aggressive.Utility,Year)%>%pivot_longer(cols = c(Trade,RiskofWar,Aggressive.Utility), names_to = "Variable")
ggplot(Graph_Data) + theme_classic() + geom_line(aes(x=Graph_Data$Year,y=Graph_Data$value,color=Variable), size = 1) + labs(x="Year", y="Standardized Value") 

Graph_Data=Scl_Data%>%filter(ccode %in% c(365,710,740,750))%>%select(Country,Trade,RiskofWar,Aggressive.Utility,Year)%>%pivot_longer(cols = c(Trade,RiskofWar,Aggressive.Utility), names_to = "Variable")
ggplot(Graph_Data) + theme_bw() + geom_line(aes(x=Graph_Data$Year,y=Graph_Data$value,color=Variable), size = 1) + labs(x="Year", y="Standardized Value") + facet_wrap(~Country)


#Country specific mentions
x = Reg_Data[((1987-Start_Year)*length(RisingCodes)+1):nrow(Reg_Data),]
ggplot(x) + theme_classic() + geom_line(aes(x=x$Year,y=x$Mentions,color=x$Country), size = 1) + labs(title="Distribution of Mentions per 100 words in NSS", x="Year", y="Mentions per 100 words")


summary(Reg_Data$Aggressive.Utility)
summary(Reg_Data$Defense.Effectiveness)
summary(Reg_Data$Trade)
summary(Reg_Data$MII_HLev)
summary(Reg_Data$Democracy)
summary(Reg_Data$Hawkish)
summary(Reg_Data$Mentions)

#*********************REGRESSION MODELS****************************
# Current analysis: Poisson, All variables
Mpois = glm(MII_HLev~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj+Country, data=Reg_Data,family = poisson)
summary(Mpois) 
rsq(Mpois,adj=T)
dispersiontest(Mpois,trafo=1)
# Analysis w/o countries: Poisson
Mpois = glm(MII_HLev~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj, data=Reg_Data,family = poisson)
summary(Mpois) 
rsq(Mpois,adj=T)

#Using Conflict number
Mpois = glm(MII_Nums~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+After1991+Democracy+Nuclear+YearAdj+Country, data=Reg_Data,family = poisson)
summary(Mpois) # Current analysis, Add time
rsq(Mpois,adj=T)

ggplot(Reg_Data) + theme_classic() + geom_point(aes(x=Reg_Data$Year,y=(Reg_Data$preds-Reg_Data$MII_HLev),color=Reg_Data$Country)) + labs(title="Predicted Incedents weighted by Intensity Over Time", x="Year", y="Incedents")

#Sqrt Analysis
ggplot(Reg_Data) + theme_classic() + geom_point(aes(x=Reg_Data$Year,y=log(Reg_Data$MII_HLev+1),color=Reg_Data$Country)) + labs(title="Distribution of Incedents weighted by Intensity", x="Year", y="Incedents")
Mpois = glm(sqrt(MII_HLev)~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+After1991+Democracy+Nuclear+YearAdj+Country, data=Reg_Data,family = poisson)
summary(Mpois) # Current analysis, Add time
rsq(Mpois)

Mpois = glm(log(MII_HLev+1)~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+After1991+Democracy+Nuclear+YearAdj, data=Reg_Data,family = poisson)
Mpois = glm(log(MII_HLev+1)~Trade+Defense.Effectiveness+Aggressive.Utility+Hawkish+After1991+Democracy+Nuclear+YearAdj+Country, data=Reg_Data,family = poisson)

#Predictions
Reg_Data$preds = predict.glm(Mpois,newdata = Reg_Data, type = "response")
ggplot(Reg_Data) + theme_classic() + geom_smooth(aes(x=Reg_Data$Year,y=Reg_Data$preds,color=Country)) + labs(x="Year", y="Incedents")
ggplot(Reg_Data) + theme_classic() + geom_point(aes(x=Reg_Data$Year,y=(Reg_Data$preds-log(Reg_Data$MII_HLev+1)),color=Reg_Data$Country)) + labs(title="Predicted Incedents weighted by Intensity Over Time", x="Year", y="Incedents")


Mpois = glm(MII_HLev~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj+Country, data=Reg_Data,family = poisson)

#make it NB
Mnb = glm.nb(MII_HLev~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj+Country, data=Reg_Data)
summary(Mnb)
rsq(Mnb,adj = T)
#Predictions
Reg_Data$preds = predict.glm(Mnb,newdata = Reg_Data)
ggplot(Reg_Data) + theme_classic() + geom_point(aes(x=Reg_Data$Year,y=Reg_Data$preds,color=Reg_Data$Country)) + labs(title="Predicted Incedents weighted by Intensity Over Time", x="Year", y="Incedents")

#NB w/o countries
Mnb = glm.nb(MII_HLev~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj, data=Reg_Data)
summary(Mnb)
rsq(Mnb,adj = T,type='kl')
stargazer(Mnb,MSLR,no.space=T)
#Predictions
Reg_Data$preds = predict.glm(Mnb,newdata = Reg_Data, type = "response")
ggplot(Reg_Data) + theme_classic() + geom_point(aes(x=Reg_Data$Year,y=Reg_Data$preds,color=Reg_Data$Country)) + labs(title="Predicted Incedents weighted by Intensity Over Time", x="Year", y="Incedents")

# As SLR
MSLR = glm(MII_HLev~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj+Country, data=Reg_Data,family = gaussian)
summary(MSLR) 
rsq(MSLR,adj=T)
MSLR = glm(MII_HLev~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj, data=Reg_Data,family = gaussian)
summary(MSLR) 
rsq(MSLR,adj=T)

#Reg_Data$Trade = Reg_Data$Trade/100 #division by 100 to get back to ratio
Mpois = glm(MII_HLev~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+After1991+Democracy+Nuclear+YearAdj+Country, data=Reg_Data,family = poisson)
Newdata = Reg_Data
Newdata$Trade = seq(from=min(Reg_Data$Trade),to=max(Reg_Data$Trade),by=(max(Reg_Data$Trade)/(nrow(Reg_Data)-1)))
Newdata$Defense.Effectiveness = rep(mean(Reg_Data$Defense.Effectiveness),nrow(Reg_Data))
Newdata$Aggressive.Utility = rep(mean(Reg_Data$Aggressive.Utility),nrow(Reg_Data))
Newdata$After1991 = rep(1,nrow(Reg_Data))
Newdata = Newdata%>%filter(ccode == 710)
pred = predict(Mpois,newdata = Newdata,se.fit=TRUE)
val = exp(pred$fit)
plot(x=Newdata$Trade,y=val)

theme_set(theme_grey() + theme(panel.background = element_rect(fill = NA, color = 'black'))+ theme(axis.text=element_text(size=14),axis.title=element_text(size=15,face="bold")))

ggplot(Newdata) + geom_line(aes(x=Trade,y=val)) + labs(x="Bilateral trade proportional to US GDP", y="Incidents")+ geom_ribbon(aes(x=Newdata$Trade,ymin=val-exp(pred$se.fit)*2,ymax=val+exp(pred$se.fit)*2,fill="95% Confidence Interval"),alpha=.25)


#graphing trade curve
Mzinb = zeroinfl(MII_HLev~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+I(Aggressive.Utility^3)+US_Interest+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj | Country,
                 data = Scl_Data, dist = "negbin", EM = F)
Newdata = Scl_Data

Newdata$Aggressive.Utility = rep(mean(Scl_Data$Aggressive.Utility),nrow(Scl_Data))
Newdata$US_Interest = rep(mean(Scl_Data$US_Interest),nrow(Scl_Data))
Newdata$RiskofWar = rep(mean(Scl_Data$RiskofWar),nrow(Scl_Data))
Newdata$Containment = rep(mean(Scl_Data$Containment),nrow(Scl_Data))
Newdata$Hawkish = rep(mean(Scl_Data$Hawkish),nrow(Scl_Data))
Newdata$Mentions = rep(mean(Scl_Data$Mentions),nrow(Scl_Data))
Newdata$Democracy = rep(mean(na.omit(Scl_Data$Democracy)),nrow(Scl_Data))
Newdata$N_Borders = rep(mean(Scl_Data$N_Borders),nrow(Scl_Data))
Newdata$YearAdj = rep(mean(Scl_Data$YearAdj),nrow(Scl_Data))
Newdata$After1991 = rep(1,nrow(Scl_Data))
Newdata = Newdata%>%filter(ccode == 710)
Newdata$Trade = seq(from=min(Scl_Data$Trade),to=max(Scl_Data$Trade),by=((max(Scl_Data$Trade)-min(Scl_Data$Trade))/(nrow(Newdata)-1)))
pred = predict(Mzinb,  newdata = Newdata, se.fit=TRUE, interval = 'confidence')
val = as.numeric(pred)
ggplot(Newdata) + labs(x="Bilateral trade proportional to US GDP", y="Incidents")+ geom_line(aes(x=Newdata$Trade,y=val))


Mpois1 = glm(MII_Nums~Trade+Defense.Effectiveness+Aggressive.Utility+MII_HLev+YearAdj, data=Reg_Data,family = poisson)
summary(Mpois1)
rsq(Mpois1)

Mpois2 = glm(log(MII_HLev+1)~Trade+Defense.Effectiveness+Aggressive.Utility+After1991, data=Reg_Data,family = poisson)
summary(Mpois2)
rsq(Mpois2)

M2 = lm(MII_Nums~Trade+Defense.Effectiveness+Aggressive.Utility, data=Reg_Data)
summary(M2)
M3 = glm(log(MII_Nums+1)~Trade+Defense.Effectiveness+Aggressive.Utility, data=Reg_Data,family = poisson)
summary(M3)
pred = predict(Mpois,Reg_Data,se.fit=TRUE)
plot(x=Reg_Data$MII_Nums,y=Mpois$residuals)

pred2 = predict(M2,Reg_Data,se.fit=TRUE)
plot(x=Reg_Data$MII_Nums,y=M2$residuals)

pred3 = predict(M3,Reg_Data,se.fit=TRUE)
plot(x=Reg_Data$MII_Nums,y=M3$residuals)

#ZINB Modeling
Mzinb = zeroinfl(MII_HLev~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj | Country,
                 data = Reg_Data, dist = "negbin", EM = F)
summary(Mzinb)
stargazer(Mzinb,no.space=T)

#**********************************************************
#*#************************Diagnostics*********************
#*#********************************************************
#*#********************************************************
Mpois = glm(MII_HLev~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+US_Interest+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj+Country, data=Scl_Data,family = poisson)
summary(Mpois) 
rsq(Mpois,adj=T)
dispersiontest(Mpois,trafo=1) #Over dispersion test for choosing nb

Mzinb = zeroinfl(MII_HLev~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+US_Interest+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj | Country,
                 data = Scl_Data, dist = "negbin", EM = F)
summary(Mzinb)
Mnb = glm.nb(MII_HLev~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+US_Interest+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj+Country, data=Scl_Data)
summary(Mnb)
vuong(Mzinb, Mnb)#Vuong Test if Zero inflated is better

#**********************************************************
#*#**********Current Battery for Series I******************
#*#********************************************************
#*#********************************************************
Mzinb = zeroinfl(MII_HLev~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+I(Aggressive.Utility^3)+US_Interest+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj | Country,
                 data = Scl_Data, dist = "negbin", EM = F)
summary(Mzinb)
Mzinb1 = zeroinfl(MII_HLev~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+I(Aggressive.Utility^3)+US_Interest+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj | Country,
                 data = Scl_Data_1, dist = "negbin", EM = F)
Mzinb2 = zeroinfl(MII_HLev~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+I(Aggressive.Utility^3)+US_Interest+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj | Country,
                  data = Scl_Data_2, dist = "negbin", EM = F)
MSLR = glm(MII_HLev~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+I(Aggressive.Utility^3)+US_Interest+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj+Country, data=Scl_Data,family = gaussian)
summary(MSLR)
stargazer(Mzinb,Mzinb1,Mzinb2,MSLR,no.space=T)
#*#********************************************************
#*#********************************************************
#*#********************************************************
#Adding in Global sanctions database for sanction outcome of SQUO power

#Could include delta troop deployments 
GSDB = readxl::read_xls("GSDB_V2.xls")%>%dplyr::select(c("sanctioned_state",'sanctioning_state',"begin","end"))

#Splitting a case
GSDB = rbind(GSDB,c("Pakistan","United States","1965","1975"))
GSDB = rbind(GSDB,c("India","United States","1965","1975"))
GSDB = GSDB%>% #Cleaning data
  filter(sanctioning_state == "United States")%>%
  mutate(ccode = countrycode(sanctioned_state,"country.name","cown"))%>%
  na.omit()%>%filter(ccode %in% RisingCodes)

Sanctions = Reg_Data%>%select(c("Year","ccode"))%>%complete(Year, nesting(ccode))#Starting from cases in every country in every year
Sanctions$Sanction = 0
Sanctions$SanctionSum = 0
Sanctions$SanctionStart = 0
Sanctions$SanctionEnd = 0
for(i in years){
  for(j in 1:length(RisingCodes)){
    for(k in 1:nrow(GSDB)){
      if((i %in% GSDB$begin[k]:GSDB$end[k])&(GSDB$ccode[k] == Sanctions[(i-Start_Year)*length(RisingCodes)+j,2])){
        Sanctions[(i-Start_Year)*length(RisingCodes)+j,4] = Sanctions[(i-Start_Year)*length(RisingCodes)+j,4] + 1#Creates a sum of sanctions on the country
        Sanctions[(i-Start_Year)*length(RisingCodes)+j,3] = 1#Creates a binary value for sanctions on the country
      }
      if((i == GSDB$begin[k])&(GSDB$ccode[k] == Sanctions[(i-Start_Year)*length(RisingCodes)+j,2])){
        Sanctions[(i-Start_Year)*length(RisingCodes)+j,5] = 1#Creates a binary value for start of sanctions on the country
      }
      if((i == GSDB$end[k])&(GSDB$ccode[k] == Sanctions[(i-Start_Year)*length(RisingCodes)+j,2])){
        Sanctions[(i-Start_Year)*length(RisingCodes)+j,6] = 1#Creates a binary value for end of sanctions on the country
      }
    }
  }
}

Sanc_Reg = merge(Scl_Data,Sanctions, by = c("Year","ccode")) #adding other regression variables and selecting only valid cases
Sanc_Reg = Sanc_Reg%>%mutate(Delta.Troops = Delta.Troops/1000) #dividing troops by 1000
#Scl_Sanc_Reg = Sanc_Reg%>%mutate(Aggressive.Utility = scale(Aggressive.Utility))%>%mutate(Trade = scale(Trade))%>%mutate(Rising_Bombs = scale(Rising_Bombs))%>%
#  mutate(US_Bombs = scale(US_Bombs))%>%mutate(Hawkish = scale(Hawkish))%>%mutate(Mentions = scale(Mentions))%>%mutate(Containment = scale(Containment)) #Scaling all but Years, Democracy, Boarders
Sanc_Reg_1 = Sanc_Reg%>%filter(UScinc >= (1/3)) #Data of dyads with rising CINC at least 1/3rd of US CINC
Sanc_Reg_2 = Sanc_Reg%>%filter(UScinc < (1/3)) #Data of dyads with rising CINC lower than 1/3rd of US CINC
Sanc_Reg_3 = na.omit(Sanc_Reg)
#**********************************************************
#*#**********Current Battery for Series II******************
#*#********************************************************
#*#********************************************************
Mzinb = zeroinfl(SanctionSum~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj | Country,
                 data = Sanc_Reg, dist = "negbin", EM = F)
Mzinb1 = zeroinfl(SanctionSum~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj | Country,
                  data = Sanc_Reg_1, dist = "negbin", EM = F)
Mzinb2 = zeroinfl(SanctionSum~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj | Country,
                 data = Sanc_Reg_2, dist = "negbin", EM = F)
summary(Mzinb2)
SLR = glm(Delta.Troops~Trade+I(Trade^2)+Aggressive.Utility+I(Aggressive.Utility^2)+RiskofWar+I(RiskofWar^2)+Containment+I(Containment^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+YearAdj+Country, data=Sanc_Reg_3, family = gaussian)
stargazer(Mzinb,Mzinb1,Mzinb2,SLR,no.space=T)

#**********************************************************
#*#**************Visuals for Series II*********************
#*#********************************************************
#Graphing only select countries vs average in SanctionSum
Average_Data = Sanc_Reg%>%group_by(Year)%>%summarise(mean(SanctionSum))
colnames(Average_Data) = c("Year","SanctionSum")
Graph_Data = Sanc_Reg%>%filter(ccode %in% c(365,710,740,290))
x = ggplot(Graph_Data) + theme_classic() + geom_line(aes(x=Graph_Data$Year,y=Graph_Data$SanctionSum,color=Country), size = 1) + labs(x="Year", y="Sum of Sanctions") 
x + geom_line(data = Average_Data, aes(x=Average_Data$Year,y=Average_Data$SanctionSum, color="Average"), size = 1.5)

#Graphing only select countries vs average in DeltaTroops
Average_Data = Sanc_Reg%>%group_by(Year)%>%summarise(mean(Delta.Troops))
colnames(Average_Data) = c("Year","Delta.Troops")
Graph_Data = Sanc_Reg%>%filter(ccode %in% c(365,710,740,750))
x = ggplot(Graph_Data) + theme_classic() + geom_line(aes(x=Graph_Data$Year,y=Graph_Data$Delta.Troops,color=Country), size = 1) + labs(x="Year", y="Change in Troops") 
x + geom_line(data = Average_Data, aes(x=Average_Data$Year,y=Average_Data$Delta.Troops, color="Average"), size = 1.5)


#lets try to spline
require("splines")
m1 = lm(MII_Nums~bs(YearAdj,knots=c(4,8,12)), data=Reg_Data)
summary(m1)

m2 = lm(MII_Nums~Trade+Defense.Effectiveness+Aggressive.Utility+bs(YearAdj,knots=c(4,8,12)), data=Reg_Data)
summary(m2)
m3 = glm(MII_Nums~Trade+Defense.Effectiveness+Aggressive.Utility+bs(YearAdj,knots=c(4,8,12)), data=Reg_Data,family = poisson)
summary(m3)
m4 = glm(MII_Nums~Trade+Defense.Effectiveness+Aggressive.Utility+bs(YearAdj,knots=c(4), degree = 2), data=Reg_Data,family = poisson)
summary(m4)
plot(Reg_Data$YearAdj)

hist(Reg_Data$MII_HLev,breaks = 150)


#**********************************************************
#*#**********Current Battery for Series II*****************
#*#********************************************************
Mzinb = zeroinfl(SanctionSum~Trade+I(Trade^2)+Containment+I(Containment^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj | Country,
                 data = Scl_Sanc_Reg, dist = "negbin", EM = F)
summary(Mzinb)
Mnb = glm.nb(SanctionSum~Trade+I(Trade^2)+Containment+I(Containment^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj, data=Scl_Sanc_Reg)
summary(Mnb)
vuong(Mzinb, Mpois)#Vuong Test if Zero inflated is better
MSLR = glm(SanctionSum~Trade+I(Trade^2)+Containment+I(Containment^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj, data=Scl_Sanc_Reg,family = gaussian)
summary(MSLR)
Mzinb2 = zeroinfl(Delta.Troops~Trade+I(Trade^2)+Containment+I(Containment^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj | Country,
                 data = na.omit(Scl_Sanc_Reg), dist = "negbin", EM = F)
MSLR2 = glm(Delta.Troops~Trade+I(Trade^2)+Containment+I(Containment^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+Mentions+After1991+Democracy+N_Borders+Nuclear+Rising_Bombs+US_Bombs+YearAdj, data=Scl_Sanc_Reg,family = gaussian)
summary(MSLR)
stargazer(Mzinb,MSLR,MSLR2,no.space=T)








#Models
#arms, troop deployments, foriegn military aid
#Could do logit on Sanction IV
Mpois = glm(SanctionSum~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+Hawkish+I(Hawkish^2)+After1991+Democracy+Nuclear+YearAdj+Country, data=Sanc_Reg,family = poisson)
summary(Mpois)
rsq(Mpois)
  
Mpois = glm(SanctionStart~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+After1991+Country+Democracy, data=Sanc_Reg,family = poisson)
summary(Mpois)
rsq(Mpois)
  
Mpois = glm(Sanction~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+After1991+Country+Democracy, data=Sanc_Reg,family = poisson)
summary(Mpois)
rsq(Mpois)  


gs = seq(from = 0, to = 0.1, by = 0.0001)
m = lm.ridge(MII_HLev~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+After1991+Democracy+Country+Hawkish, Sanc_Reg, lambda = gs)

# Choose the ridge penalty
library(glmnet)
varmtx <- model.matrix(MII_HLev~Trade+I(Trade^2)+Defense.Effectiveness+I(Defense.Effectiveness^2)+Aggressive.Utility+I(Aggressive.Utility^2)+After1991+Democracy+Country+Hawkish-1, data=Reg_Data)
response <- Reg_Data$MII_HLev
varmtx <- model.matrix(attacks~.-1, data=dat)

# alpha=0 means ridge regression. 
ridge <- glmnet(scale(varmtx), response, alpha=0, family = poisson)

# Cross validation to find the optimal lambda penalization
cv.ridge <- cv.glmnet(varmtx, response, alpha=0)

lbs_fun <- function(fit, offset_x=1, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])+ offset_x
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
}


plot(ridge, xvar = "lambda", label=T)
lbs_fun(ridge)
abline(v=cv.ridge$lambda.min, col = "red", lty=2)
abline(v=cv.ridge$lambda.1se, col="blue", lty=2)


#Using Lasso
# alpha=1 means lasso regression. 
lasso <- glmnet(scale(varmtx), response, alpha=1)

# Cross validation to find the optimal lambda penalization
cv.lasso <- cv.glmnet(varmtx, response, alpha=1)


plot(lasso, xvar = "lambda", label=T)
lbs_fun(lasso)
abline(v=cv.lasso$lambda.min, col = "red", lty=2)
abline(v=cv.lasso$lambda.1se, col="blue", lty=2)













rm(list =c(ls()[c(5,10)])) #cleaning out Data

25*.36

dbinom(9,25,.5)














normal_exponential_model = "
data {
int <lower=1> n;
int <lower=0> YearAdj[n];
int <lower=0> After1991[n];
real <lower=0> Hawkish[n];
int <lower=1> Democracy[n];
real <lower=0> Trade[n];
real <lower=0> Defense.Effectiveness[n];
real <lower=0> Aggressive.Utility[n];

real <lower=0> MII_HLev[n];
}

parameters {
real<lower=0> q[n_counties];
real Beta_YearAdj;
real Beta_After1991;
real Beta_Hawkish;
real Beta_Democracy;
real Beta_Trade;
real Beta_Defense.Effectiveness;
real Beta_Aggressive.Utility;


real<lower=0> sigma[n_counties];
real<lower=0> theta[n_counties];
real<lower=0> mu;
real<lower=0> tau;
real<lower=0> nu;
real<lower=0> eta;
}

model {
// Prior
Beta_YearAdj ~ normal(-.02,10);
Beta_After1991 ~ normal(-.5,5);
Beta_Hawkish ~ normal(.5,5);
Beta_Democracy ~ normal(-.02,5);
Beta_Trade ~ normal(-.5,5);
Beta_Defense.Effectiveness ~ normal(-.5,5);
Beta_Aggressive.Utility ~ normal(.5,5);

mu ~ normal(0,100);
nu ~ exponential(.1);
eta ~ exponential(.1);

// Hierarchial model
theta ~ normal(mu,tau);
sigma ~ gamma(nu/2,nu*eta/2);

// Data model
for (i in 1:n) radon[i] ~ neg_binomial_2(theta[county[i]], sigma[county[i]]);
for (i in 1:n) radon[i] ~ neg_binomial_2(mu[county[i]], phi[county[i]]);
}
"

m = stan_model(model_code = normal_exponential_model)
radon.dat = list(n = nrow(dat),
                 n_counties = as.integer(length(unique(dat$county))),
                 radon = as.integer(dat$radon),
                 county = as.integer(factor(dat$county, levels=unique(as.character(dat$county)))))

radonpost = sampling(m, radon.dat)
radonpost



library("rstan")
regression_model = "
data {
  
  int <lower=1> N;
  vector[N] YearAdj;
  vector[N] After1991;
  vector[N] Hawkish;
  vector[N] Democracy;
  vector[N] Trade;
  vector[N] DefenseEffectiveness;
  vector[N] AggressiveUtility;

  vector[N] MII_HLev;
}
parameters {
  real alpha;
  real Beta_YearAdj;
  real Beta_After1991;
  real Beta_Hawkish;
  real Beta_Democracy;
  real Beta_Trade;
  real Beta_DefenseEffectiveness;
  real Beta_AggressiveUtility;
  
  real Beta_Trade_2;
  real Beta_DefenseEffectiveness_2;
  real Beta_AggressiveUtility_2;
  real<lower = 0> sigma;
  
}
model {
  // priors including all constants
                          
  target += normal_lpdf(alpha | 4, 10);
  target += normal_lpdf(Beta_YearAdj | 0, 2);
  target += normal_lpdf(Beta_After1991 | 0, 2);
  target += normal_lpdf(Beta_Hawkish | 0, 2);
  target += normal_lpdf(Beta_Democracy | 0, 2);
  target += normal_lpdf(Beta_Trade | 0, 2);
  target += normal_lpdf(Beta_DefenseEffectiveness | 0, 2);
  target += normal_lpdf(Beta_AggressiveUtility | 0, 2);
  target += normal_lpdf(Beta_Trade_2 | 0, 2);
  target += normal_lpdf(Beta_DefenseEffectiveness_2 | 0, 2);
  target += normal_lpdf(Beta_AggressiveUtility_2 | 0, 2);
  target += normal_lpdf(sigma | 0, 10)
    - normal_lccdf(0 | 0, 10);
  target += normal_lpdf(MII_HLev | alpha + YearAdj*Beta_YearAdj + After1991*Beta_After1991 + Hawkish*Beta_Hawkish +
                          Democracy*Beta_Democracy + Trade*Beta_Trade + DefenseEffectiveness*Beta_DefenseEffectiveness +
                          AggressiveUtility*Beta_AggressiveUtility + Trade .* Trade*Beta_Trade_2 +
                          DefenseEffectiveness .* DefenseEffectiveness*Beta_DefenseEffectiveness_2 +
                          AggressiveUtility .* AggressiveUtility*Beta_AggressiveUtility_2, sigma);
}
"

m = stan_model(model_code = regression_model)
model.dat = list(N = nrow(Reg_Data),
                 YearAdj = Reg_Data$YearAdj,
                 After1991 = Reg_Data$After1991,
                 Hawkish = Reg_Data$Hawkish,
                 Democracy = Reg_Data$Democracy,
                 Trade = Reg_Data$Trade,
                 DefenseEffectiveness = Reg_Data$Defense.Effectiveness,
                 AggressiveUtility = Reg_Data$Aggressive.Utility,
                 MII_HLev = Reg_Data$MII_HLev,
                 country = as.integer(factor(Reg_Data$Country, levels=unique(as.character(Reg_Data$Country)))))

regmodel.post = sampling(m, model.dat)
regmodel.post
