# UtahMunicipalIndustiralUse_InitAnalysis.r
#
# Initial analysis of Utah's Municipal and Industrial Water Use Data
# 2018-2015
#
# Downloaded from https://dwre-utahdnr.opendata.arcgis.com/pages/municipal-and-industrial-data
#
# David E. Rosenberg
# November 28, 2020
# Updated November 12, 2021
# Utah State University
# david.rosenberg@usu.edu

rm(list = ls())  #Clear history

# Load required libraies

if (!require(tidyverse)) { 
  install.packages("tidyverse", repos="http://cran.r-project.org") 
  library(tidyverse) 
}

if (!require(readxl)) { 
  install.packages("readxl", repos="http://cran.r-project.org") 
  library(readxl) 
}

  
if (!require(RColorBrewer)) { 
  install.packages("RColorBrewer",repos="http://cran.r-project.org") 
  library(RColorBrewer) # 
}

if (!require(dplyr)) { 
  install.packages("dplyr",repos="http://cran.r-project.org") 
  library(dplyr) # 
}

if (!require(expss)) { 
  install.packages("expss",repos="http://cran.r-project.org") 
  library(expss) # 
}

if (!require(reshape)) { 
  install.packages("reshape", repos="http://cran.r-project.org") 
  library(reshape) 
}

if (!require(reshape2)) { 
  install.packages("reshape2", repos="http://cran.r-project.org") 
  library(reshape2) 
}


if (!require(pracma)) { 
  install.packages("pracma", repos="http://cran.r-project.org") 
  library(pracma) 
}

if (!require(lubridate)) { 
  install.packages("lubridate", repos="http://cran.r-project.org") 
  library(lubridate) 
}

if (!require(directlabels)) { 
  install.packages("directlabels", repo="http://cran.r-project.org")
  library(directlabels) 
}


#if (!require(plyr)) { 
#  install.packages("plyr", repo="http://cran.r-project.org")
#  library(plyr) 
#}

if (!require(ggrepel)) { 
  devtools::install_github("slowkow/ggrepel")
  library(ggrepel) 
}

library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)


### 0. Definitions

sFileBase <- "_Municipal_and_Industrial_Water_Use_Databases.csv"

#Define the River locations to use
dfFileYears <- data.frame(year = seq(2015,2018, by=1))
dfFileYears$filename <- paste0(dfFileYears$year,sFileBase,"")

### 1. Read IN the data files. Rbind so long list

for(i in 1:nrow(dfFileYears)){
 # i <- 1
  
  print(dfFileYears$year[i])
  
  # Read in the historical Powell data
  dfTempFile <- read.csv(file=dfFileYears$filename[i], 
                         header=TRUE, 
                         
                         stringsAsFactors=FALSE,
                         sep=",")
  
  dfTempFile$year <- dfFileYears$year[i]                     #factor(dfLocations$rivermile[i], levels = dfLocations$rivermile)
  #Rename the first column because they differ amoung files
  cColNames <- colnames(dfTempFile)
  cColNames[1] <- "FID"
  colnames(dfTempFile) <- cColNames
  
  if(dfTempFile$year[i] %in% c(2018)) { #We need to adjust some other columns
    cColNamesTemp <- cColNames
    cColNamesTemp[c(12, 25,26,27,28)] <- c("TotPotaGPCD", "TotalSup", "WellsSup","SpringsSup","SurfaceSup")
    
    colnames(dfTempFile) <- cColNamesTemp
    
    #Add blank columns for these fields
    cColsAdd <- c("ComSecGPCD", "IndSecGPCD", "InsSecGPCD", "InsSecoUse", "ResSecGPCD", "ResSecoUse", "ComSecoUse", "IndSecoUse", "IndSecoUse")
    for (i in (1:length(cColsAdd))) {
      dfTempFile[[cColsAdd[i]]] = 0
      }
    
  }
  
  if(dfTempFile$year[i] %in% c(2019)) { #We need to adjust some other columns
    cColNamesTemp <- cColNames
    cColNamesTemp[c(12, 25,26,27,28)] <- c("TotPotaGPCD", "TotalSup", "WellsSup","SpringsSup","SurfaceSup")
    
    colnames(dfTempFile) <- cColNamesTemp
    
    #Add blank columns for these fields
    cColsAdd <- c("ComSecGPCD", "IndSecGPCD", "InsSecGPCD", "InsSecoUse", "ResSecGPCD", "ResSecoUse", "ComSecoUse", "IndSecoUse", "IndSecoUse")
    for (i in (1:length(cColsAdd))) {
      dfTempFile[[cColsAdd[i]]] = 0
    }
    
  }

  if(i==1){ #first year, creat new data frame
    dfAllYears <- dfTempFile
  } else { #subsequent year, bind to existing records
  
    #bind the latest record to the existing records
    dfAllYears <- rbind(dfAllYears,dfTempFile)
    }

}



#Calculate population from per capita daily use and annual volume
dGalPerAcreFeet <- 325851
dfAllYears$Population <-  dfAllYears$TotalUse * dGalPerAcreFeet / (dfAllYears$TotalGPCD * 365)

#Calculate average percapita use statewide

dfAvgGPCD <- dfAllYears %>% group_by(year) %>% filter(is.na(Population) == FALSE, is.infinite(Population) == FALSE) %>% summarize(TotalUse = sum(TotalUse), 
                                                         Population=sum(Population),
                                                         AvgGPCD = sum(TotalUse)*dGalPerAcreFeet/(365*sum(Population)),
                                                         UsePerPersonPerYear = sum(TotalUse)/sum(Population))


#Grab color palettes
palBlues <- brewer.pal(9, "Blues")
palReds <- brewer.pal(9, "Reds")
palBlueFunc <- colorRampPalette(c(palBlues[3],palBlues[9]))

###### 2. Plot Total Use vs. Population. Sloped line for avg gpcd

#Plot hourly - too much data
library(scales)

ggplot(dfAllYears %>% filter(year == 2018), aes(x = Population/100000, y= TotalUse/10000, color=BASIN)) +
  geom_point(size=2) +
  
  #Sloped line for statewide average per capacita per day
  geom_abline(aes(intercept = 0, slope = as.numeric(10*dfAvgGPCD %>% filter(year == 2018) %>% select(UsePerPersonPerYear))), color="black", size = 2, show.legend = NA) +

  #scale_colour_manual(name="Lines", labels=c(paste(round(dfAvgGPCD %>% filter(year==2015) %>% select(AvgGPCD)), "gpcd")), values=c("Black")) +
  facet_wrap(~BASIN) + 
  #scale_color_manual(values = palBlues[3:8], breaks = c(194,149,76,61,30,0)) + #c(0,30,61,76,149,194)) +
  #scale_x_continuous(labels = numeric() ) +
  labs(x="Population (100,000)", y="Total Use (10,000 acre-feet per year)", color = "County") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank())

ggsave("UseVsPopulation.png", width=9, height = 6.5, units="in")

#Histogram of per capita daily use by provider

ggplot(dfAllYears, aes(x=TotalGPCD, color = BASIN, fill = BASIN)) +
  geom_histogram(binwidth = 50) +
  #Plot vertial line for average gpcd
  geom_vline(xintercept = dfAvgGPCD$AvgGPCD, color="black", size = 2) +
  
  geom_text(data=dfAvgGPCD %>% filter(year==2015), aes(x=AvgGPCD+30,y=25), label = "Utah average", size=6, hjust=0) +
  
  scale_x_continuous(limits = c(0,3000)) +
  facet_wrap(~year) +
  
  labs(x="Total Gallons per Person per Day", y="Number of Providers") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank())


#Histogram of daily per caita water use by water volume
#Create the bins
cGPCDBins <- seq(0,max(dfAllYears$TotalGPCD), by=50)
#Assign each GPCD value to a bin
dfAllYears$TotGPCDBin <- cut(dfAllYears$TotalGPCD, breaks = cGPCDBins, labels = cGPCDBins[1:(length(cGPCDBins)-1)])
#Sum in each group
dfGCPDByVolume <- dfAllYears %>% group_by(TotGPCDBin, year) %>% summarize(TotalUse = sum(TotalUse))

ggplot(dfGCPDByVolume) +
  geom_bar(aes(x= cGPCDBins[as.numeric(TotGPCDBin)], y=TotalUse/10000), fill="blue", color="blue", stat="identity") +
  #Plot vertial line for average gpcd
  geom_vline(xintercept = dfAvgGPCD$AvgGPCD, color="black", size = 2) +
  
  geom_text(data=dfAvgGPCD %>% filter(year==2015), aes(x=AvgGPCD+30,y=25), label = "Utah average", size=6, hjust=0) +
  
  scale_x_continuous(limits=c(0,3000)) +
  facet_wrap(~year) +
  
  labs(x="Total Gallons per Person per Day", y="Total Use\n(10,000 acre-feet per year)") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank())

#Cumulative sum
ggplot(dfGCPDByVolume) +
  geom_line(aes(x= cGPCDBins[as.numeric(TotGPCDBin)], y=cumsum(TotalUse)/10000, color=as.factor(year)), size =1.5) +
  #Plot vertial line for average gpcd
  geom_vline(xintercept = dfAvgGPCD$AvgGPCD, color="black", size = 2) +
  #Label the vertical line
  geom_text(data=dfAvgGPCD %>% filter(year==2015), aes(x=AvgGPCD+20,y=50), label = "Utah average", size=8, hjust=0) +
  
  scale_x_continuous(limits=c(0,3000)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / (max(cumsum(dfAvgGPCD$TotalUse))/100)))+ #, name="Cumulative Use\n(Percent of Total)") +
  #facet_wrap(~year) +
  
  labs(x="Total Gallons per Person per Day", y="Total Use\n(10,000 acre-feet per year)") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank())


#Bar graph of providers ranked by Total GPCD
#Colors to split into zones. The zones are Salt Lake City's use of 191 gpcd, 2x that, and above
#Highlight listed cities as black bars

#order the data frame by the TotalGPCD field
dfAllYearsSort <- dfAllYears %>% filter(year == 2018) %>% arrange(TotalGPCD)
dfAllYearsSort$Row <- seq(1,nrow(dfAllYearsSort), by=1)

#Cities to highlight
#These cities have a range of Total GPCD, Total GPCD is often similar to Potable GPCD, and people will recognize these providers
#Read in from Excel
dfHighlightCities <- read_excel("HighlightCities.xlsx","HighlightCities","A1:B23")

# The Salt Lake City gallons per person per day reported
nAvgGPCD <- 191 #240 = state average
sAvgText <- "Salt Lake City"

# Define the state conservation goal and calculate the equivalent GPCD that
# will reach the nAvgGPCD vaue
nStateConserveGoal <- 0.2
#Calculate the GPCD that will meet the SLC target
nGPCDToMeetTarget <-  -nAvgGPCD/(nStateConserveGoal - 1)
print(paste(sprintf("%0.f",nGPCDToMeetTarget), "gpcd can reduce by state conservation goal of", sprintf("%.0f%%", nStateConserveGoal*100), "and achieve SLC use"))

# Break at 0, the average, the GPCD to reduce to meet the average, and max
cGPCDBreaks <-  c(0, nAvgGPCD, nGPCDToMeetTarget, max(dfAllYearsSort$TotalGPCD))
#Count water providers within breaks
dfCounts <- hist(dfAllYearsSort$TotalGPCD, breaks = cGPCDBreaks, plot=FALSE) #, breaks = c(0,100,200,300,400,500, 1000, 5000))
dfCounts$CumSum <- nrow(dfAllYearsSort) - cumsum(dfCounts$counts)

#Assign each water provider to a break
dfAllYearsSort$TotGPCDBin <- cut(dfAllYearsSort$TotalGPCD, breaks = cGPCDBreaks, labels = cGPCDBreaks[1:(length(cGPCDBreaks)-1)])
#Sum in each group
dfGPCDByVolume <- dfAllYearsSort %>% group_by(TotGPCDBin, year) %>% summarize(TotalUse = sum(TotalUse))

#Add an additional group that is the Water providers to highlight
dfAllYearsSort$TotGPCDBin <- ifelse(dfAllYearsSort$WRENAME %in% dfHighlightCities$FullName,"Highlight", dfAllYearsSort$TotGPCDBin)

#Joint the Short cities names
dfAllYearsSort <- left_join(dfAllYearsSort, dfHighlightCities, by = c("WRENAME" = "FullName"))
#Convert NAs to ""
dfAllYearsSort$ShortName <- ifelse(is.na(dfAllYearsSort$ShortName),"",dfAllYearsSort$ShortName)

#Add the volume data to the prior counts data
dfCounts$Volume <- dfGCPDByVolume$TotalUse
dfCounts$PercentVolume <- dfCounts$Volume/sum(dfCounts$Volume)
dfCounts$TextFull <- paste("These ",dfCounts$counts, "providers use\n", sprintf("%1.0f%%", dfCounts$PercentVolume*100), "of the statewide total")

dfCounts$Text <- paste(dfCounts$counts, "\nProviders")
dfCounts$TextFull <- paste(dfCounts$counts, "\nProviders\n", "(",sprintf("%1.0f%%", dfCounts$PercentVolume*100), "state volume)")


#Nullify the last text entry
dfCounts$TextFull <- dfCounts$TextFull[1:(length(dfCounts$TextFull)-1)]

dfCounts$Text <- paste(dfCounts$counts, "\nProviders")


#Build a data frame to plot labels for the groups
dfLabels <- data.frame(Rank = dfCounts$CumSum, 
                       Text = dfCounts$Text, 
                       TextFull = dfCounts$TextFull,
                       Counts = dfCounts$counts,
                       FontColor = as.factor(c(1,2,3)))
#Calculate the Mid rank as Cumulative + 1/2 current counts
dfLabels$MidRank <- dfLabels$Rank + dfLabels$Counts/2
dfLabels$YPos <- 1250

dfAllYearsSort$Blank <- ""

#Build a data frame to label the lines
dfLineLabels <- data.frame(Rank = dfLabels$Rank[1:2],
                           GPCD = cGPCDBreaks[2:3],
                           #Label = c("Salt Lake City ~ 191 gpcd", "2x Salt Lake City ~ 382 gpcd"))
                          Label = c(paste(sAvgText, "~ ", nAvgGPCD, "gpcd"), paste("120%", sAvgText, "~", sprintf("%.0f", nGPCDToMeetTarget), "gpcd")))

sBlockLabelColor <- "red"
sBlockSepColor <- "black"
sHighlightColor <- "red"

#Flip the Short Names upside down to plot labels correctly
dfAllYearsSortFlip <- dfAllYearsSort %>% map_df(rev)

ggplot(dfAllYearsSort ) +
  #Bar graph of Total gallons per person per day reversed ranked (largest at left)
  geom_bar(aes(x= reorder(WRENAME, -TotalGPCD), y=TotalGPCD, fill = TotGPCDBin, color = TotGPCDBin), stat="identity") +
  #Plot vertical line to separate blocks
  geom_vline(xintercept = dfCounts$CumSum, color=sBlockSepColor, size = 1, linetype = "dashed") +
  
  #Label the number of providers in each block
  geom_text(data = dfLabels, aes(x = MidRank, y = YPos, label = Text, color = FontColor), size = 7) +
  #Label the line breaks
  geom_text(data = dfLineLabels, aes(x = Rank - 5, y = 2000, label = Label), size = 6, angle = 90, color = sBlockSepColor) +
   
  #Plot some points to see where they are
  #geom_point(data = data.frame(x = c(0,1000), y = c(0,1000)), aes(x=x,y=y), size= 10) +
  
  #geom_text(data=dfAvgGPCD %>% filter(year==2015), aes(x=AvgGPCD+30,y=25), label = "Utah average", size=6, hjust=0) +
  
  #Define scales
  #Flip the x labels upside down
  scale_x_discrete(labels = dfAllYearsSortFlip$ShortName) + 
  scale_color_manual(values = c(palBlues[4], palBlues[6], palBlues[8], sHighlightColor)) +
  scale_fill_manual(values = c(palBlues[4], palBlues[6], palBlues[8], sHighlightColor)) +
  scale_y_continuous(breaks = seq(0,4500, by=500)) +
  
  #Turn the Fill guide off
  guides(fill = "none", color = "none") + 
  labs(x="Utah water provider (Ranked)", y="Gallons per person per day") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        #Remove the minor x grid lines
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = sHighlightColor))


#Prepare a data frame to download to excel that shows:
#   1) Highlight city
#   2) Total GPCD
#   3) Potable GPCD
#   4) Percent reducation to get to Salt Lake City value

#Filter and select to get the data we want
dfReductions <- dfAllYearsSort %>%  filter(ShortName != "") %>% select(ShortName, TotalGPCD, TotPotaGPCD) %>% arrange(-TotalGPCD)
# Calculate % reduction
dfReductions$PercentReduce <- (dfReductions$TotalGPCD - nAvgGPCD)/dfReductions$TotalGPCD

dfReductions$TargetCanMeet <- ifelse(dfReductions$PercentReduce > nStateConserveGoal, "No", "Yes")
# State whether sufficient to meet conservation goal of 20%


#Write the sorted dataframe to csv
write.csv(dfReductions, "dfReductions.csv")


#Compare TotalGPCD to Potable GPCD
ggplot(dfAllYearsSort ) +
  #Bar graph of Total gallons per person per day reversed ranked (largest at left)
  geom_point(aes(x= TotalGPCD, y=TotPotaGPCD, color = TotGPCDBin), size = 6) +

  scale_color_manual(values = c(palBlues[4], palBlues[6], palBlues[8], sHighlightColor)) +
  
  guides(color = "none") + 
  labs(x="Total Use (gallons per person per day)", y="Potable Use (gallons per person per day)") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        #Remove the minor x grid lines
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.key = element_blank())
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  