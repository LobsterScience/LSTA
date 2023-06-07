#using output from 3.SummarizeSurvey.r
require(ggplot2)
require(bio.lobster)
require(bio.utilities)
require(devtools)

load_all('~/git/bio.utilities')
load_all('C:/Users/cooka/Documents/git/LSTA')

a = lobster.db('process.logs.unfiltered')
b = lobster.db('community_code')
d = lobster.db('vessels.by.port')
d = na.zero(d)
d1 = aggregate(cbind(GROSS_TONNAGE, BHP, LOA, BREADTH, DEPTH,YEAR_BUILT)~VR_NUMBER+LFA+YR_FISHED,data=d,FUN=min)
d = na.zero(d1,rev=T)
w = lobster.db('port')
v = lobster.db('port_location')

#Demographics on Lic
#o = lobster.db('licence_characteristics')
o = readRDS(file='data/FisheryLicences.rds')



##age distribution
        ages = readRDS('data/ageOfRespondants.rds')
        o$ageBin = floor(o$Age/10)*10
        o$ageBin = bio.utilities::recode(o$ageBin, "'20'='20-29'; '30'='30-39'; '40'='40-49';'50'='50-59';'60'='60-69'; '70'='70-79';'80'='80 +'; ")
        o$LFAs = paste('LFA',o$LFA,sep=" ")

        f=subset(o,LFA==28& !is.na(Age))
        s = ages[['LFA 28']]
        plotAges(s=s,fp=f)

        f=subset(o,LFA==29& !is.na(Age))
        s = ages[['LFA 29']]
        plotAges(s=s,fp=f)

        f=subset(o,LFA==30& !is.na(Age))
        s = ages[['LFA 30']]
        plotAges(s=s,fp=f)

        f=subset(o,LFA==36& !is.na(Age))
        s = ages[['LFA 36']]
        plotAges(s=s,fp=f)

        f=subset(o,LFA==38& !is.na(Age))
        s = ages[['LFA 38']]
        plotAges(s=s,fp=f)


##years as captain distribution
        yrc = readRDS('data/yearsAsCaptainRespondants.rds')

        o$yrH = dplyr::case_when(o$YearsHeld<5~'<5',
                                 o$YearsHeld>=5 & o$YearsHeld<15 ~ '5-14',
                                 o$YearsHeld>=15 & o$YearsHeld<30 ~ '15-29',
                                 o$YearsHeld>=30  ~ '30+'
        )
        o$LFAs = paste('LFA',o$LFA,sep=" ")

        f=subset(o,LFA==28)
        s = yrc[['LFA 28']]
        plotYearsAsCaptain(s=s,fp=f)

        f=subset(o,LFA==29& !is.na(Age))
        s = yrc[['LFA 29']]
        plotYearsAsCaptain(s=s,fp=f)

        f=subset(o,LFA==30& !is.na(Age))
        s = yrc[['LFA 30']]
        plotYearsAsCaptain(s=s,fp=f)

        f=subset(o,LFA==36& !is.na(Age))
        s = yrc[['LFA 36']]
        plotYearsAsCaptain(s=s,fp=f)

        f=subset(o,LFA==38& !is.na(Age))
        s = yrc[['LFA 38']]
        plotYearsAsCaptain(s=s,fp=f)




##vessel size distribution
        #vessel characterisitcs
        v = readRDS('data/VesselCharacterisitics.rds')
