#using output from 3.SummarizeSurvey.r
require(ggplot2)
require(bio.lobster)
require(bio.utilities)
require(devtools)

load_all('~/git/bio.utilities')

a = lobster.db('process.logs.unfiltered')
b = lobster.db('community_code')
d = lobster.db('vessels.by.port')
d = na.zero(d)
d1 = aggregate(cbind(GROSS_TONNAGE, BHP, LOA, BREADTH, DEPTH,YEAR_BUILT)~VR_NUMBER+LFA+YR_FISHED,data=d,FUN=min)
d = na.zero(d1,rev=T)
w = lobster.db('port')
v = lobster.db('port_location')

#Demographics on Lic
o = readRDS('data/LicenceCharacterisitics.rds')

#vessel characterisitcs
v = readRDS('data/VesselCharacterisitics.rds')


##age distribution
ages = readRDS('data/ageOfRespondants.rds')
o$ageBin = floor(o$Age/10)*10
o$ageBin = bio.utilities::recode(o$ageBin, "'20'='20-29'; '30'='30-39'; '40'='40-49';'50'='50-59';'60'='60-69'; '70'='70-79';'80'='80-89'; ")
o$LFAs = paste('LFA',o$LFA,sep=" ")
f=subset(o,LFA==38)
s = ages[['LFA 38']]

plotAges(f,s)
