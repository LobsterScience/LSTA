require(readxl)
require(devtools)
load_all('C:/Users/cooka/Documents/git/LSTA')

#read in responses
re = responsesReadIn(onefile=T)
su = surveyReadIn()
cq = columns_per_question() #what columns from spread sheet do we need for this question


