require(readxl)
require(devtools)
load_all('C:/Users/cooka/Documents/git/LSTA')

#read in responses
re = responsesReadIn(su='C:\\Users\\cooka\\OneDrive - DFO-MPO\\Projects\\Shared Documents - Blue Economy SSH Research\\2023 Fishing Practices Survey',onefile=T)
#su = surveyReadIn(su ='C:\\Users\\cooka\\OneDrive - DFO-MPO\\Projects\\Shared Documents - Blue Economy SSH Research\\2023 Fishing Practices Survey/Survey.xlsx' )
data(survey)
cq = columns_per_question() #what columns from spread sheet do we need for this question
