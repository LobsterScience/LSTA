require(readxl)
require(devtools)
load_all('C:/C:/Users/cooka/OneDrive - DFO-MPO/git/LSTA')
#read in file
x = importFile(pa='C:/Users/cooka/OneDrive - DFO-MPO/Fishing_Behaviour/Changes in Fishing Practices Over Time.xlsx '
                 )
qu = x[[1]]
an = x[[2]]

for(i in 1:nrow(an)){
  an1 = an[i,]
  fn=paste(paste(as.character(an1[1,1]),'xlsx',sep='.'))
  write_xlsx(an1,path=fn)
}
