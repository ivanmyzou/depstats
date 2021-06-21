

defaultcol = c('allnet','allCNN',
               'ACE','AUK','Blom','dcor','EDC','Hell','Hoeff','HSIC',
               'Info','Ken','Martdiff','MIC','Rand','Spear',
               'ddrV','ddrTS2','hhgPsum','hhgGsum','hhgPmax','hhgGmax',
               "avgPower","bestPower","avgGap","worstGap",
               "allnet gap","allCNN gap")

printpowers <- function(PowersSummary,title,rows,cols=defaultcol,decimals=3){
  cat(paste('\t',title,'\n'))
  Ptable <- t(PowersSummary)
  colnames(Ptable) <- rows
  print(round(Ptable,decimals))
}
