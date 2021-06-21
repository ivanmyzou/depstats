#' LaTex Table Codes Printing
#'
#' @param PowersSummary powers summary table from powerssummary().
#' @param title title of the table.
#' @param rows row names.
#' @param cuts indices for table separation.
#' @param cols column names.
#' @param decimals number of decimals to be printed.
#' @examples
#' plotpowers(addPowers[11+seq(0,65,11),],'Additional Test 7')

defaultcol = c('allnet','allCNN','scoreonly','imageonly',
               'ACE','AUK','Blom','dcor','EDC','Hell','Hoeff','HSIC',
               'Info','Ken','Martdiff','MIC','Rand','Spear',
               'ddrV','ddrTS2','hhgPsum','hhgGsum','hhgPmax','hhgGmax',
               "avgPower","bestPower","avgGap","worstGap",
               "allnet gap","allCNN gap","scoreonly gap", "imageonly gap")
defaultrow = c('n = 30','n = 50','n = 100','n = 200','n = 300','n = 400')

latexpowers <- function(Powers,Group,sizes=c(30,50,100,200,300,400),nspecial = 4,
                        decimals=3){
  ngroup = length(Group)
  nsize = length(sizes)

  ##begins
  cat("\\begin{center}\n") # 1
  cat(paste("\\begin{tabular}{ m{1.5cm} m{0.75cm}  ",
            paste(rep(sprintf("c"), nspecial), sep = "", collapse = " "),
            paste(rep(sprintf("c"), ncol(Powers) - nspecial), sep = "", collapse = " "), "}\n",
            sep = "")) # 2
  cat("\\hline\n")

  ##columns of the table
  cat(" ")
  cat(' & n ')
  for (i in 1:(ncol(Powers))){
    cat(paste('&',colnames(Powers)[i]),'')
  }
  cat("\\\\\n")
  cat(paste0("\\cline","{", 3,"-", ncol(Powers)+2, "}","\n"))
  cat(paste0("\\cline","{", 3,"-", ncol(Powers)+2, "}","\n"))

  ##powers
  for (g in 1:ngroup){ # Group
    cat(sprintf('\n\\multirow{%s}{1.5cm}{',nsize), Group[g],'} ', sep='')
    for (i in 1:nsize){ #inner row
      r = i + (g-1) * nsize #actual row number
      m = max(Powers[r,]) #max value to be in bold text
      cat(paste('& ', sizes[i], ''))
      for (j in 1:ncol(Powers)){ #across columns
        v = Powers[r,j]
        if (v == m){
          cat(paste('& ', '\\textbf{', sprintf(v,fmt=paste0('%#.',decimals,'f')),'}', ''))
        } else{
          cat(paste('& ', sprintf(v,fmt=paste0('%#.',decimals,'f')), ''))
        }
      } #across columns
      cat("\\\\ \n")
    } #inner row
    cat("\\hline \n")
  } # Group

  AvePowers <- foreach(i=1:nsize, .combine='rbind') %do% {
    colMeans(Powers[seq(i,nrow(Powers),nsize),])
  }
  #AvePowers
  cat(sprintf('\n\\multirow{%s}{1.5cm}{',nsize), 'Average Power','} ', sep='')
  for (i in 1:nsize){ #inner row
    m = max(AvePowers[i,]) #min value to be in bold text
    cat(paste('& ', sizes[i], ''))
    for (j in 1:ncol(AvePowers)){ #across columns
      v = AvePowers[i,j]
      if (v == m){
        cat(paste('& ', '\\textbf{', sprintf(v,fmt=paste0('%#.',decimals,'f')),'}', ''))
      } else{
        cat(paste('& ', sprintf(v,fmt=paste0('%#.',decimals,'f')), ''))
      }
    } #across columns
    cat("\\\\ \n")
  } #inner row
  cat("\\hline \n")

  ##Gaps
  Gaps <- foreach(i=1:nrow(Powers), .combine='rbind') %do% {
    max(Powers[i,]) - Powers[i,] #distance from max power in each row
  }
  rownames(Gaps) <- rownames(Powers)

  AveGaps <- foreach(i=1:nsize, .combine='rbind') %do% {
    colMeans(Gaps[seq(i,nrow(Gaps),nsize),])
  }

  MaxGaps <- foreach(i=1:nsize, .combine='rbind') %do% {
    apply(Gaps[seq(i,nrow(Gaps),nsize),], 2, max)
  }

  cat("\\hline \n")
  #AverageGap
  cat(sprintf('\n\\multirow{%s}{1.5cm}{',nsize), 'Average Gap','} ', sep='')
  for (i in 1:nsize){ #inner row
    m = min(AveGaps[i,]) #min value to be in bold text
    cat(paste('& ', sizes[i], ''))
    for (j in 1:ncol(AveGaps)){ #across columns
      v = AveGaps[i,j]
      if (v == m){
        cat(paste('& ', '\\textbf{', sprintf(v,fmt=paste0('%#.',decimals,'f')),'}', ''))
      } else{
        cat(paste('& ', sprintf(v,fmt=paste0('%#.',decimals,'f')), ''))
      }
    } #across columns
    cat("\\\\ \n")
  } #inner row
  cat("\\hline \n")

  cat("\\hline \n")
  #MaxGaps
  cat(sprintf('\n\\multirow{%s}{1.5cm}{',nsize), 'Max Gap','} ', sep='')
  for (i in 1:nsize){ #inner row
    m = min(MaxGaps[i,]) #min value to be in bold text
    cat(paste('& ', sizes[i], ''))
    for (j in 1:ncol(MaxGaps)){ #across columns
      v = MaxGaps[i,j]
      if (v == m){
        cat(paste('& ', '\\textbf{', sprintf(v,fmt=paste0('%#.',decimals,'f')),'}', ''))
      } else{
        cat(paste('& ', sprintf(v,fmt=paste0('%#.',decimals,'f')), ''))
      }
    } #across columns
    cat("\\\\ \n")
  } #inner row
  cat("\\hline \n")

  ##ends
  cat("\\end{tabular}\n") #2
  cat("\\end{center}\n") #1
}
