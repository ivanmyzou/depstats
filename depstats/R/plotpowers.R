#' Power Plots
#'
#' @param Powers powers data matrix either from computepowers() or attached data in the package.
#' @param title title of the plot, defaults to 'Power Curves'.
#' @param special competitors with highlighted power curves.
#' @param cols column names.
#' @param nsample a vector of sample sizes, defaults to c(30,50,100,200,300,400).
#' @examples
#' plotpowers(addPowers[13+seq(0,79,14),],'Additional Test 7 A')

defaultcol = c('allnet','allCNN','scoreonly','imageonly',
               'ACE','AUK','Blom','dcor','EDC','Hell','Hoeff','HSIC',
               'Info','Ken','Martdiff','MIC','Rand','Spear',
               'ddrV','ddrTS2','hhgPsum','hhgGsum','hhgPmax','hhgGmax')
defaultn = c(30,50,100,200,300,400)

plotpowers <- function(Powers,title='Power Curves',special=c(1,2,3,4),nsample=defaultn,cols=defaultcol,showpowerdrop=TRUE){
  Nsamples <- c(0,defaultn) #just for plotting horizontal line of 1 (max possible power)
  plot(Nsamples,rep(1,length(Nsamples)),'l',lty = 3,lwd = 3,
       ylim=c(-0.1,1.1),
       main=title, xlab='sample size', ylab='power',
       las = 1
       ) #horizontal line power == 1
  #power curve plots by cols of Powers
  powerdrops <- c()
  for (i in (1:ncol(Powers))[-special]){
    if (all(Powers[,i] == sort(Powers[,i]))){
      points(nsample,Powers[,i], #all rows of ith column
             'l',lty = 2,col='grey30',lwd=2.5)
    }else{
      points(nsample,Powers[,i], #all rows of ith column
             'l',lty = 3,col='chocolate4',lwd=2.5)
      powerdrops <- c(powerdrops,cols[i])
    }
  }
  if (length(powerdrops) > 0 & showpowerdrop) {
    mtext(paste('Power Drop: ', paste(powerdrops, collapse = ", ")),
          side=3, cex=0.75
          ) #power drops listed
  }
  colors = c('Plum','Coral','Royalblue','Darkgreen') #special colors
  for (i in (1:length(special))){
    j = special[i] #jth column is special
    points(nsample,Powers[,j], #all rows of jth column
           'l',col=colors[i%%4+1],lwd=3.5)
    cat(defaultcol[j],' ',colors[i%%4+1],'\n')
  }
}
