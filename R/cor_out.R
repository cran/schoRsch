# -----------------------------------------------
# Assembles summary line for correlation analyses
# Requires output object from cor.test as input
# argument.
# -----------------------------------------------
cor_out <- function(coroutput, stats = FALSE, print = TRUE, df = TRUE) {
  
  # ---------------------------------------------
  # (1) Assemble summary table
  # ---------------------------------------------

  if (coroutput$method == "Pearson's product-moment correlation") 
  {
    outtable <- data.frame(
    coefficient=format(round(coroutput$estimate,2),nsmall = 2),
    n=coroutput$parameter+2,
    p=format(round(coroutput$p.value,3),nsmall = 3)
    )
    
    
    if (stats == TRUE)  # output statistics
    {
       statout = paste(", t(" , coroutput$parameter , ") = " , 
                       format(round(coroutput$statistic,2),nsmall = 2),sep="")  
    } else
    {
      statout = ""
    }
  } else
  {
    outtable <- data.frame(
    coefficient=format(round(coroutput$estimate,2),nsmall = 2),
    p=format(round(coroutput$p.value,3),nsmall= 3)
    )
    
    if (stats == TRUE)  # output statistics
    {
      if (coroutput$method == "Kendall's rank correlation tau")
      {
        statout = paste(", z = " , format(round(coroutput$statistic,2),nsmall=2),sep="")  
      } else if (coroutput$method == "Spearman's rank correlation rho")
      {
        statout = paste(", S = " , format(round(coroutput$statistic,2),nsmall= 2),sep="")  
      }
    } else
    {
      statout = ""
    }
  }
  
  # ---------------------------------------------
  # (2) Format output table
  # ---------------------------------------------
  ####
  
  
  if (coroutput$method == "Pearson's product-moment correlation") 
  {  
     pcorr <- paste(", p = ", outtable$p, sep="")
     pcorr <- gsub("p = 1.000","p > .999", pcorr, fixed=TRUE)
     pcorr <- gsub("p = 0.000","p < .001", pcorr, fixed=TRUE)
     pcorr <- gsub("p = 0","p = ", pcorr, fixed=TRUE)
     
     rcorr <- gsub("0.",".",outtable$coefficient,fixed=TRUE)
     rcorr <- gsub("-0.","-.",rcorr,fixed=TRUE)
    
  if (df == FALSE) {
    outtext <- data.frame(Text=paste("r(",outtable$n,") = ", 
                                     rcorr,statout, pcorr,sep=""));
  } else {
    outtext <- data.frame(Text=paste("r(",outtable$n-2,") = ", 
                                     rcorr,statout, pcorr,sep=""));
  }
  
  }  else if (coroutput$method == "Kendall's rank correlation tau")
  {
     pcorr <- paste(", p = ", outtable$p, sep="")
     pcorr <- gsub("p = 1.000","p > .999", pcorr, fixed=TRUE)
     pcorr <- gsub("p = 0.000","p < .001", pcorr, fixed=TRUE)
     pcorr <- gsub("p = 0","p = ", pcorr, fixed=TRUE)
     
     rcorr <- gsub("0.",".",outtable$coefficient,fixed=TRUE)
     rcorr <- gsub("-.0","-.",rcorr,fixed=TRUE)
    
     outtext <- data.frame(
     Text=paste("tau = " , rcorr,statout, pcorr, sep="")); 
  }  else if (coroutput$method == "Spearman's rank correlation rho")
  {
    
     pcorr <- paste(", p = ", outtable$p, sep="")
     pcorr <- gsub("p = 1.000","p > .999", pcorr, fixed=TRUE)
     pcorr <- gsub("p = 0.000","p < .001", pcorr, fixed=TRUE)
     pcorr <- gsub("p = 0","p = ", pcorr, fixed=TRUE)
     
     rcorr <- gsub("0.",".",outtable$coefficient,fixed=TRUE)
     rcorr <- gsub("-0.","-.",rcorr,fixed=TRUE)
     
     outtext <- data.frame(
     Text=paste("rho = " , rcorr, statout,pcorr,sep=""));
  }
  
  if (print==TRUE) {
    print(outtext);
  } else {
    outtext;  
  }
  
  if (coroutput$method == "Pearson's product-moment correlation") {
  if (df == TRUE) {
    cat("\nNote: As of version 1.11, cor_cout shows r(df) rather than r(N) by default.\n")
  }
  }
  
}
