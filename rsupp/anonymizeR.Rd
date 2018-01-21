\name{AnonymizeR}
\alias{AnonymizeR}
\alias{plot.AnonymizeR}
\title{Data Anonymization for R}
\description{
AnonymizeR is a privacy tool that enables users to enforce privacy standards through minimal random data suppression. 

\itemize{
  \item 
}
}

\usage{
anonymizeR.par (alpha = 15, gamma = 0.8, n.burn = 200L, n.samples = 1000L,
                      n.chains = 8L, rowSwap.prob = 0.45, colSwap.prob = 0.25, na.prob = 0.95)

}
\arguments{
   \item{ }{
     
   }
}

\usage{
anonymizeWorker(x, varTypes, risk.f, na.risk.within, par, skip.rinit, verbose)

}
\arguments{
   \item{x}{
     Data frame containing the observations with sensitive values. Rows should correspond to observations and columns to variables.
   }
   \item{varTypes}{
     Describes the variable types for the purposes of anonymization in the data frame. Types include key variables, strata variables, non-strata variables, and diversity variables ("keyVars","strataVars","nonStrataVars","divVar").
   }
   \item{na.risk.within}{
    Should NAs be treated as a specific attribute value? For example, if anonymizing age ranges, should suppressed values of NA be considered alongside "young","middle-aged", and "elderly"? Especially applies when trying to prevent inferences about individual attributes based on group characteristics. 
   \item{par}{
    parameters to be passed the random suppression process.
    }
   \item{skip.rinit}{
     
    }
   \item{verbose}{
    Set to TRUE to see more detailed progress during the anonymization process. 
    }
}

\usage{
calcRisk(x, keyVars, strataVars = NULL, divVar = NULL, risk.f = NULL, na.risk.within = FALSE)

}
\arguments{
   \item{x}{
     Data frame containing the observations with sensitive values. Rows should correspond to observations and columns to variables.
   }

   \item{keyVars}{
     Key variables contain the values that could be used together to re-identify individuals. For example, key variables might include all the demographic attributes that could be used to narrow potential matches to a single individual. 
   }

   \item{strataVars}{
     Strata variables can be used to subset observations and their key variable values into meaningful groups. Privacy standards are enforced within each strata. For example, in a large dataset containing observations about many individuals, a strata variable might be geography, which could be used to reduce the number of individuals who share particular demographic characteristics. 
   }

   \item{divVar}{
    Diversity variables that contain the sensitive outcomes or other sensitive information that might be associated with an observation. For example, in a dataset of hospital patients, this might include a diagnostic code. In a dataset describing individual arrests, the reason for arrest might be treated as a diversity variable. 
   }

   \item{risk.f}{
    The risk function that ought to be used for determining which observations and corresponding values need to be protected. Can be defined by the user. The default is k-anonymity, which ensures that at least k individuals within each strata share any set of key variable values. l-diversity is also built-in and can be called using risk.f="l-div". 
   }
   
   \item{na.risk.within}{
    Should NAs be treated as a specific attribute value? For example, if anonymizing age ranges, should suppressed values of NA be considered alongside "young","middle-aged", and "elderly"? Especially applies when trying to prevent inferences about individual attributes based on group characteristics. 

   }
   }


\usage{
localSuppression(x, keyVars, strataVars = NULL, divVar = NULL, risk.f = NULL, na.risk.within = FALSE,
           risk.k = 5, n.threads = parallel::detectCores(), n.chains = max(8L, n.threads), keyVars.w = NULL, verbose = FALSE)

}
\arguments{
   \item{x}{
     Data frame containing the observations with sensitive values. Rows should correspond to observations and columns to variables.
   }

   \item{keyVars}{
     Key variables contain the values that could be used together to re-identify individuals. For example, key variables might include all the demographic attributes that could be used to narrow potential matches to a single individual. 
   }

   \item{strataVars}{
     Strata variables can be used to subset observations and their key variable values into meaningful groups. Privacy standards are enforced within each strata. For example, in a large dataset containing observations about many individuals, a strata variable might be geography, which could be used to reduce the number of individuals who share particular demographic characteristics. 
   }

   \item{divVar}{
    Diversity variables that contain the sensitive outcomes or other sensitive information that might be associated with an observation. For example, in a dataset of hospital patients, this might include a diagnostic code. In a dataset describing individual arrests, the reason for arrest might be treated as a diversity variable. 
   }

   \item{risk.f}{
    The risk function that ought to be used for determining which observations and corresponding values need to be protected. Can be defined by the user. The default is k-anonymity, which ensures that at least k individuals within each strata share any set of key variable values. l-diversity is also built-in and can be called using risk.f="l-div". 
   }
   
   \item{na.risk.within}{
    Should NAs be treated as a specific attribute value? For example, if anonymizing age ranges, should suppressed values of NA be considered alongside "young","middle-aged", and "elderly"? Especially applies when trying to prevent inferences about individual attributes based on group characteristics. 

   }
   
   \item{risk.k}{
    If using the default method of k-anonymity, sets the value of k. 
    
   }

 \item{n.threads}{
    Number of threads to use.

   }

 \item{n.chains}{
    Number of chains to initialize. 

   }
   
    \item{keyVars.w}{
    Weighting scheme to apply to the variables. User can provide weights for each variable, which are scaled relative to each other. Weights are used both to determine what fraction of NA attempts will be applied to each variable as well as to adjust the penalty of NA-ing values from each variable. Recommend starting with relatively similar values and making them more extreme after observing the initial results. If not specified, variables are treated equally. 

   }
   
    \item{verbose}{
    Set to TRUE to see more detailed progress during the anonymization process. 

   }


\usage{
getAtRiskSubset(x, keyVars = colnames(x), divVar = NULL, risk.f = NULL, na.risk.within = FALSE, risk.k = 5)

}
\arguments{
   \item{x}{
     Data frame containing the observations with sensitive values. Rows should correspond to observations and columns to variables.
   }

   \item{keyVars}{
     Key variables contain the values that could be used together to re-identify individuals. For example, key variables might include all the demographic attributes that could be used to narrow potential matches to a single individual. 
   }

   \item{strataVars}{
     Strata variables can be used to subset observations and their key variable values into meaningful groups. Privacy standards are enforced within each strata. For example, in a large dataset containing observations about many individuals, a strata variable might be geography, which could be used to reduce the number of individuals who share particular demographic characteristics. 
   }

   \item{divVar}{
    Diversity variables that contain the sensitive outcomes or other sensitive information that might be associated with an observation. For example, in a dataset of hospital patients, this might include a diagnostic code. In a dataset describing individual arrests, the reason for arrest might be treated as a diversity variable. 
   }

   \item{risk.f}{
    The risk function that ought to be used for determining which observations and corresponding values need to be protected. Can be defined by the user. The default is k-anonymity, which ensures that at least k individuals within each strata share any set of key variable values. l-diversity is also built-in and can be called using risk.f="l-div". 
   }
   
   \item{na.risk.within}{
    Should NAs be treated as a specific attribute value? For example, if anonymizing age ranges, should suppressed values of NA be considered alongside "young","middle-aged", and "elderly"? Especially applies when trying to prevent inferences about individual attributes based on group characteristics. 

   }
   
   \item{risk.k}{
    If using the default method of k-anonymity, sets the value of k. 
    
   }
}

\usage{
getRunVariables(vars, keyVars, strataVars, divVar)

}
\arguments{
   \item{vars}{
     All variables in the data frame.
   }
   
   \item{keyVars}{
     Key variables contain the values that could be used together to re-identify individuals. For example, key variables might include all the demographic attributes that could be used to narrow potential matches to a single individual. 
   }
   
      \item{strataVars}{
     Strata variables can be used to subset observations and their key variable values into meaningful groups. Privacy standards are enforced within each strata. For example, in a large dataset containing observations about many individuals, a strata variable might be geography, which could be used to reduce the number of individuals who share particular demographic characteristics. 
   }

   \item{divVar}{
    Diversity variables that contain the sensitive outcomes or other sensitive information that might be associated with an observation. For example, in a dataset of hospital patients, this might include a diagnostic code. In a dataset describing individual arrests, the reason for arrest might be treated as a diversity variable. 
   }

}




\details{
anonymizeR provides flexible functions that allow users to define risk measures, thresholds for those measures, and to enforce them through random suppression of data values. anonymizeR has commonly used risk measures, such as k-anonymity and l-diversity, but also allows users to define custom risk measures to be passed to the random suppression function. Users can specify the order in which anonymizeR ought to try suppressing variable values (e.g., age before race). After suppressing data to meet or exceed privacy standards, anonymizeR evaluates whether any data values can be restored while maintaining those standards. Because anonymizeR makes use of randomness based on the order of the data and a random seed, the results of anonymizeR are difficult for an attacker to reverse engineer, even with knowledge of the privacy standards and thresholds used. 

Open data efforts often require posting of some version of original, not synthetic, data on a website. Basic disclosure control steps are still essential, but can be implemented by users manually. These steps including removing direct identifiers, rounding, binning data into ranges (including top and bottom coding), and combining small groups into larger groups. 
anonymizeR is meant to perform a last essential last step in data privacy protection. It allows for the enforcement of common and user-defined privacy standards through minimal suppression of data values. Mostly written in C, anonymizeR allows for enforcement of data privacy standards on data sets containing hundreds of millions of observations. 


  \subsection{}{

  }
}
\value{
  \code{localSuppression} returns a data frame. 

\references{

}

\author{
Vincent Dorie: \email{vdorie@gmail.com}.
Eric Giannella: \email{eric.giannella@gmail.com}.
}

\seealso{
\code{\link{ }}
}
\examples{
#EG to fill this in


}
\keyword{anonymization}
\keyword{statistical}
\keyword{disclosure}
\keyword{control}
\keyword{privacy}
\keyword{random}
\keyword{suppression}
