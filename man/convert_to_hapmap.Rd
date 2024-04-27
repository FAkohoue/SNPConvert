\name{convert_to_hapmap}
\alias{convert_to_hapmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert SNP from numeric (0, 1, 2) format to hapmap
%%  ~~function to do ... ~~
}
\description{This function uses basic R functions and constructs to convert a matrix of numerical SNP data into hapmap format based on the SNP information provided.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convert_to_hapmap(data, snp_info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data frame of SNP coded as 0, 1, 2, where each row corresponds to an individual and each column corresponds to a SNP.
  "0" = homozygous for reference allele, "1" = homozygous for alternative allele, "2" = heterozygous.
%%     ~~Describe \code{x} here~~
}
\item{snp_info}{A data frame containing SNP information such as the SNP names, reference allele (REF), and alternate allele (ALT) for each SNP. The SNP names in this data frame should match the column names of the data matrix. This information is used to determine the reference and alternate alleles for each SNP when converting the numerical data to hapmap format.
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{a data frame containing the SNP data in hapmap format.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Félicien Akohoue
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
numerical_data <- matrix(sample(0:2, 725*299, replace = TRUE), nrow = 299, ncol = 725)
colnames(numerical_data) <- paste0("SNP", 1:725)  # Assign column names to numerical data
Sample second dataset containing SNP information
snp_info <- data.frame(SNP = paste0("SNP", 1:725),
                    REF = sample(c("A", "C", "G", "T"), 725, replace = TRUE),
                     ALT = sample(c("A", "C", "G", "T"), 725, replace = TRUE))
rownames(snp_data) <- snp_info$SNP  # Assign SNP names as row names

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as

convert_to_hapmap <- function(data, snp_info) {
  # Check if column names match row names
  if(!all(colnames(data) == rownames(snp_info))) {
    stop("SNP names of marker data do not match row names of SNP information.")
  }

  hapmap_data <- data.frame(matrix("", nrow = nrow(data), ncol = ncol(data), dimnames = list(rownames(data), colnames(data))))

  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (is.na(data[i, j])) {
        hapmap_data[i, j] <- "NA"
      } else if (data[i, j] == 0) {
        hapmap_data[i, j] <- paste0(snp_info$REF[j], "", snp_info$REF[j])
      } else if (data[i, j] == 2) {
        hapmap_data[i, j] <- paste0(snp_info$ALT[j], "", snp_info$ALT[j])
      } else if (data[i, j] == 1) {
        hapmap_data[i, j] <- paste0(snp_info$REF[j], "", snp_info$ALT[j])
      } else {
        hapmap_data[i, j] <- "NA"
      }
    }
  }

  return(hapmap_data)
}

{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
