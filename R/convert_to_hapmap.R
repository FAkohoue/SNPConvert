#' Convert SNP from numeric (0, 1, 2) format to hampmap
#'
#'
#' @param data a data frame of SNP coded as 0, 1, 2, where each row corresponds to an individual and each column corresponds to a SNP.
#' "0" = homozygous for reference allele, "1" = homozygous for alternative allele, "2" = heterozygous.
#' .
#' @param snp_info a data frame containing SNP information such as the SNP names, reference allele (REF), and alternate allele (ALT) for each SNP.
#' The SNP names in this data frame should match the column names of the data matrix.
#' This information is used to determine the reference and alternate alleles for each SNP when converting the numerical data to hapmap format.
#'
#' @return a data frame containing the SNP data in hapmap format.
#'
#' @references
#'
#'
#' @author
#' Félicien Akohoue
#'
#' @examples
#' # Sample numerical SNP data (725 SNPs x 299 individuals)
#' set.seed(123)  # For reproducibility
#' numerical_data <- matrix(sample(0:2, 725*299, replace = TRUE), nrow = 299, ncol = 725)
#' colnames(numerical_data) <- paste0("SNP", 1:725)  # Assign column names to numerical data
#' Sample second dataset containing SNP information
#' snp_info <- data.frame(SNP = paste0("SNP", 1:725),
#'                    REF = sample(c("A", "C", "G", "T"), 725, replace = TRUE),
#'                     ALT = sample(c("A", "C", "G", "T"), 725, replace = TRUE))
#' rownames(snp_data) <- snp_info$SNP  # Assign SNP names as row names
#'
#' @export
#' @seealso \code{\link{help}}
#'
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

