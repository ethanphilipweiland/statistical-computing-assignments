## Setting working directory
# setwd("/Users/ethanphilipweiland/Library/CloudStorage/OneDrive-IndianaUniversity/Year 2.1 (AU ‘22)/Statistical Computing/Homework/")

## Question 1

mutations_per_sequence <- function(start_seq, end_seq) {
    nchar_start <- nchar(start_seq)
    nchar_end <- nchar(end_seq)
    if (nchar_start != nchar_end) {
      print("ERROR!")
      break
    }
    if (grepl("[^(A|C|G|T)]", start_seq) == TRUE) {
      print("ERROR!")
      break
    }
    if (grepl("[^(A|C|G|T)]", end_seq) == TRUE) {
      print("ERROR!")
      break
    }
    nucleotides_start_seq <- strsplit(start_seq, split="")
    nucleotides_start_seq <- unlist(nucleotides_start_seq)
    nucleotides_end_seq <- strsplit(end_seq, split="")
    nucleotides_end_seq <- unlist(nucleotides_end_seq)
    for (i in 1:length(nucleotides_start_seq)) {
      if (i == 1) {
        num_mutations <- 0
      }
      if (nucleotides_start_seq[i] != nucleotides_end_seq[i]) {
        num_mutations <- num_mutations + 1
      }
    }
    return(num_mutations)
}

## Question 2

## Assume that transition_matrix is a matrix with row and column names (like T below).
## Notice that you can refer to the rows and columns by name in addition to index.
## With T as defined in question 3 below, T["A", "A"] is the 1,1 element of T, T["G", "T"] is the 2,3 element, and so on.
## Using this behavior will make writing this function simpler.

sequence_divergence <- function(start_seq, end_seq, transition_matrix) {
    ## Checking input sequence and output sequence
    nchar_start <- nchar(start_seq)
    nchar_end <- nchar(end_seq)
    if (nchar_start != nchar_end) {
      print("ERROR!")
      break
    }
    if (grepl("[^(A|C|G|T)]", start_seq) == TRUE) {
      print("ERROR!")
      break
    }
    if (grepl("[^(A|C|G|T)]", end_seq) == TRUE) {
      print("ERROR!")
      break
    }
    ## Checking transition matrix
    if (nrow(transition_matrix) != 4) {
      print("ERROR!")
      break
    }
    if (ncol(transition_matrix) != 4) {
      print("ERROR!")
      break
    }
    for (i in 1:nrow(transition_matrix)) {
      for (j in 1:ncol(transition_matrix)) {
        if (transition_matrix[i,j] < 0) {
          print("ERROR!")
          break
        }
      }
    }
    row_sums_T <- rowSums(transition_matrix)
    for (i in row_sums_T) {
      if (i != 1) {
        print("ERROR!")
        break
      }
    }
    #Calculating divergence
    nucleotides_start_seq <- strsplit(start_seq, split="")
    nucleotides_start_seq <- unlist(nucleotides_start_seq)
    nucleotides_end_seq <- strsplit(end_seq, split="")
    nucleotides_end_seq <- unlist(nucleotides_end_seq)
    for (i in 1:length(nucleotides_start_seq)) {
      if (i == 1) { 
        divergence <- 0
      }
      if (nucleotides_start_seq[i] == "A") {
        divergence <- divergence + log(transition_matrix["A",nucleotides_end_seq[i]])
      }
      if (nucleotides_start_seq[i] == "G") {
        divergence <- divergence + log(transition_matrix["G",nucleotides_end_seq[i]])
      }
      if (nucleotides_start_seq[i] == "T") {
        divergence <- divergence + log(transition_matrix["T",nucleotides_end_seq[i]])
      }
      if (nucleotides_start_seq[i] == "C") {
        divergence <- divergence + log(transition_matrix["C",nucleotides_end_seq[i]])
      }
    }
    return(divergence)
}


## Question 3

## Use this matrix T as the transition_matrix in the sequence_divergence function.
T <- matrix(c(.93, .05, .01, .01, .05, .93, .01,.01,
             .01, .01, .93, .05, .01, .01, .05, .93), nrow=4, byrow=TRUE)
rownames(T) <- colnames(T) <- c("A", "G", "T", "C")

sequences <- read.csv("sequences.csv", stringsAsFactors = FALSE)
germline <- readLines("germline.txt")

## Question 4

for (i in 1:nrow(sequences)) {
  if (i == 1) {
    mutation_count_type_a <- c()
    mutation_count_type_b <- c()
  }
  if (sequences[i,"type"] == "a") {
    mutation_count_type_a[i] <- mutations_per_sequence(germline, sequences[i,"seqs"])
  }
  if (sequences[i,"type"] == "b") {
    mutation_count_type_b[(i - length(mutation_count_type_a))] <- mutations_per_sequence(germline, sequences[i,"seqs"])
  }
}

for (i in 1:nrow(sequences)) {
  if (i == 1) {
    sequence_divergence_type_a <- c()
    sequence_divergence_type_b <- c()
  }
  if (sequences[i,"type"] == "a") {
    sequence_divergence_type_a[i] <- sequence_divergence(germline,sequences[i,"seqs"],T)
  }
  if (sequences[i,"type"] == "b") {
    sequence_divergence_type_b[(i - length(sequence_divergence_type_a))] <- sequence_divergence(germline,sequences[i,"seqs"],T)
  }
}

  #Means and standard deviations
  cat("The mean mutation count for experimental condition a is", mean(mutation_count_type_a), fill=TRUE)
  cat("The standard deviation of the mutation count for experimental condition a is", sqrt(var(mutation_count_type_a)), fill=TRUE)
  cat("The mean mutation count for experimental condition b is", mean(mutation_count_type_b), fill=TRUE)
  cat("The standard deviation of the mutation count for experimental condition b is", sqrt(var(mutation_count_type_b)), fill=TRUE)
  cat("The mean likelihood-based statistic for experimental condition a is", mean(sequence_divergence_type_a), fill=TRUE)
  cat("The standard deviation of the likelihood-based statistic for experimental condition a is", sqrt(var(sequence_divergence_type_a)), fill=TRUE)
  cat("The mean likelihood-based statistic for experimental condition b is", mean(sequence_divergence_type_b), fill=TRUE)
  cat("The standard deviation of the likelihood-based statistic for experimental condition b is", sqrt(var(sequence_divergence_type_b)), fill=TRUE)
    
## Question 5
t_test_mutation_count <- t.test(mutation_count_type_a, mutation_count_type_b)
t_test_mutation_count

## Question 6
t_test_sequence_likelihoods <- t.test(sequence_divergence_type_a, sequence_divergence_type_b)
t_test_sequence_likelihoods

## Question 7
cat("The p-value for the two-sample t-test in mutation count between experimental condition a and experimental condition b is", t_test_mutation_count$p.value, fill=TRUE)
cat("The p-value for the two-sample t-test in sequence likelihood between experimental condition a and experimental condition b is", t_test_sequence_likelihoods$p.value, fill=TRUE)
print("The p-value for the two-sample t-test for mutation count suggests failing to reject the null hypothesis. The p-value for the two-sample t-test for sequence likelihood between experimental condition a and experimental condition b suggests rejecting the null hypothesis. I think the first measure (mutation count) did end up being too crude, and the second measure (sequence likelihood) better captured evolutionary changes. Ultimately, this demonstrates the importance of utilizing prior scientific expertise before running statistical analyses. Utlizing the transition matrix of probabilities of changes led to drastically different p-values.")
