## Homework 4: Functions to be debugged

## 1. Berhu penalty

berhu <- function(x, delta) {
    ##Should be less than or equal to delta, not just less than delta
    if(abs(x) <= delta) {
        abs(x)
    } else {
    ##Cleaned up these lines to make order of operations clear
        (x^2 / (2*delta)) + (delta / 2) 
    }
}

berhu(1,1) ## should be 1
berhu(.5, 1) ## should be .5
berhu(2, 1) ## should be 2.5
xseq = seq(-3, 3, length.out = 200)
plot(sapply(xseq, berhu, 1) ~ xseq, type = 'l')


## 2. Trimmed mean

trimmed_mean <- function(x, trim) {
    n = length(x)
    ## the number to trim on each end
    n_to_trim = floor(trim * length(x))
    ## sort the vector
    x_sorted = sort(x)
    ## the indices of the elements to trim at the low end
    lo_idx = 1:n_to_trim
    ## the indices of the elements to trim at the high end
    hi_idx = (n - n_to_trim):n
    ## get rid of the lowest elements
    ##Replaced the "!" with "-" because "-" is the proper way to subset
    x_trimmed = x_sorted[-lo_idx]
    ## get rid of the highest elements
    ##Replaced the "!" with "-" because "-" is the proper way to subset
    x_trimmed = x_trimmed[-hi_idx]
    return(mean(x_trimmed))
}

## should give 2.75
trimmed_mean(c(-20, 1, 3, 2, 2, 5, 20, 2, 3, 4), trim = .1)
mean(c(-20, 1, 3, 2, 2, 5, 20, 2, 3, 4), trim = .1)
## should give -9
trimmed_mean(c(-20, -20,  20, 2), trim = .25)
mean(c(-20, -20,  20, 2), trim = .25)


## 3. String processing

process_table = function(str) {
    ## split by line
    ##Loading "readr" package so read_lines can be used
    library("readr")
    lines = read_lines(str)
    ## remove padding lines
    padding_lines = lines == "|-"
    ##Should be a "!" instead of a "-" 
    lines = lines[!padding_lines]
    ## remove the class definition line, header line, footer line
    class_line = grep("class=", lines)
    header_line = grep("! Rank !! Overall Ranking", lines)
    footer_line = grep("\\|}", lines)
    lines = lines[-c(class_line, header_line, footer_line)]
    ## apply the process_data_line function to all of the lines
    plyr::adply(lines, 1, process_data_line)
}

process_data_line = function(str) {
  split = strsplit(str, ' *\\|\\|? +')[[1]]
  ## extract the city
  ##Need to change regular expression so that it picks up cities spelled with non-English letters
  city_pattern = "\\[\\[[A-z].*\\]\\]"
  city_idx = grep(city_pattern, split)
  city = regmatches(split[city_idx], regexpr(pattern = city_pattern, split[city_idx]))
  city = gsub("\\[|\\]", "", x = city, perl = TRUE)
  ## extract the rankings and scores
  row = lapply(split[-city_idx], function(x) {
    return(as.numeric(x))
  })
  row = c(row, city)
  row[is.na(row)] = NULL
  #Changing the names to those specified in the assignment
  names(row) = c("Rank", "Old Rank", "Score", "City")
  row = data.frame(row)
  return(row)
}

city_rankings = '{| class="wikitable sortable"
|-
! Rank !! Overall Ranking 2017 !! City !! 2010 Score
|-
| 1 || 1 || {{flagicon|Austria}} [[Vienna]] || 108.6
|-
| 2 || 2 || {{flagicon|Switzerland}} [[Zürich]] || 108.0
|-
| 3 || 4 || {{flagicon|Germany}} [[Munich]] || 107.0
|-
| 4 || 6 || {{flagicon|Germany}} [[Düsseldorf]] || 107.2
|-
| 5 || 7 || {{flagicon|Germany}} [[Frankfurt]] || 107.0
|}'
process_table(city_rankings)


## 4. Gradient descent
gradient_descent <- function(fn, deriv, start, step_size, epsilon) {
  x = start
  while(TRUE) {
    ##This should be a difference, not a sum
    new_x = x - step_size * deriv(x)
    ##Not the correct expression in this for loop argument
    if(abs(fn(new_x) - fn(x)) < epsilon) {
      break
    }
    x = new_x
  }
  ##Should return new_x not x
  return(new_x)
}

## should return something close to 0
gradient_descent(function(x) x^2, function(x) 2 * x , start = 1,
                 step_size = .1, epsilon = 1e-10)

## 5. Line search
backtrack_desc <- function(fn, deriv, start, alpha, beta, epsilon) {
    x = start
    while(TRUE) {
        step_size = backtrack(fn, deriv, x, alpha, beta)
        ##Should just be x rather than fn(x)
        new_x = x - step_size * deriv(x)
        ##Argument of this if loop is incorrect
        if(abs(fn(new_x) - fn(x)) < epsilon) {
            break
        }
        x = new_x
    }
    ##Should return new_x not x
    return(new_x)
}

backtrack <- function(fn, deriv, x, alpha, beta) {
    t = 1
    while(fn(x - t * deriv(x)) > (fn(x) - alpha * t * deriv(x)^2)) {
        t = beta * t
    }
    ##Function doesn't return t once argument of while loop isn't met
    return(t)
}

## should return something close to 0
backtrack_desc(function(x) x^2, function(x) 2 * x, start = 10,
               alpha = .03, beta = .8, epsilon = 1e-10)
backtrack_desc(function(x) x^2, function(x) 2 * x, start = 1,
               alpha = .03, beta = .8, epsilon = 1e-10)
