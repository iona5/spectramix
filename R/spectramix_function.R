
#' create a new weighted spectrum
#'
#' create a new spectrum from numeric weights. the weights can add up to 1 or 100 indicating
#' a plain factor or percentages. if they add up to some other value, indicate via normalize = TRUE,
#' that you want a normalization.
#'
#' @param data a data frame containing the spectra to mix. it is assumed the first column
#' contains the Wavelength labels
#' @param weights a 2-col matrix of the spectra to mix. The first column is the column index of
#' the spectrum in data, the second column is the weight of the spectrum
#' @param normalize if the weights should be normalized
#'
#' @return a data frame containing with the wavelength labels in the first column and the mixed spectrum in the second
#' @export
#'
#' @examples
#'  \donttest{ }
smix = function(data, weights, normalize = FALSE) {

  weights.sum <- sum(weights[,2])


  isPercent <- all.equal(weights.sum, 100)
  isNormal <- all.equal(weights.sum, 1.0)

  isNormalNormalization <- is.logical(isPercent) || is.logical(isNormal)


  if( !isNormalNormalization) {
    if(normalize) {
      warning(sprintf("normalize weights to a basis of %f", weights.sum))
    } else {
      stop(sprintf("The sum of the weights add up to %f (thats NEITHER 1 nor 100!!!), if you really know what you are doing, use normalize = TRUE", weights.sum))
    }
  }



  results = data[,1] * 0

  for(row_no in 1:nrow(weights)) {
    results = results + ( data[,weights[row_no,1] ] * weights[row_no,2] )
  }

  # normalize:
  results <- results/weights.sum


  result_df = data.frame(Wavelength = data[,1], Reflectance = results)

  return(result_df)
}

#' create a new weighted spectrum with spectrum names
#'
#' a convenience function to create a new spectrum from named weights. the weights can add up to 1 or 100 indicating
#' a plain factor or percentages. if they add up to some other value, indicate via normalize = TRUE,
#' that you want a normalization.
#'
#' @param data a data frame containing the spectra to mix. it is assumed the first column
#' contains the Wavelength labels
#' @param weights_df a 1-row data frame with the weights. the column names are the names of the spectra
#' to be mixed from data. the row itself contains its weights
#' @param normalize if the weights should be normalized
#'
#' @return a data frame containing with the wavelength labels in the first column and the mixed spectrum in the second
#' @export
#'
#' @example
#' \donttest{ new_spectrum <- smix_by_names(data, data.frame(
#'   Buche = 0.2,
#'   Tanne = 0.4,
#'   Fichte = 0.4
#' ))
#' }
smix_by_names = function(data, weights_df, normalize = FALSE) {

  matrix_vector = c()

  for(col_no in 1:ncol(weights_df)) {
    name <- colnames(weights_df)[col_no]

    col_num = which( colnames(data) == name )
    matrix_vector = c(matrix_vector, col_num)

    weight <- weights_df[1,col_no]
    matrix_vector = c(matrix_vector, weight)
  }

  matrix =  matrix( matrix_vector, ncol = 2, byrow = TRUE)
  print(matrix)
  return (smix(data, matrix, normalize))
}



#' filter a spectrum by a filter config
#'
#' @param spectrum the spectrum to filter
#' @param filter the filter. right now it is just a vector containing the bandwidth
#' labels from spectrum
#'
#' @return a new spectrum with just the labels and values after filtering
#' @export
#'
#' @examples  \donttest{ }
smix_filter <- function(spectrum, filter) {

  if(ncol(filter) == 1) {
    # just simply the spectra values:

    result <- data.frame(
      Wavelength = filter[,1],
      Reflectance = spectrum[match(filter[,1], spectrum[,1]), 2]
    )
    return(result)
  }
}
