#' @title Movie Lense 100k ratings
#'
#' @description
#' The 100k MovieLense ratings data set. The data was collected through the MovieLens
#' web site (movielens.umn.edu) during the seven-month period from September 19th, 1997
#' through April 22nd, 1998. The data set contains about 100,000 ratings (1-5) from 943
#' users on 1664 movies.
#'
#' @format A data frame with three variables:
#' \describe{
#' \item{\code{User}}{Factor variable of the user ID who rated the movie}
#' \item{\code{Movie}}{Factor variable of the movie name}
#' \item{\code{Rating}}{A rating from 1 to 5}
#' }
#'
#' @source
#' GroupLens Research, \url{http://www.grouplens.org/node/73}
#'
#' Data reformatted from the recommenderlab R package
#'
#' @references
#'  Herlocker, J., Konstan, J., Borchers, A., Riedl, J.. An Algorithmic Framework
#'  for Performing Collaborative Filtering. Proceedings of the 1999 Conference on
#'  Research and Development in Information Retrieval. Aug. 1999.
#'
#' @examples
#' data(movie_lense)
"movie_lense"
