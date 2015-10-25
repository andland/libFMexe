data("MovieLense", package = "recommenderlab")

# go through some pain to convert to a data.frame
user_to_df = function(i) {
  lst = as(MovieLense, "list")[[i]]
  data.frame(User = i, Movie = names(lst), Rating = as.numeric(lst), stringsAsFactors = FALSE)
}

rating_dfs = lapply(1:nrow(MovieLense), user_to_df)
movie_lense = do.call(rbind, rating_dfs)

movie_lense$User = factor(movie_lense$User)
movie_lense$Movie = factor(movie_lense$Movie)

save(movie_lense, file = "data/movie_lense.rdata", compress = "xz")
