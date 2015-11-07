data("MovieLense", package = "recommenderlab")

# go through some pain to convert to a data.frame
user_to_df = function(i) {
  lst = as(MovieLense, "list")[[i]]
  data.frame(User = i, Movie = names(lst), Rating = as.numeric(lst), stringsAsFactors = FALSE)
}

rating_dfs = lapply(1:nrow(MovieLense), user_to_df)
movie_lens = do.call(rbind, rating_dfs)

movie_lens$User = factor(movie_lens$User)
movie_lens$Movie = factor(movie_lens$Movie)

save(movie_lens, file = "data/movie_lens.rdata", compress = "xz")
