context("data")

data("movie_lens")
dat = tail(movie_lens, n = 6)

movie_lens_libFM1 = model_frame_libFM(Rating ~ User + Movie, dat)
should_be1 = c("2 942:1 1961:1", "2 942:1 1155:1", "4 942:1 2158:1", "3 942:1 2604:1", "3 942:1 2504:1", "3 942:1 1012:1")

movie_lens_mm = model.matrix(Rating ~ User + Movie - 1, data = dat)
movie_lens_libFM2 = matrix_libFM(movie_lens_mm, dat$Rating)
should_be2 = c("2 942:1 1960:1", "2 942:1 1154:1", "4 942:1 2157:1", "3 942:1 2603:1", "3 942:1 2503:1", "3 942:1 1011:1")

dat2 = rbind(head(movie_lens, n = 100), tail(movie_lens, n = 100))
movie_lens_mm_mat = model.matrix(Rating ~ User + Movie - 1, data = dat2)
movie_lens_mm_sp = Matrix::sparse.model.matrix(Rating ~ User + Movie - 1, data = dat2)
movie_lens_libFM_mat = matrix_libFM(movie_lens_mm_mat, dat2$Rating)
movie_lens_libFM_sp = sp_matrix_libFM(movie_lens_mm_sp, dat2$Rating)

test_that("correct classes", {
  expect_is(movie_lens_libFM1, "character")
  expect_is(movie_lens_libFM2, "character")
  expect_is(movie_lens_libFM_sp, "character")
})

test_that("correct values", {
  expect_identical(movie_lens_libFM1, should_be1)
  expect_identical(movie_lens_libFM2, should_be2)
})

test_that("matrix = sparse matrix", {
  expect_identical(movie_lens_libFM_mat, movie_lens_libFM_sp)
})
