test_that("get_input() extracts data matrix from technical drawing image", {
  expect_equal(get_input(filename='SADR010324.jpg',trim=10,thr=0.9,wd='~/myRpacks/morphotype/inst/extdata'),
               list("matrix" = image_denoised, "alternative" = NULL))
})

# test_that("get_input() errors if the file extension is not provided", {
#   expect_error(get_input(filename='SADR010324',trim=10,thr=0.9,wd='~/myRpacks/morphotype/inst/extdata'),
#                list("matrix" = image_denoised, "alternative" = NULL))
# })
