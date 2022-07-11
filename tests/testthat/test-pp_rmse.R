# load data
data('src', package = "populR")
data('trg', package = "populR")

# estimate population counts using awi and vwi
awi <- pp_estimate(trg, src, sid = sid, spop = pop, method = awi)
vwi <- pp_estimate(trg, src, sid = sid, spop = pop, volume = floors, method = vwi)

test_that("argument errors", {
  # test on missing spop
  expect_error(
    pp_rmse(target = awi, source = src, sid = sid, tpop = pp_est,
            title ='awi'),
    "spop is required"
  )

  # test on misspelled target
  expect_error(
    pp_rmse(target = awis, source = src, sid = sid, spop = pop, tpop = pp_est,
            title ='awi'),
    "object 'awis' not found"
  )

  #test on missing source
  expect_error(
    pp_rmse(target = awis, sid = sid, spop = pop, tpop = pp_est,
            title ='awi'),
    "source is required"
  )

})

test_that("check on results", {
  expected_awi <- pp_rmse(target = awi, source = src, sid = sid, spop = pop, tpop = pp_est,
                             title ='awi')
  expected_vwi <- pp_rmse(target = vwi, source = src, sid = sid, spop = pop, tpop = pp_est,
                          title ='vwi')

  # check on return - length is not working as expected though
  expect_length(
    lengths(expected_awi), 4
  )

  expect_length(
    lengths(expected_vwi), 4
  )

  # check on values - both awi and vwi are volume preserving and therefore
  # results are the same - rmse, mae
  expect_equal(
    expected_awi$rmse, expected_vwi$rmse
  )

  expect_equal(
    expected_awi$mae, expected_vwi$mae
  )

})

test_that("non numeric fields", {
  awi$pp_est_text <- as.character(awi$pp_est)
  expect_error(
    pp_rmse(target = awi, source = src, sid = sid, spop = pop, tpop = pp_est_text,
          title ='awi'),
    "pp_est_text must be numeric"
  )
})
