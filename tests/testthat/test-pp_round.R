# load data
data('src', package = "populR")
data('trg', package = "populR")

# estimate population counts using awi and vwi
awi <- pp_estimate(trg, src, sid = sid, spop = pop, method = awi)
vwi <- pp_estimate(trg, src, sid = sid, spop = pop, volume = floors, method = vwi)
awi$pp_est_text <- as.character(awi$pp_est)

test_that("argument errors", {
  # misspelled object
  expect_error(
    pp_round(target = awis, tpop = pp_est, spop = pop, sid = sid),
    "object 'awis' not found"
  )

  # misspelled object
  expect_error(
    pp_round(tpop = pp_est, spop = pop, sid = sid),
    "target is required"
  )

  # missing spop
  expect_error(
    pp_round(tpop = pp_est, sid = sid),
    "target is required"
  )

  # missplelled sid
  expect_error(
    pp_round(target = awi, tpop = pp_est, spop = pop, sid = sids),
    "sids cannot be found in the given target object"
  )
})

test_that("non numeric fields", {
  expect_error(
    pp_round(target = awi, tpop = pp_est_text, spop = pop, sid = sid),
    "pp_est_text must be numeric"
  )
})
