test_that("Funnel area is correctly calculated", {
  expect_equal(get_funnel_area(20), 314.159265358979)
  expect_equal(get_funnel_area(9.5), 70.8821842466197)
})

test_that("Diameter of funnel is numeric",{
  expect_equal(get_funnel_area("a"), NA_real_)
  expect_equal(get_funnel_area(factor("a")), NA_real_)
})

test_that("Diameter of funnel is a positive numeric",{
  expect_equal(get_funnel_area(-20), NA_real_)
  expect_equal(get_funnel_area(0), NA_real_)
})
