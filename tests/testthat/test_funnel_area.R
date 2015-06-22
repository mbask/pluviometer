test_that("[get_funnel_area()] Funnel area is correctly calculated", {
  expect_equal(get_funnel_area(20), 314.159265358979)
  expect_equal(get_funnel_area(9.5), 70.8821842466197)
})

test_that("[get_funnel_area()] Diameter of funnel is numeric",{
  expect_equal(get_funnel_area("a"), NA_real_)
  expect_equal(get_funnel_area(factor("a")), NA_real_)
  expect_equal(get_funnel_area(c("a", 2)), NA_real_)
})

test_that("[get_funnel_area()] Diameter of funnel is a positive numeric",{
  expect_equal(get_funnel_area(-20), NA_real_)
  expect_equal(get_funnel_area(0), NA_real_)
  expect_equal(get_funnel_area(c(0, 4)), NA_real_)
})

test_that("[get_funnel_area()] Funnel area is vectorized", {
  expect_equal(length(get_funnel_area(c(3, 40))), 2)
})