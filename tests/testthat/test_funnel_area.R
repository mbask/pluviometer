test_that("[get_funnel_area()] Funnel area is numeric", {
   expect_is(get_funnel_area(3), "numeric")
 })

test_that("[get_funnel_area()] Funnel area is correctly calculated", {
  expect_equal(get_funnel_area(20), 314.159265358979)
  expect_equal(get_funnel_area(9.5), 70.8821842466197)
})

test_that("[get_funnel_area()] Diameter of funnel is numeric",{
  expect_error(get_funnel_area("a"), "not a numeric")
  expect_error(get_funnel_area(factor("a")), "not a numeric")
  expect_error(get_funnel_area(c("a", 2)), "not a numeric")
})

test_that("[get_funnel_area()] Diameter of funnel is a positive numeric",{
  expect_warning(get_funnel_area(-20), "Zero or negative")
  expect_warning(get_funnel_area(0), "Zero or negative")
  expect_warning(get_funnel_area(c(0, 4)), "Zero or negative")
})

test_that("[get_funnel_area()] Funnel area is vectorized", {
  expect_equal(length(get_funnel_area(c(3, 40))), 2)
})
