test_that("Test_Toto", {
  expect_equal(toto(2), 4)
})

test_that("Testing_Errors",{
  expect_error(1 / "a","non-numeric argument")
  expect_error(str_duq(1:2, 1:3), "could not find function \"str_duq\"")
})
