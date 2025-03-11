
test_that("example snapshot test", {
  expect_snapshot({
    print("Hello, world!")
    data.frame(a = 1:3, b = 4:6)
  })
})
