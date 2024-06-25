test_that("let works", {
  expect_error(let(f(), f = \() f()))
  expect_equal(letrec(f(10),
                      k = \(y) f(y),
                      f = \(x) if (x) x * k(x - 1) else 1), 3628800)
})
