
mydf <- st_locate(sample_raw_data,
                  adm0 = regions$adm0$Canada,
                  adm1 = regions$adm1$Canada$Alberta,
                  adm2 = regions$adm2$Canada$Alberta$Calgary,
                  into = c("adm1_new", "adm2_new"))


test_that("new columns are generated", {
  expect_equal(names(mydf)[c(ncol(mydf)-1, ncol(mydf))],
               c("adm1_new", "adm2_new"))
})


test_that("missing adm1 invokes error", {
  expect_error(
    st_locate(sample_raw_data,
              adm0 = regions$adm0$Canada,
              adm2 = regions$adm2$Canada$Alberta$Calgary)
  )
})
