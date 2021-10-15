test_that("base intermediaia aneel funciona", {

  
  expect_snapshot_value(
    cria_base_intermediaria_aneel(),
    style = "serialize" 
  )  
  
  
})
