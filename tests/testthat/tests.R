#####

context("creation")

test_that( "navr makes a htmlwidget ", {
  expect_is( navr( "nav1", htmltools::tagList() ), "htmlwidget" )
  expect_is( navr( "navr", htmltools::tagList() ), "navr" )
})

test_that( "navr checks for bad parameters",{
  # should throw error if selector not a character
  expect_error( navr( NULL ) )
  # should throw error if taglist not a htmltools::tagList or a htmltools::HTML
  expect_error( navr(taglist = "") )
  # NULL or empty list should be valid for options, so no error here
  #   just warning for a taglist with length == 0
  #   this tests both no error for NULL options and warning for taglist of length 0
  expect_warning( navr() )
})
