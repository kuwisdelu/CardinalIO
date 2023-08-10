require(testthat)
require(CardinalIO)

context("ImzMeta")

test_that("ImzMeta - validity", {

	e <- ImzMeta()

	expect_true(validObject(e))

	e$spectrumType <- "MS1 spectrum"
	e$spectrumRepresentation <- "profile"

	expect_true(validObject(e))
	expect_equivalent(e$spectrumType, "MS1 spectrum")
	expect_equivalent(e$spectrumRepresentation, "profile spectrum")

	expect_error(e$spectrumType <- "profile")
	expect_error(e$spectrumRepresentation <- "MS1 spectrum")

	e$contactName <- "Ritsuko Akagi"
	e$contactAffiliation <- "NERV"
	e$contactEmail <- "akagi.ritsuko@gehirn.co.jp"

	expect_true(validObject(e))
	expect_error(e$foo <- "test")
	expect_error(e$bar <- "test")

})

test_that("ImzMeta/ImzML conversion", {

	expect_error(as(ImzMeta(), "ImzML"))

	path <- exampleImzMLFile("continuous")
	p <- parseImzML(path)
	e <- as(p, "ImzMeta")

	expect_true(validObject(e))
	expect_equivalent(e$spectrumType, "MS1 spectrum")
	expect_equivalent(e$spectrumRepresentation, "profile spectrum")

	p2 <- as(e, "ImzML")
	fc <- p2$fileDescription$fileContent

	expect_true(validObject(p2))
	expect_in(find_term("MS1 spectrum", "ms", "accession"), names(fc))
	expect_in(find_term("profile spectrum", "ms", "accession"), names(fc))

})
