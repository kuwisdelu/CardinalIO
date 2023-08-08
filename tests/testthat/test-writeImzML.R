require(testthat)
require(CardinalIO)

context("writeImzML")

test_that("writeImzML - continuous", {

	path <- exampleImzMLFile("continuous")
	path2 <- tempfile(fileext=".imzML")

	p <- parseImzML(path)
	success <- writeImzML(p, path2)
	
	expect_true(success)

	p2 <- parseImzML(path2)

	expect_identical(p$fileDescription, p2$fileDescription)
	expect_identical(p$sampleList, p2$sampleList)
	expect_identical(p$scanSettingsList, p2$scanSettingsList)
	expect_identical(p$softwareList, p2$softwareList)
	expect_identical(p$instrumentConfigurationList, p2$instrumentConfigurationList)
	expect_identical(p$dataProcessingList, p2$dataProcessingList)

	expect_equivalent(p$run$spectrumList$positions, p2$run$spectrumList$positions)
	expect_equivalent(p$run$spectrumList$mzArrays, p2$run$spectrumList$mzArrays)
	expect_equivalent(p$run$spectrumList$intensityArrays, p2$run$spectrumList$intensityArrays)

})

test_that("writeImzML - processed", {

	path <- exampleImzMLFile("processed")
	path2 <- tempfile(fileext=".imzML")

	p <- parseImzML(path)
	success <- writeImzML(p, path2)
	
	expect_true(success)

	p2 <- parseImzML(path2)

	expect_identical(p$fileDescription, p2$fileDescription)
	expect_identical(p$sampleList, p2$sampleList)
	expect_identical(p$scanSettingsList, p2$scanSettingsList)
	expect_identical(p$softwareList, p2$softwareList)
	expect_identical(p$instrumentConfigurationList, p2$instrumentConfigurationList)
	expect_identical(p$dataProcessingList, p2$dataProcessingList)

	expect_equivalent(p$run$spectrumList$positions, p2$run$spectrumList$positions)
	expect_equivalent(p$run$spectrumList$mzArrays, p2$run$spectrumList$mzArrays)
	expect_equivalent(p$run$spectrumList$intensityArrays, p2$run$spectrumList$intensityArrays)

})
