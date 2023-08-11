require(testthat)
require(CardinalIO)

context("writeImzML")

test_that("writeImzML - continuous", {

	path <- exampleImzMLFile("continuous")
	path2 <- tempfile(fileext=".imzML")
	path3 <- tempfile(fileext=".imzML")

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

	p3 <- as(as(p, "ImzMeta"), "ImzML")

	expect_error(writeImzML(p3))
	expect_error(writeImzML(p3, path3))

	p4 <- p2
	p4$sampleList <- list()
	p4$scanSettingsList <- list()
	p4$softwareList <- list()
	p4$instrumentConfigurationList <- list()
	p4$dataProcessingList <- list()

	success <- writeImzML(p4, path3)

	expect_true(success)

})

test_that("writeImzML - processed", {

	path <- exampleImzMLFile("processed")
	path2 <- tempfile(fileext=".imzML")
	path3 <- tempfile(fileext=".imzML")

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

	p3 <- as(as(p, "ImzMeta"), "ImzML")

	expect_error(writeImzML(p3))
	expect_error(writeImzML(p3, path3))

	p4 <- p2
	p4$sampleList <- list()
	p4$scanSettingsList <- list()
	p4$softwareList <- list()
	p4$instrumentConfigurationList <- list()
	p4$dataProcessingList <- list()

	success <- writeImzML(p4, path3)

	expect_true(success)

})

test_that("writeImzML + ibd - continuous", {

	path <- exampleImzMLFile("continuous")
	path2 <- tempfile(fileext=".imzML")
	path3 <- tempfile(fileext=".imzML")

	p <- parseImzML(path, ibd=TRUE)
	
	mz <- as.numeric(p$ibd$mz[[1L]])
	intensity <- do.call(cbind, as.list(p$ibd$intensity))
	positions <- p$run$spectrumList$positions
	success <- writeImzML(p, path2, positions=positions,
		mz=mz, intensity=intensity)
	
	p2 <- parseImzML(path2, ibd=TRUE)
	mz2 <- as.numeric(p2$ibd$mz[[1L]])
	intensity2 <- do.call(cbind, as.list(p2$ibd$intensity))
	positions2 <- p2$run$spectrumList$positions

	expect_equivalent(mz, mz2)
	expect_equivalent(intensity, intensity2)
	expect_equivalent(positions, positions2)

	file.remove(path2)
	mz3 <- p2$ibd$mz
	intensity3 <- p2$ibd$intensity
	positions3 <- p2$run$spectrumList$positions
	
	success <- writeImzML(p, path2, positions=positions,
		mz=mz3, intensity=intensity3, asis=TRUE)
	
	p3 <- parseImzML(path2, ibd=TRUE)
	
	expect_identical(
		p2$fileDescription$fileContent,
		p3$fileDescription$fileContent)

	e <- ImzMeta(spectrumType="MS1 spectrum",
		spectrumRepresentation="profile spectrum")

	success <- writeImzML(e, path3, positions=positions,
		mz=mz, intensity=intensity)

	expect_true(success)

})

test_that("writeImzML + ibd - processed", {

	path <- exampleImzMLFile("processed")
	path2 <- tempfile(fileext=".imzML")
	path3 <- tempfile(fileext=".imzML")

	p <- parseImzML(path, ibd=TRUE)
	
	mz <- as.list(p$ibd$mz)
	intensity <- as.list(p$ibd$intensity)
	positions <- p$run$spectrumList$positions
	
	success <- writeImzML(p, path2, positions=positions,
		mz=mz, intensity=intensity)

	p2 <- parseImzML(path2, ibd=TRUE)
	mz2 <- as.list(p2$ibd$mz)
	intensity2 <- as.list(p2$ibd$intensity)
	positions2 <- p2$run$spectrumList$positions

	expect_equivalent(mz, mz2)
	expect_equivalent(intensity, intensity2)
	expect_equivalent(positions, positions2)

	file.remove(path2)
	mz3 <- p2$ibd$mz
	intensity3 <- p2$ibd$intensity
	positions3 <- p2$run$spectrumList$positions

	success <- writeImzML(p, path2, positions=positions,
		mz=mz3, intensity=intensity3, asis=TRUE)
	
	p3 <- parseImzML(path2, ibd=TRUE)
	
	expect_identical(
		p2$fileDescription$fileContent,
		p3$fileDescription$fileContent)

	e <- ImzMeta(spectrumType="MS1 spectrum",
		spectrumRepresentation="profile spectrum")

	success <- writeImzML(e, path3, positions=positions,
		mz=mz, intensity=intensity)

	expect_true(success)

})

