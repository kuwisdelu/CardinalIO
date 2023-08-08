require(testthat)
require(CardinalIO)

context("parseimzML")

test_that("parseImzML", {

	ims <- get_obo("ims")
	path <- exampleImzMLFile("continuous")
	p <- parseImzML(path)
	fc <- p$fileDescription$fileContent
	ss1 <- p$scanSettingsList[[1L]]

	expect_in(find_term("continuous", value="accession"), names(fc))
	expect_in(find_term("top down", value="accession"), names(ss1))
	expect_in(find_term("flyback", value="accession"), names(ss1))
	expect_in(find_term("horizontal line scan", value="accession"), names(ss1))
	expect_in(find_term("linescan left right", value="accession"), names(ss1))

	mcpx <- find_term("max count of pixels x", value="accession")
	mcpy <- find_term("max count of pixels y", value="accession")
	mdx <- find_term("max dimension x", value="accession")
	mdy <- find_term("max dimension y", value="accession")
	psx <- find_term("pixel size (x)", value="accession")

	expect_equivalent(ss1[[mcpx]]["value"], "3")
	expect_equivalent(ss1[[mcpy]]["value"], "3")
	expect_equivalent(ss1[[mdx]]["value"], "300")
	expect_equivalent(ss1[[mdy]]["value"], "300")
	expect_equivalent(ss1[[psx]]["value"], "100.0")

	pos <- p$run$spectrumList$positions
	mza <- p$run$spectrumList$mzArrays
	ia <- p$run$spectrumList$intensityArrays
	coords <- expand.grid(
		"position x"=as.character(1:3),
		"position y"=as.character(1:3), stringsAsFactors=FALSE)
	offsets <- as.character(seq(33612, by=33596, length.out=9))
	
	expect_equivalent(pos[,c(1L, 2L)], coords)
	expect_setequal(mza[["external offset"]], "16")
	expect_setequal(mza[["external array length"]], "8399")
	expect_setequal(mza[["external encoded length"]], "33596")
	expect_setequal(mza[["binary data type"]], "32-bit float")

	expect_setequal(ia[["external offset"]], offsets)
	expect_setequal(ia[["external array length"]], "8399")
	expect_setequal(ia[["external encoded length"]], "33596")
	expect_setequal(ia[["binary data type"]], "32-bit float")

})

test_that("parseImzML - processed", {

	ims <- get_obo("ims")
	path <- exampleImzMLFile("processed")
	p <- parseImzML(path)
	fc <- p$fileDescription$fileContent
	ss1 <- p$scanSettingsList[[1L]]

	expect_in(find_term("processed", value="accession"), names(fc))
	expect_in(find_term("top down", value="accession"), names(ss1))
	expect_in(find_term("flyback", value="accession"), names(ss1))
	expect_in(find_term("horizontal line scan", value="accession"), names(ss1))
	expect_in(find_term("linescan left right", value="accession"), names(ss1))

	mcpx <- find_term("max count of pixels x", value="accession")
	mcpy <- find_term("max count of pixels y", value="accession")
	mdx <- find_term("max dimension x", value="accession")
	mdy <- find_term("max dimension y", value="accession")
	psx <- find_term("pixel size (x)", value="accession")

	expect_equivalent(ss1[[mcpx]]["value"], "3")
	expect_equivalent(ss1[[mcpy]]["value"], "3")
	expect_equivalent(ss1[[mdx]]["value"], "300")
	expect_equivalent(ss1[[mdy]]["value"], "300")
	expect_equivalent(ss1[[psx]]["value"], "100.0")

	pos <- p$run$spectrumList$positions
	mza <- p$run$spectrumList$mzArrays
	ia <- p$run$spectrumList$intensityArrays
	coords <- expand.grid(
		"position x"=as.character(1:3),
		"position y"=as.character(1:3), stringsAsFactors=FALSE)
	
	mzoffsets <- as.character(seq(16, by=2*33596, length.out=9))
	ioffsets <- as.character(seq(33612, by=2*33596, length.out=9))
	
	expect_equivalent(pos[,c(1L, 2L)], coords)
	expect_setequal(mza[["external offset"]], mzoffsets)
	expect_setequal(mza[["external array length"]], "8399")
	expect_setequal(mza[["external encoded length"]], "33596")
	expect_setequal(mza[["binary data type"]], "32-bit float")

	expect_setequal(ia[["external offset"]], ioffsets)
	expect_setequal(ia[["external array length"]], "8399")
	expect_setequal(ia[["external encoded length"]], "33596")
	expect_setequal(ia[["binary data type"]], "32-bit float")

})

