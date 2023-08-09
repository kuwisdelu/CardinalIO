require(testthat)
require(CardinalIO)

context("parseimzML")

test_that("parseImzML - continuous", {

	path <- exampleImzMLFile("continuous")
	p <- parseImzML(path)
	
	fc <- p$fileDescription$fileContent
	ssl <- p$scanSettingsList[[1L]]

	expect_in(find_term("continuous", value="accession"), names(fc))
	expect_in(find_term("top down", value="accession"), names(ssl))
	expect_in(find_term("flyback", value="accession"), names(ssl))
	expect_in(find_term("horizontal line scan", value="accession"), names(ssl))
	expect_in(find_term("linescan left right", value="accession"), names(ssl))

	mcpx <- find_term("max count of pixels x", value="accession")
	mcpy <- find_term("max count of pixels y", value="accession")
	mdx <- find_term("max dimension x", value="accession")
	mdy <- find_term("max dimension y", value="accession")
	psx <- find_term("pixel size (x)", value="accession")

	expect_equivalent(ssl[[mcpx]]["value"], "3")
	expect_equivalent(ssl[[mcpy]]["value"], "3")
	expect_equivalent(ssl[[mdx]]["value"], "300")
	expect_equivalent(ssl[[mdy]]["value"], "300")
	expect_equivalent(ssl[[psx]]["value"], "100.0")

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

	path <- exampleImzMLFile("processed")
	p <- parseImzML(path)

	fc <- p$fileDescription$fileContent
	ssl <- p$scanSettingsList[[1L]]

	expect_in(find_term("processed", value="accession"), names(fc))
	expect_in(find_term("top down", value="accession"), names(ssl))
	expect_in(find_term("flyback", value="accession"), names(ssl))
	expect_in(find_term("horizontal line scan", value="accession"), names(ssl))
	expect_in(find_term("linescan left right", value="accession"), names(ssl))

	mcpx <- find_term("max count of pixels x", value="accession")
	mcpy <- find_term("max count of pixels y", value="accession")
	mdx <- find_term("max dimension x", value="accession")
	mdy <- find_term("max dimension y", value="accession")
	psx <- find_term("pixel size (x)", value="accession")

	expect_equivalent(ssl[[mcpx]]["value"], "3")
	expect_equivalent(ssl[[mcpy]]["value"], "3")
	expect_equivalent(ssl[[mdx]]["value"], "300")
	expect_equivalent(ssl[[mdy]]["value"], "300")
	expect_equivalent(ssl[[psx]]["value"], "100.0")

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

test_that("parseImzML - ibd/extra", {

	tic <- c(
		"121.85039039868471",
		"182.31835420101888",
		"161.8091904482675",
		"200.9633277092539",
		"135.30584173158496",
		"108.39597418421639",
		"127.84664447846832",
		"168.27018147522492",
		"243.5395066031077")

	path <- exampleImzMLFile("continuous")
	path2 <- exampleImzMLFile("processed")
	p <- parseImzML(path, ibd=TRUE, extra="total ion current")
	p2 <- parseImzML(path2, ibd=TRUE, extra="total ion current")
	
	m <- as.list(mz(p))
	m2 <- as.list(mz(p2))

	i <- as.list(intensity(p))
	i2 <- as.list(intensity(p2))

	expect_equal(m, m2)
	expect_equal(i, i2)

	sl <- p$run$spectrumList
	sl2 <- p2$run$spectrumList

	expect_equal(tic, sl$extra[["total ion current"]])
	expect_equal(tic, sl2$extra[["total ion current"]])

})

