require(testthat)
require(CardinalIO)

context("parseimzML")

test_that("parseImzML - continuous", {

	path <- exampleImzMLFile("continuous")
	p <- parseImzML(path)
	
	fc <- p$fileDescription$fileContent
	ss1 <- p$scanSettingsList[[1L]]

	expect_length(find_descendants_in(fc, "IMS:1000003"), 1L)
	expect_length(find_descendants_in(fc, "IMS:1000008"), 1L)
	expect_length(find_descendants_in(fc, "IMS:1000009"), 1L)

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

	path <- exampleImzMLFile("processed")
	p <- parseImzML(path)

	fc <- p$fileDescription$fileContent
	ss1 <- p$scanSettingsList[[1L]]

	expect_length(find_descendants_in(fc, "IMS:1000003"), 1L)
	expect_length(find_descendants_in(fc, "IMS:1000008"), 1L)
	expect_length(find_descendants_in(fc, "IMS:1000009"), 1L)

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
	p <- parseImzML(path, ibd=TRUE,
		extra=c("total ion current", "MS:1000285"))
	p2 <- parseImzML(path2, ibd=TRUE,
		extra=c("total ion current", "MS:1000285"))
	
	m <- as.list(p$ibd$mz)
	m2 <- as.list(p2$ibd$mz)

	i <- as.list(p$ibd$intensity)
	i2 <- as.list(p2$ibd$intensity)

	expect_equal(m, m2)
	expect_equal(i, i2)

	sl <- p$run$spectrumList
	sl2 <- p2$run$spectrumList

	expect_equal(tic, sl$extra[["total ion current"]])
	expect_equal(tic, sl2$extra[["total ion current"]])
	
	expect_equal(tic, sl$extra[["MS:1000285"]])
	expect_equal(tic, sl2$extra[["MS:1000285"]])

	p3 <- parseImzML(path2, ibd=TRUE,
		extraArrays=c("m/z array", "MS:1000514"))

	sl3 <- p3$run$spectrumList

	expect_equal(sl3$mzArrays, sl3$extraArrays[[1L]])
	expect_equal(sl3$mzArrays, sl3$extraArrays[[2L]])
	expect_equal(p2$ibd$mz, p3$ibd$extra[[1L]])
	expect_equal(p2$ibd$mz, p3$ibd$extra[[2L]])

	p4 <- parseImzML(path2, ibd=TRUE,
		extra=c(TIC="MS:1000285"),
		extraArrays=c(mz="m/z array"))

	sl4 <- p4$run$spectrumList

	expect_equal(tic, sl4$extra[["TIC"]])
	expect_equal(p4$ibd$mz, p4$ibd$extra[["mz"]])

})

test_that("parseImzML - check", {

	path <- exampleImzMLFile("continuous")
	tmp <- tempfile()
	tmp_imzML <- paste0(tmp, ".imzML")
	tmp_ibd <- paste0(tmp, ".ibd")
	
	ok_imzML <- file.copy(path, tmp_imzML)
	ok_ibd <- file.create(tmp_ibd)

	expect_true(ok_imzML)
	expect_true(ok_ibd)
	
	expect_warning(parseImzML(tmp_imzML, check=TRUE))
	expect_warning(parseImzML(tmp_imzML, check=c("checksum", "uuid")))
	expect_warning(parseImzML(tmp_imzML, check="filesize"))

})

