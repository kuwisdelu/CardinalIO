require(testthat)
require(CardinalIO)

context("Analyze 7.5")

test_that("Analyze 7.5 - write/parse", {

	set.seed(2023)
	nx <- 3
	ny <- 3
	nmz <- 500
	mz <- seq(500, 510, length.out=nmz)
	intensity <- replicate(nx * ny, rlnorm(nmz))
	dim(intensity) <- c(nmz, nx, ny)
	path <- tempfile(fileext=".hdr")

	success <- writeAnalyze(intensity, path, type="float32", mz=mz)

	expect_true(success)
	expect_equal(file.size(path), 348L)

	p <- parseAnalyze(path)

	mz2 <- as.numeric(p$mz)
	intensity2 <- as.array(p$img)

	expect_equal(mz, mz2, tolerance=1e-5)
	expect_equal(intensity, intensity2, tolerance=1e-5)
	expect_equal(nrow(intensity), nrow(intensity2))
	
})
