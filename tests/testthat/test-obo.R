require(testthat)
require(CardinalIO)

context("obo")

test_that("obo", {

	ims <- get_obo("ims")
	ms <- get_obo("ms")
	uo <- get_obo("uo")

	expect_is(ims, "ontology_index")
	expect_is(ms, "ontology_index")
	expect_is(uo, "ontology_index")

	pos_terms <- unname(grep("position", ims$name, value=TRUE))
	ibd_terms <- unname(grep("ibd", ims$name, value=TRUE))

	expect_setequal(find_terms("position", "ims"), pos_terms)
	expect_setequal(find_terms("ibd", "ims"), ibd_terms)

	expect_true(all(valid_terms(find_terms("position", "ims"))))
	expect_true(all(valid_terms(find_terms("ibd", "ims"))))

	expect_equivalent(
		find_term("position x", "ims"),
		ims$name["IMS:1000050"])
	expect_equivalent(
		find_term("position y", "ims"),
		ims$name["IMS:1000051"])
	expect_equivalent(
		find_term("position z", "ims"),
		ims$name["IMS:1000052"])
	expect_equivalent(
		find_term("external offset", "ims"),
		ims$name["IMS:1000102"])
	expect_equivalent(
		find_term("external offset", "ims"),
		ims$name["IMS:1000102"])
	expect_equivalent(
		find_term("external array length", "ims"),
		ims$name["IMS:1000103"])
	expect_equivalent(
		find_term("external encoded length", "ims"),
		ims$name["IMS:1000104"])

	expect_error(find_term("position"))

	terms <- list(
		"MS:1000128"="profile spectrum",
		"MS:1000127"="centroid spectrum")

	expect_equal(terms, find_descendants_in(terms, "MS:1000525", "ms"))

})

