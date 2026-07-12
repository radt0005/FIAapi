test_that("invalid grouping variable throws error", {

    expect_error(
        GB_api(
            EVAL_GRP = 12019,
            ATTRIBUTE_NBR = 10,
            GRP_BY_ATTRIB = "BANANA"
        )
    )

})

test_that("returns a data frame", {

    x <- GB_api(
        12019,
        10,
        "STATECD"
    )

    expect_s3_class(x, "data.frame")

})
