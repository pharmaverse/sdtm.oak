test_that("recode() works as intended on typical input", {
  x <- c("a", NA_character_, "α")
  # Map letters from lowercase to uppercase. NA is left as NA. Unmatched
  # values are returned as inputted.
  expect_identical(
    recode(x = x, from = letters, to = LETTERS),
    c("A", NA_character_, "α")
  )

  # The same as before but map now to integer values. Note though that the
  # type of the returned vector is nonetheless character because "α" is not
  # matched and will be preserved in the output, forcing coercion to character.
  expect_identical(
    recode(
      x = x,
      from = letters,
      to = seq_along(LETTERS)
    ),
    c("1", NA_character_, "α")
  )

  # Now that `.no_match` is of integer type, like the vector supplied in `to`,
  # the returned vector is also integer
  expect_identical(
    recode(
      x = x,
      from = letters,
      to = seq_along(LETTERS),
      .no_match = 0L
    ),
    c(1L, NA_integer_, 0L)
  )
})

test_that("recode() handling of NAs in input", {
  x <- c("aye", "nay", "maybe", NA_character_)
  from <- c("aye", "nay")
  to <- c("yes", "no")

  expect_identical(
    recode(x = x, from = from, to = to),
    c("yes", "no", "maybe", NA_character_)
  )
  expect_identical(
    recode(
      x = x,
      from = from,
      to = to,
      .na = "uh?"
    ),
    c("yes", "no", "maybe", "uh?")
  )

  # The type of the vector in the output is always the most compatible across
  # the types of `to`, `.no_match` and `.na`.
  expect_identical(
    recode(
      x = x,
      from = from,
      to = to,
      .na = NA
    ),
    c("yes", "no", "maybe", NA_character_)
  )
  expect_identical(
    recode(
      x = x,
      from = from,
      to = to,
      .na = NA_integer_
    ),
    c("yes", "no", "maybe", NA_character_)
  )
  expect_identical(
    recode(
      x = x,
      from = from,
      to = to,
      .na = NA_character_
    ),
    c("yes", "no", "maybe", NA_character_)
  )
})

test_that("recode(): recycling between `from` and `to` parameters", {
  x <- c("aye", "nay", "maybe", NA_character_)
  from <- c("aye", "nay")
  to <- "?"

  # Mapping one to many values
  expect_identical(
    recode(x = x, from = from, to = to),
    c("?", "?", "maybe", NA_character_)
  )

  # Enforce every value to become the hardcoded value specified in `to`.
  expect_identical(
    recode(
      x = x,
      from = from,
      to = to,
      .no_match = to,
      .na = to
    ),
    c("?", "?", "?", "?")
  )
})

test_that("recode(): notable cases", {
  x <- c(letters[1L:3L], NA_character_)

  # Identity: no recoding.
  expect_identical(recode(x = x), x)

  # Hardcode all values, leave NA at peace
  expect_identical(recode(x = x, to = "X"), c(rep("X", 3L), NA_character_))

  # Or, really hardcode every single value
  expect_identical(recode(
    x = x,
    to = "X",
    .no_match = "X",
    .na = "X"
  ), rep("X", 4L))
})

test_that("index_for_recode(): basic usage", {
  expect_identical(
    index_for_recode(x = 1L:5L, from = c(2L, 4L)),
    as.integer(c(NA, 1L, NA, 2L, NA))
  )
})
