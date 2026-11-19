# ---- shared fixtures and mock model -------------------------------------

n_per_series <- function() 6L
n_horizon <- function() 2L
n_distinct_series <- function(x = NULL) {
  if (is.null(x)) 3L else nrow(dplyr::distinct(as_tibble(x), dplyr::across(all_of(key_vars(x)))))
}

tourism_like <- function() {
  region <- paste0("r", 1:3)
  base <- tidyr::expand_grid(region = region,
                             date = as.Date("2020-01-01") + 0:(n_per_series() - 1))
  base$trips <- seq_len(nrow(base))
  as_tsibble(base, key = region, index = date)
}

tourism_like_future <- function(hist) {
  last <- max(hist$date)
  grid <- tidyr::expand_grid(region = unique(hist$region),
                             date = last + seq_len(n_horizon()))
  as_tsibble(grid, key = region, index = date)
}

make_composite_tsibble <- function() {
  grid <- tidyr::expand_grid(g1 = c("a", "b"), g2 = c(1L, 2L),
                             ts = 1:4)
  grid$ts <- as.POSIXct("2020-01-01", tz = "UTC") +
    (grid$ts - 1) * 3600
  grid$y <- seq_len(nrow(grid))
  hist <- as_tsibble(grid, key = c(g1, g2), index = ts)
  fgrid <- tidyr::expand_grid(g1 = c("a", "b"), g2 = c(1L, 2L),
                              ts = max(grid$ts) + c(3600, 7200))
  fut <- as_tsibble(fgrid, key = c(g1, g2), index = ts)
  list(history = hist, future = fut)
}

# losses: named list of per-series loss vectors, in history key order
# losses: named list of per-series loss vectors, aligned positionally to the
# arranged key table (fixtures use sortable keys; pass an explicit losses
# table for keys with non-obvious ordering)
expand_losses <- function(hist, model_names, loss_list) {
  key <- key_vars(hist)
  keys <- dplyr::arrange(dplyr::distinct(as_tibble(hist), dplyr::across(all_of(key))),
                         dplyr::across(all_of(key)))
  if (!all(vapply(loss_list, length, integer(1)) == nrow(keys))) {
    stop("each loss vector must have one value per arranged key")
  }
  out <- list()
  for (m in model_names) {
    tbl <- keys
    tbl[[".model"]] <- m
    tbl[[".loss"]] <- loss_list[[m]]
    tbl[[".weight"]] <- 1
    out[[length(out) + 1L]] <- tbl
  }
  bind_rows(out)
}

# mock model: records every (history, future) pair it receives, returns a
# forecast tsibble on the exact future grid with native key/index types
recording_naive_model <- function(calls, name) {
  force(name)
  function(history, future) {
    calls$log[[name]] <- c(calls$log[[name]],
                           list(list(history = history, future = future)))
    out <- as_tibble(future)
    out$.forecast <- out[[index_var(future)]] # deterministic, type-agnostic
    out$.pi <- 1
    as_tsibble(out, key = key_vars(future), index = index_var(future))
  }
}

test_that("budget_route finds the global weighted-loss optimum under budget", {
  hist <- tourism_like()
  fut <- tourism_like_future(hist)
  # 3 series x 2 models; weights 1
  losses <- expand_losses(hist, c("m_cheap", "m_dear"),
                          list(m_cheap = c(10, 20, 30),
                               m_dear  = c(1, 2, 3)))
  losses[[".weight"]] <- 1
  # allocation fee makes "all dear" cost 18, "all cheap" cost 3
  alloc <- tibble(.model = c("m_cheap", "m_dear"), .fee = c(1L, 6L))
  calls <- new.env(parent = emptyenv())
  models <- list(
    m_cheap = recording_naive_model(calls, "m_cheap"),
    m_dear  = recording_naive_model(calls, "m_dear"))

  res <- budget_route(hist, fut, losses, models,
                      allocation_fee = alloc, budget = 100L)

  # unconstrained optimum: all series take m_dear (weighted loss 6)
  expect_setequal(res$routing$.model, "m_dear")
  expect_equal(res$budget$weighted_loss, 6)
  expect_equal(res$budget$total_fee, 18L)
  expect_true(res$budget$within_budget)

  # budget 10 excludes "all dear" (18); optimum is a mixed routing:
  # one m_dear allocation (6) + two m_cheap allocations (2), fee 8,
  # loss = 1 (dear, s1) + 20 (cheap, s2) + 12 (cheap, s3) = 33
  res2 <- budget_route(hist, fut, losses, models,
                       allocation_fee = alloc, budget = 10L)
  expect_setequal(unique(res2$routing$.model), c("m_cheap", "m_dear"))
  expect_equal(sum(res2$routing$.model == "m_dear"), 1L)
  expect_equal(res2$budget$weighted_loss, 33)
  expect_equal(res2$budget$total_fee, 8L)
  expect_true(res2$budget$within_budget)

  # minimum possible fee is 3; budget 2 is infeasible before any run
  expect_error(
    budget_route(hist, fut, losses, models,
                 allocation_fee = alloc, budget = 2L),
    "infeasible")

  # budget 3 excludes any m_dear allocation: all m_cheap, fee 3, loss 60
  res3 <- budget_route(hist, fut, losses, models,
                       allocation_fee = alloc, budget = 3L)
  expect_setequal(res3$routing$.model, "m_cheap")
  expect_equal(res3$budget$weighted_loss, 60)
  expect_equal(res3$budget$total_fee, 3L)
})

test_that("enablement fee is charged once per used model and can reroute", {
  hist <- tourism_like()
  fut <- tourism_like_future(hist)
  losses <- expand_losses(hist, c("a", "b"),
                          list(a = c(5, 5, 5), b = c(4, 4, 4)))
  losses[[".weight"]] <- 1
  enable <- tibble(.model = c("a", "b"), .fee = c(0L, 10L))

  res <- budget_route(hist, fut, losses,
                      list(a = recording_naive_model(new.env(), "a"),
                           b = recording_naive_model(new.env(), "b")),
                      enable_fee = enable, budget = 9L)
  expect_setequal(res$routing$.model, "a")
  expect_equal(res$budget$enablement_fee, 0L)
  expect_equal(res$budget$weighted_loss, 15)

  res2 <- budget_route(hist, fut, losses,
                       list(a = recording_naive_model(new.env(), "a"),
                            b = recording_naive_model(new.env(), "b")),
                       enable_fee = enable, budget = 100L)
  expect_setequal(res2$routing$.model, "b")
  expect_equal(res2$budget$enablement_fee, 10L)
})

test_that("each used model is called exactly once with its assigned sub-tsibbles", {
  hist <- tourism_like()
  fut <- tourism_like_future(hist)
  losses <- expand_losses(hist, c("a", "b"),
                          list(a = c(1, 100, 100), b = c(100, 1, 1)))
  losses[[".weight"]] <- 1
  calls <- new.env(parent = emptyenv())
  calls$log <- list()
  models <- list(a = recording_naive_model(calls, "a"),
                 b = recording_naive_model(calls, "b"))

  res <- budget_route(hist, fut, losses, models, budget = 1000L)

  expect_setequal(names(calls$log), c("a", "b"))
  # exactly one invocation per used model
  expect_equal(length(calls$log$a), 1L)
  expect_equal(length(calls$log$b), 1L)
  # model a receives only series 1, model b series 2 and 3: native sub-tsibbles
  expect_equal(nrow(calls$log$a[[1]]$history), n_per_series())
  expect_equal(key_vars(calls$log$a[[1]]$history), key_vars(hist))
  expect_s3_class(calls$log$a[[1]]$history, "tbl_ts")
  expect_equal(nrow(calls$log$b[[1]]$future), n_horizon() * 2L)
  # unselected models never run: build a case where one model is unused
  losses2 <- losses
  losses2[[".loss"]][losses2[[".model"]] == "b"] <- 999
  calls2 <- new.env(parent = emptyenv()); calls2$log <- list()
  models2 <- list(a = recording_naive_model(calls2, "a"),
                  b = recording_naive_model(calls2, "b"))
  budget_route(hist, fut, losses2, models2, budget = 1000L)
  expect_true(is.null(calls2$log$b))
  expect_equal(length(calls2$log$a), 1L)
})

test_that("forecast keys are complete, unique and key/index types preserved", {
  hist <- tourism_like()
  fut <- tourism_like_future(hist)
  losses <- expand_losses(hist, c("a", "b"),
                          list(a = c(1, 2, 3), b = c(3, 2, 1)))
  losses[[".weight"]] <- 1
  res <- budget_route(hist, fut, losses,
                      list(a = recording_naive_model(new.env(), "a"),
                           b = recording_naive_model(new.env(), "b")),
                      budget = 1000L)
  fc <- res$forecast
  expect_s3_class(fc, "tbl_ts")
  expect_equal(key_vars(fc), key_vars(hist))
  expect_equal(index_var(fc), index_var(hist))
  expect_s3_class(fc[[index_var(fc)]], class(hist[[index_var(hist)]])[1])
  # exactly the future grid, no missing/duplicated rows
  expect_equal(nrow(fc), nrow(fut))
  grid <- as_tibble(fc)[c(key_vars(fc), index_var(fc))]
  expect_equal(nrow(dplyr::distinct(grid)), nrow(grid))
  expect_equal(
    dplyr::arrange(grid, dplyr::across(everything())),
    dplyr::arrange(as_tibble(fut)[names(grid)], dplyr::across(everything())))
  # routing is a separate table with one row per series and original scores
  expect_equal(nrow(res$routing), n_distinct_series())
  expect_false(".model" %in% names(fc))
  expect_type(res$routing$.loss, "double")
})

test_that("original loss precision is retained (no rounding of loss values)", {
  hist <- tourism_like()
  fut <- tourism_like_future(hist)
  awkward <- c(1/3, sqrt(2), .Machine$double.eps * 10)
  losses <- expand_losses(hist, c("a", "b"),
                          list(a = awkward, b = awkward + 1e-6))
  losses[[".weight"]] <- c(rep(2, 3), rep(1, 3))[seq_len(nrow(losses))]
  res <- budget_route(hist, fut, losses,
                      list(a = recording_naive_model(new.env(), "a"),
                           b = recording_naive_model(new.env(), "b")),
                      budget = 1000L)
  # routing loss values are bit-identical to the input values
  merged <- dplyr::inner_join(res$routing, losses,
                       by = c(key_vars(hist), ".model" = ".model"),
                       suffix = c(".route", ".input"))
  expect_equal(merged$.loss.route, merged$.loss.input)
  expect_true(all(!is.na(merged$.loss.route)))
  # objective recomputed from original values, not a solver-rounded copy
  expected <- sum(res$routing$.loss * res$routing$.weight)
  expect_equal(res$budget$weighted_loss, expected)
})

test_that("infeasibility fails before any model is run", {
  hist <- tourism_like()
  fut <- tourism_like_future(hist)
  losses <- expand_losses(hist, c("a", "b"),
                          list(a = c(1, 1, 1), b = c(2, 2, 2)))
  losses[[".weight"]] <- 1
  calls <- new.env(parent = emptyenv()); calls$log <- list()
  models <- list(a = recording_naive_model(calls, "a"),
                 b = recording_naive_model(calls, "b"))
  alloc <- tibble(.model = c("a", "b"), .fee = c(10L, 10L))

  expect_error(
    budget_route(hist, fut, losses, models, allocation_fee = alloc, budget = 5L),
    "infeasible")
  expect_length(calls$log, 0)

  # a series with no available pair fails before solving/running:
  # remove every pair of the first series (one row per model)
  key <- key_vars(hist)
  first_key <- dplyr::arrange(dplyr::distinct(as_tibble(hist), dplyr::across(all_of(key))),
                              dplyr::across(all_of(key)))[1, , drop = FALSE]
  drop_rows <- dplyr::inner_join(
    tibble::rowid_to_column(losses, ".rid"), first_key, by = key)$.rid
  losses_na <- losses
  losses_na[[".loss"]][drop_rows] <- NA_real_
  expect_error(
    budget_route(hist, fut, losses_na, models, budget = 1000L),
    "no non-missing loss score|available model")
  expect_length(calls$log, 0)
})

test_that("pair-level and mixed allocation fees are charged exactly", {
  hist <- tourism_like()
  fut <- tourism_like_future(hist)
  losses <- expand_losses(hist, c("a", "b"),
                          list(a = c(1, 1, 1), b = c(2, 2, 2)))
  losses[[".weight"]] <- 1
  key <- key_vars(hist)
  keys <- dplyr::arrange(dplyr::distinct(as_tibble(hist), dplyr::across(all_of(key))),
                         dplyr::across(all_of(key)))

  # pair-specific fee for the first series on model a, model-level fallback
  # fees for everything else. b's fallback fee (10) dominates its small loss
  # advantage, so the unconstrained optimum stays on a.
  mixed <- bind_rows(
    mutate(keys[1, , drop = FALSE], .model = "a", .fee = 9L),
    tibble(.model = c("a", "b"), .fee = c(1L, 10L)))
  res <- budget_route(hist, fut, losses,
                      list(a = recording_naive_model(new.env(), "a"),
                           b = recording_naive_model(new.env(), "b")),
                      allocation_fee = mixed, budget = 100L)
  expect_setequal(res$routing$.model, "a")
  # pair fee 9 on (r1, a), fallback fee 1 for the other two series
  expect_equal(res$budget$allocation_fee, 11L)

  # force a: the pair fee applies only to the first series, fallback to the rest
  losses2 <- losses
  losses2[[".loss"]][losses2[[".model"]] == "b"] <- 100
  res2 <- budget_route(hist, fut, losses2,
                       list(a = recording_naive_model(new.env(), "a"),
                            b = recording_naive_model(new.env(), "b")),
                       allocation_fee = mixed, budget = 100L)
  expect_setequal(res2$routing$.model, "a")
  expect_equal(res2$budget$allocation_fee, 11L)

  # duplicate fee rows are rejected before any model runs
  dup <- bind_rows(mixed, mixed[1, ])
  expect_error(
    budget_route(hist, fut, losses,
                 list(a = recording_naive_model(new.env(), "a"),
                      b = recording_naive_model(new.env(), "b")),
                 allocation_fee = dup, budget = 100L),
    "duplicate fee rows")
})

test_that("non-integer budget or fees are rejected up front", {
  hist <- tourism_like()
  fut <- tourism_like_future(hist)
  losses <- expand_losses(hist, "a", list(a = c(1, 1, 1)))
  losses[[".weight"]] <- 1
  models <- list(a = recording_naive_model(new.env(), "a"))
  expect_error(budget_route(hist, fut, losses, models, budget = 4.5),
               "whole number")
  expect_error(
    budget_route(hist, fut, losses, models, budget = 10L,
                 allocation_fee = tibble(.model = "a", .fee = 1.5)),
    "whole number")
  expect_error(
    budget_route(hist, fut, losses, models, budget = -1L),
    "non-negative")
  # 32-bit integer overflow is rejected rather than truncated
  expect_error(
    budget_route(hist, fut, losses, models, budget = 2^31),
    "exceeds the maximum integer")
  expect_error(
    budget_route(hist, fut, losses, models, budget = 10L,
                 enable_fee = tibble(.model = "a", .fee = 2^31)),
    "exceeds the maximum integer")
  # the default budget is unlimited and selects the unconstrained optimum
  res <- budget_route(hist, fut, losses, models)
  expect_equal(res$budget$budget, .Machine$integer.max)
  expect_true(res$budget$within_budget)
  expect_setequal(res$routing$.model, "a")
})

test_that("unknown model references and malformed inputs are rejected", {
  hist <- tourism_like()
  fut <- tourism_like_future(hist)
  losses <- expand_losses(hist, c("a", "ghost"),
                          list(a = c(1, 1, 1), ghost = c(2, 2, 2)))
  losses[[".weight"]] <- 1
  models <- list(a = recording_naive_model(new.env(), "a"))
  expect_error(budget_route(hist, fut, losses, models, budget = 100L),
               "absent from `models`")
  bad <- losses[setdiff(names(losses), ".weight")]
  expect_error(budget_route(hist, fut, bad, models, budget = 100L),
               "missing column")
  expect_error(
    budget_route(hist, fut, losses, list(recording_naive_model(new.env(), "a")),
                 budget = 100L),
    "named list")
})

test_that("model error aborts the whole call with no partial forecast", {
  hist <- tourism_like()
  fut <- tourism_like_future(hist)
  losses <- expand_losses(hist, c("a", "b"),
                          list(a = c(1, 100, 100), b = c(100, 1, 1)))
  losses[[".weight"]] <- 1
  boom <- function(history, future) abort("kaboom")
  models <- list(a = recording_naive_model(new.env(), "a"), b = boom)
  expect_error(budget_route(hist, fut, losses, models, budget = 1000L),
               "Model `b` failed.*kaboom")
})

test_that("result with wrong keys or wrong future grid is rejected", {
  hist <- tourism_like()
  fut <- tourism_like_future(hist)
  # losses force both models to be selected (series 1 -> a, rest -> b)
  losses <- expand_losses(hist, c("a", "b"),
                          list(a = c(1, 100, 100), b = c(100, 1, 1)))
  losses[[".weight"]] <- 1

  leaky <- function(history, future) {
    # ignores its assignment: forecasts every series in the package data
    recording_naive_model(new.env(), "leak")(
      tourism_like(), tourism_like_future(tourism_like()))
  }
  expect_error(
    budget_route(hist, fut, losses,
                 list(a = leaky, b = recording_naive_model(new.env(), "b")),
                 budget = 1000L),
    "do not match exactly")

  short <- function(history, future) {
    out <- recording_naive_model(new.env(), "short")(history, future)
    out[-1, ]
  }
  expect_error(
    budget_route(hist, fut, losses,
                 list(a = short, b = recording_naive_model(new.env(), "b")),
                 budget = 1000L),
    "do not match exactly")
})

test_that("composite keys and POSIXct indexes are preserved end to end", {
  dat <- make_composite_tsibble()
  hist <- dat$history
  fut <- dat$future
  key <- key_vars(hist)
  losses <- expand_losses(hist, c("a", "b"),
                          list(a = seq_len(n_distinct_series(hist)),
                               b = rep(99, n_distinct_series(hist))))
  losses[[".weight"]] <- 1
  res <- budget_route(hist, fut, losses,
                      list(a = recording_naive_model(new.env(), "a"),
                           b = recording_naive_model(new.env(), "b")),
                      budget = 1000L)
  expect_equal(key_vars(res$forecast), key)
  expect_type(res$forecast[[key[1]]], "character")
  expect_type(res$forecast[[key[2]]], "integer")
  expect_s3_class(res$forecast[[index_var(res$forecast)]], "POSIXct")
  expect_equal(nrow(res$forecast), nrow(fut))
})

test_that("key values containing separator characters route without collision", {
  # two keys whose pasted forms could collide under a single-character join
  grid <- tidyr::expand_grid(
    sid = c("a\rb", "a", "b"),
    date = as.Date("2020-01-01") + 0:3)
  hist <- as_tsibble(dplyr::mutate(grid, y = seq_len(nrow(grid))),
                     key = sid, index = date)
  fgrid <- tidyr::expand_grid(sid = c("a\rb", "a", "b"),
                              date = as.Date("2020-01-04") + 0:1)
  fut <- as_tsibble(fgrid, key = sid, index = date)
  # explicit, key-aligned loss table: odd key -> a, ordinary keys -> b
  key_tbl <- dplyr::distinct(as_tibble(hist), sid)
  loss_for <- function(m) {
    tbl <- key_tbl
    tbl[[".model"]] <- m
    tbl[[".loss"]] <- ifelse(tbl$sid == "a\rb",
                             if (m == "a") 1 else 100,
                             if (m == "b") 1 else 100)
    tbl[[".weight"]] <- 1
    tbl
  }
  losses <- bind_rows(loss_for("a"), loss_for("b"))
  res <- budget_route(hist, fut, losses,
                      list(a = recording_naive_model(new.env(), "a"),
                           b = recording_naive_model(new.env(), "b")),
                      budget = 1000L)
  expect_equal(nrow(res$routing), 3L)
  expect_setequal(res$routing$sid, c("a\rb", "a", "b"))
  # the odd key must be independent of the two ordinary keys
  odd <- res$routing[res$routing$sid == "a\rb", ".model"][[1]]
  expect_equal(odd, "a")
  expect_setequal(res$routing$.model[res$routing$sid %in% c("a", "b")], "b")
  expect_equal(nrow(res$forecast), nrow(fut))
})

