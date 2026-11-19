# ---- budget routing ----------------------------------------------------
# Exact, budget-constrained routing of tsibble key series to forecasting
# models. The assignment problem is a fixed-charge generalised assignment
# problem solved to proven integer optimality by `lpSolve::lp()`; no loss
# value is ever transformed, rounded, or approximated.

#' Budget-constrained routing of tsibble series to forecasting models
#'
#' Assigns every key series of `history` to exactly one of the supplied
#' `models`, minimising the total weighted loss under an integer budget.
#' Models carry an enablement fee (charged once when a model is used at all)
#' and an allocation fee (charged for every series routed to a model). The
#' routing is solved as a binary fixed-charge assignment problem with an
#' exact integer-programming solver, so the returned assignment is the
#' global optimum on the *original* loss scores. Selected models are then
#' invoked exactly once, each receiving only the native sub-tsibbles of the
#' series assigned to it.
#'
#' @section Input tables:
#' `losses` is a data frame with one row per series-model pair and columns:
#' the history key columns, `.model` (model name), `.loss` (the loss score in
#' its original business units) and, unless `.weight_var` is given, `.weight`.
#' A pair is "available" if and only if it has a row in `losses`. Missing
#' (`NA`) loss scores make a pair unavailable.
#'
#' `enable_fee` has columns `.model` and `.fee` (non-negative integers).
#' `allocation_fee` may either mirror `enable_fee` (one fee per model) or
#' additionally contain key columns, in which case a fee applies to the
#' matching series-model pair. Models or pairs without a fee row are charged
#' zero.
#'
#' @section Failure semantics:
#' All feasibility checks (unknown models, series without any available
#' model, sub-budget minimum cost, non-integer fees or budget) raise an
#' error *before* any model function is called. If a model function errors,
#' or any model result fails key/row validation, no partial forecast is
#' returned: the call fails as a whole.
#'
#' @param history A tsibble containing the observed history.
#' @param future A tsibble (without true values) sharing the key, index and
#'   interval of `history`, containing the future index values to forecast.
#' @param losses Data frame of per-series, per-model loss scores and weights
#'   (see Input tables).
#' @param models A named list of model functions. Each function must have the
#'   signature `function(history, future)` and return a tsibble of forecasts
#'   on the same key and index as its inputs. Names must match the `.model`
#'   values in `losses`.
#' @param enable_fee Data frame of one-time model enablement fees.
#' @param allocation_fee Data frame of per-allocation fees.
#' @param budget A single non-negative integer: the inclusive total budget.
#' @param .model_var,.loss_var,.weight_var,.fee_var Column names used by the
#'   input tables. Defaults match the documented table shapes.
#'
#' @return A list with three elements:
#' * `forecast`: a tsibble holding the stacked model outputs, with the
#'   original key and index columns (and their types) preserved. It contains
#'   no routing columns.
#' * `routing`: a tibble with one row per series giving the chosen model,
#'   original loss score and weight.
#' * `budget`: a tibble summarising the objective and the enablement,
#'   allocation and total integer fees.
#'
#' @export
budget_route <- function(history, future, losses, models,
                         enable_fee = NULL, allocation_fee = NULL,
                         budget = Inf,
                         .model_var = ".model",
                         .loss_var = ".loss",
                         .weight_var = ".weight",
                         .fee_var = ".fee") {
  # ---- 1. structural validation (no models run here) -------------------
  not_tsibble(history)
  abort_if_irregular(history)
  if (!is_tsibble(future)) {
    abort("`future` must be a tsibble.")
  }
  abort_if_irregular(future)

  key_nms <- key_vars(history)
  idx_nm <- index_var(history)
  if (!identical(key_vars(future), key_nms) ||
        !identical(index_var(future), idx_nm)) {
    abort(paste(
      "`future` must share the key and index of `history`.",
      "Use the same tsibble key and index variables."))
  }

  if (!is.list(models) || is.null(names(models)) ||
        any(names(models) == "") || !all(vapply(models, is.function, logical(1)))) {
    abort("`models` must be a fully named list of functions.")
  }
  model_nms <- names(models)
  if (anyDuplicated(model_nms)) {
    abort("`models` must have unique names.")
  }

  budget <- validate_budget(budget)
  losses <- validate_losses(losses, key_nms, model_nms,
                            .model_var, .loss_var, .weight_var)
  enable_fee <- validate_fee_table(
    enable_fee, key_nms, model_nms, .model_var, .fee_var, "enable_fee",
    pair_level = FALSE)
  allocation_fee <- validate_fee_table(
    allocation_fee, key_nms, model_nms, .model_var, .fee_var,
    "allocation_fee", pair_level = TRUE)

  # ---- 2. the routing universe -----------------------------------------
  series_tbl <- dplyr::distinct(as_tibble(history), dplyr::across(all_of(key_nms)))
  series_tbl <- arrange(series_tbl, dplyr::across(all_of(key_nms)))
  n_series <- nrow(series_tbl)
  check_series_coverage(series_tbl, future, key_nms, idx_nm)

  # one canonical row per available (series, model) pair
  pairs <- inner_join(losses, series_tbl, by = key_nms)
  if (nrow(pairs) == 0) {
    abort("No loss score matches the key series in `history`.")
  }
  if (anyDuplicated(pairs[c(key_nms, .model_var)])) {
    abort("`losses` must contain at most one row per series and model.")
  }
  pairs <- arrange(pairs, dplyr::across(all_of(key_nms)), .data[[.model_var]])

  uncovered <- anti_join(series_tbl, dplyr::distinct(pairs, dplyr::across(all_of(key_nms))),
                         by = key_nms)
  if (nrow(uncovered) > 0) {
    abort(paste(
      "Every series must have at least one available model, but",
      nrow(uncovered), "series have no non-missing loss score.",
      "No model has been run."))
  }

  # fees aligned to the pair table (zero when no fee row applies)
  pairs[[".alloc_fee"]] <- lookup_pair_fee(
    pairs, allocation_fee, key_nms, .model_var, .fee_var)
  enable_lookup <- lookup_enable_fee(
    pairs, enable_fee, .model_var, .fee_var)

  # ---- 3. pre-solve feasibility and exact MILP --------------------------
  min_cost <- min_routing_cost(pairs, enable_lookup, key_nms, .model_var,
                               .alloc_fee_col = ".alloc_fee")
  if (min_cost > budget) {
    abort(paste0(
      "Budget infeasible: the cheapest feasible routing costs ", min_cost,
      " but the budget is ", budget,
      ". No model has been run."))
  }

  sol <- solve_routing_milp(
    pairs, enable_lookup, budget, key_nms, .model_var, .loss_var,
    .weight_var, .alloc_fee_col = ".alloc_fee")

  # ---- 4. materialise the assignment and recompute in R -----------------
  chosen <- pairs[as.logical(sol$x[seq_len(nrow(pairs))]), , drop = FALSE]
  chosen <- arrange(chosen, dplyr::across(all_of(key_nms)))
  if (nrow(chosen) != n_series ||
        anyDuplicated(chosen[key_nms]) ||
        nrow(anti_join(series_tbl, chosen[key_nms], by = key_nms)) > 0) {
    # defensive: the MILP constraints guarantee this cannot happen
    abort("Internal error: the solver did not return exactly one model per series.")
  }
  used_models <- unique(chosen[[.model_var]])
  y_values <- as.logical(sol$x[nrow(pairs) + seq_along(enable_lookup$model)])
  if (!setequal(enable_lookup$model[y_values], used_models)) {
    abort("Internal error: inconsistent enablement variables from the solver.")
  }

  achieved <- sum(chosen[[.loss_var]] * chosen[[.weight_var]])
  if (abs(achieved - sol$objval) > 1e-9 * max(1, abs(achieved))) {
    abort(paste0(
      "Internal error: the solver objective (", format(sol$objval, digits = 17),
      ") does not match the original-loss objective (",
      format(achieved, digits = 17), ")."))
  }

  # ---- 5. drive the selected model functions, once each ----------------
  forecast_tbl <- run_selected_models(
    chosen, used_models, models, history, future, key_nms, idx_nm,
    .model_var)

  # ---- 6. outputs -------------------------------------------------------
  routing_tbl <- chosen[c(key_nms, .model_var, .loss_var, .weight_var)]
  names(routing_tbl)[names(routing_tbl) == .model_var] <- ".model"
  names(routing_tbl)[names(routing_tbl) == .loss_var] <- ".loss"
  names(routing_tbl)[names(routing_tbl) == .weight_var] <- ".weight"
  routing_tbl <- as_tibble(routing_tbl)

  alloc_total <- sum(chosen[[".alloc_fee"]])
  enable_total <- sum(enable_lookup$fee[match(used_models,
                                              enable_lookup$model)])
  budget_tbl <- tibble(
    weighted_loss = achieved,
    enablement_fee = enable_total,
    allocation_fee = alloc_total,
    total_fee = enable_total + alloc_total,
    budget = budget,
    within_budget = (enable_total + alloc_total) <= budget)

  list(forecast = forecast_tbl,
       routing = routing_tbl,
       budget = budget_tbl)
}

# ---- validation helpers -------------------------------------------------

validate_budget <- function(budget) {
  if (length(budget) != 1L || is.na(budget) || !is.numeric(budget)) {
    abort("`budget` must be a single non-negative number.")
  }
  if (is.infinite(budget)) {
    return(.Machine$integer.max)
  }
  if (budget < 0) {
    abort("`budget` must be non-negative.")
  }
  if (!is_integerish(budget, 1)) {
    abort(paste0(
      "`budget` must be a whole number (integer budget); got ",
      format(budget, digits = 17), "."))
  }
  to_int_fee(budget, "budget")
}

# Coerce a guaranteed-whole numeric fee/budget to integer, rejecting values
# outside the 32-bit integer range instead of silently truncating them.
to_int_fee <- function(x, arg) {
  if (any(abs(x) > .Machine$integer.max)) {
    abort(paste0(
      "`", arg, "` exceeds the maximum integer fee/budget (",
      .Machine$integer.max, "). No model has been run."))
  }
  as.integer(x)
}

validate_losses <- function(losses, key_nms, model_nms,
                            .model_var, .loss_var, .weight_var) {
  if (!is.data.frame(losses)) {
    abort("`losses` must be a data frame.")
  }
  required <- c(key_nms, .model_var, .loss_var, .weight_var)
  missing_cols <- setdiff(required, names(losses))
  if (length(missing_cols) > 0) {
    abort(paste0("`losses` is missing column(s): ",
                 paste0("`", missing_cols, "`", collapse = ", "), "."))
  }
  out <- as_tibble(losses)
  if (!is.character(out[[.model_var]]) && !is.factor(out[[.model_var]])) {
    abort(paste0("`", .model_var, "` in `losses` must be character or factor."))
  }
  out[[.model_var]] <- as.character(out[[.model_var]])
  unknown <- setdiff(unique(out[[.model_var]]), model_nms)
  if (length(unknown) > 0) {
    abort(paste0(
      "`losses` references model name(s) absent from `models`: ",
      paste0("`", unknown, "`", collapse = ", "), ". No model has been run."))
  }
  if (!is.numeric(out[[.loss_var]])) {
    abort(paste0("`", .loss_var, "` must be numeric and must not be rounded."))
  }
  if (any(out[[.loss_var]] < 0, na.rm = TRUE)) {
    abort(paste0("`", .loss_var, "` scores must be non-negative."))
  }
  if (any(is.na(out[[.weight_var]])) || !is.numeric(out[[.weight_var]]) ||
        any(out[[.weight_var]] < 0)) {
    abort(paste0("`", .weight_var, "` must be non-negative and never missing."))
  }
  # NA loss means "unavailable": drop the pair entirely
  out <- out[!is.na(out[[.loss_var]]), , drop = FALSE]
  out
}

validate_fee_table <- function(fee, key_nms, model_nms,
                               .model_var, .fee_var, arg, pair_level) {
  if (is.null(fee)) {
    return(NULL)
  }
  if (!is.data.frame(fee)) {
    abort(paste0("`", arg, "` must be a data frame or NULL."))
  }
  required <- c(.model_var, .fee_var)
  missing_cols <- setdiff(required, names(fee))
  if (length(missing_cols) > 0) {
    abort(paste0("`", arg, "` is missing column(s): ",
                 paste0("`", missing_cols, "`", collapse = ", "), "."))
  }
  extra <- setdiff(names(fee), c(required, key_nms))
  if (length(extra) > 0) {
    abort(paste0("`", arg, "` has unexpected column(s): ",
                 paste0("`", extra, "`", collapse = ", "), "."))
  }
  if (!pair_level && length(intersect(names(fee), key_nms)) > 0) {
    abort(paste0("`", arg,
                 "` is a model-level fee table and must not contain key columns."))
  }
  out <- as_tibble(fee)
  out[[.model_var]] <- as.character(out[[.model_var]])
  unknown <- setdiff(unique(out[[.model_var]]), model_nms)
  if (length(unknown) > 0) {
    abort(paste0(
      "`", arg, "` references model name(s) absent from `models`: ",
      paste0("`", unknown, "`", collapse = ", "), "."))
  }
  vals <- out[[.fee_var]]
  if (length(vals) == 0L || any(is.na(vals)) || !is.numeric(vals) ||
        any(vals < 0)) {
    abort(paste0("`", arg, "` fees must be non-negative and never missing."))
  }
  if (!all(is_integerish(vals))) {
    abort(paste0(
      "`", arg,
      "` fees must be whole numbers (integer fees). No model has been run."))
  }
  out[[.fee_var]] <- to_int_fee(vals, arg)
  # In an allocation table a row may be either pair-specific (all key
  # columns populated) or a model-level fallback (all key columns NA).
  # Partially populated rows are ambiguous and rejected.
  key_cols_present <- intersect(key_nms, names(out))
  if (length(key_cols_present) > 0L) {
    key_na <- vapply(seq_len(nrow(out)), function(i) {
      any(is.na(out[i, key_cols_present, drop = FALSE]))
    }, logical(1))
    key_all_na <- vapply(seq_len(nrow(out)), function(i) {
      all(is.na(out[i, key_cols_present, drop = FALSE]))
    }, logical(1))
    if (any(key_na != key_all_na)) {
      abort(paste0(
        "`", arg,
        "` rows must be either fully key-specified (pair-level fee) or have ",
        "all key columns missing (model-level fallback fee)."))
    }
  }
  dup_cols <- c(.model_var, key_cols_present)
  if (anyDuplicated(out[dup_cols])) {
    abort(paste0("`", arg,
                 "` contains duplicate fee rows for the same model or pair."))
  }
  out
}

check_series_coverage <- function(series_tbl, future, key_nms, idx_nm) {
  fut_keys <- dplyr::distinct(as_tibble(future), dplyr::across(all_of(key_nms)))
  missing_future <- anti_join(series_tbl, fut_keys, by = key_nms)
  if (nrow(missing_future) > 0) {
    abort(paste(nrow(missing_future),
                "history series have no future rows in `future`."))
  }
  extra_future <- anti_join(fut_keys, series_tbl, by = key_nms)
  if (nrow(extra_future) > 0) {
    abort(paste("`future` contains key series absent from `history`."))
  }
}

# ---- fee lookups --------------------------------------------------------

lookup_pair_fee <- function(pairs, allocation_fee, key_nms,
                            .model_var, .fee_var) {
  if (is.null(allocation_fee)) {
    return(rep(0L, nrow(pairs)))
  }
  fee_tbl <- allocation_fee
  key_cols_present <- intersect(key_nms, names(fee_tbl))
  # rows carrying key columns define pair-specific fees; the remaining rows
  # define a model-level fallback. Both forms may coexist in one table.
  is_pair_row <- if (length(key_cols_present) == 0L) {
    rep(FALSE, nrow(fee_tbl))
  } else {
    !vapply(seq_len(nrow(fee_tbl)), function(i) {
      any(is.na(fee_tbl[i, key_cols_present, drop = FALSE]))
    }, logical(1))
  }

  fees <- rep(0L, nrow(pairs))
  model_rows <- fee_tbl[!is_pair_row, , drop = FALSE]
  if (nrow(model_rows) > 0) {
    idx <- match(pairs[[.model_var]], model_rows[[.model_var]])
    vals <- model_rows[[.fee_var]][idx]
    fees <- as.integer(ifelse(is.na(vals), 0L, vals))
  }

  pair_rows <- fee_tbl[is_pair_row, , drop = FALSE]
  if (nrow(pair_rows) > 0) {
    joined <- left_join(
      pairs[c(key_nms, .model_var)],
      pair_rows[c(key_nms, .model_var, .fee_var)],
      by = c(key_nms, .model_var))
    override <- joined[[.fee_var]]
    fees <- as.integer(ifelse(is.na(override), fees, override))
  }
  fees
}

lookup_enable_fee <- function(pairs, enable_fee, .model_var, .fee_var) {
  used <- sort(unique(pairs[[.model_var]]))
  fees <- rep(0L, length(used))
  if (!is.null(enable_fee)) {
    idx <- match(used, enable_fee[[.model_var]])
    vals <- enable_fee[[.fee_var]][idx]
    fees <- as.integer(ifelse(is.na(vals), 0L, vals))
  }
  tibble(model = used, fee = fees)
}

# ---- exact optimisation -------------------------------------------------

min_routing_cost <- function(pairs, enable_lookup, key_nms,
                             .model_var, .alloc_fee_col) {
  # Cheapest feasible routing ignoring the budget: per series pick the pair
  # with the smallest allocation fee, then add the enablement fee of every
  # model used by that routing. This is a strict lower bound used only for
  # the explicit pre-solve failure message.
  best <- pairs[order(pairs[[.alloc_fee_col]]), , drop = FALSE]
  best <- best[!duplicated(best[key_nms]), , drop = FALSE]
  alloc <- sum(best[[.alloc_fee_col]])
  used <- unique(best[[.model_var]])
  enable <- sum(enable_lookup$fee[match(used, enable_lookup$model)])
  alloc + enable
}

solve_routing_milp <- function(pairs, enable_lookup, budget, key_nms,
                               .model_var, .loss_var, .weight_var,
                               .alloc_fee_col) {
  n_pairs <- nrow(pairs)
  n_models <- nrow(enable_lookup)
  n_var <- n_pairs + n_models

  # objective: sum(weight * loss * x); enablement carries no loss
  objective <- c(pairs[[.weight_var]] * pairs[[.loss_var]],
                 rep(0, n_models))

  # series ids via vctrs multi-column matching: safe for any key values,
  # including ones containing separator characters
  series_keys <- dplyr::distinct(pairs[key_nms], dplyr::across(all_of(key_nms)))
  series_id <- vec_match(pairs[key_nms], series_keys)
  model_id <- match(pairs[[.model_var]], enable_lookup$model)

  rows <- list()
  dirs <- character()
  rhs <- integer()

  # (a) exactly one model per series
  for (s in seq_len(max(series_id))) {
    row <- integer(n_var)
    row[which(series_id == s)] <- 1L
    rows[[length(rows) + 1L]] <- row
    dirs <- c(dirs, "=")
    rhs <- c(rhs, 1L)
  }

  # (b) x_{sm} <= y_m: sum over series of each model minus big-M*y <= 0
  for (m in seq_len(n_models)) {
    row <- integer(n_var)
    row[which(model_id == m)] <- 1L
    row[n_pairs + m] <- -sum(model_id == m)
    rows[[length(rows) + 1L]] <- row
    dirs <- c(dirs, "<=")
    rhs <- c(rhs, 0L)
  }

  # (c) budget: allocation fees on x, enablement fees on y
  budget_row <- integer(n_var)
  budget_row[seq_len(n_pairs)] <- pairs[[.alloc_fee_col]]
  budget_row[n_pairs + seq_len(n_models)] <- enable_lookup$fee
  rows[[length(rows) + 1L]] <- budget_row
  dirs <- c(dirs, "<=")
  rhs <- c(rhs, as.integer(budget))

  const_mat <- do.call(rbind, rows)
  storage.mode(const_mat) <- "double"

  res <- lpSolve::lp(
    direction = "min",
    objective.in = objective,
    const.mat = const_mat,
    const.dir = dirs,
    const.rhs = as.double(rhs),
    all.bin = TRUE)

  if (res$status == 2L) {
    abort(paste0(
      "Budget infeasible: no assignment of one model per series fits the ",
      "budget of ", budget, ". No model has been run."))
  }
  if (res$status != 0L) {
    abort(paste0(
      "The integer solver failed with status ", res$status,
      "; no routing could be certified and no model has been run."))
  }
  if (length(res$solution) != n_var) {
    abort("The integer solver returned a malformed solution; no model has been run.")
  }
  list(x = round(res$solution), objval = res$objval)
}

# ---- model execution and result validation ------------------------------

run_selected_models <- function(chosen, used_models, models,
                                history, future, key_nms, idx_nm,
                                .model_var) {
  outputs <- vector("list", length(used_models))
  names(outputs) <- used_models
  for (m in used_models) {
    assigned_keys <- chosen[chosen[[.model_var]] == m, key_nms, drop = FALSE]
    hist_sub <- dplyr::semi_join(history, assigned_keys, by = key_nms)
    fut_sub <- dplyr::semi_join(future, assigned_keys, by = key_nms)

    out <- tryCatch(
      models[[m]](hist_sub, fut_sub),
      error = function(e) {
        abort(paste0(
          "Model `", m, "` failed while forecasting its ",
          nrow(assigned_keys), " assigned series: ", conditionMessage(e),
          ". No partial forecast is returned."),
          parent = e)
      })
    validate_model_output(out, m, assigned_keys, hist_sub, fut_sub,
                          key_nms, idx_nm)
    outputs[[m]] <- out
  }

  # identical measure schema across models is required for a clean tsibble
  measure_sets <- lapply(outputs, measured_vars)
  if (length(unique(measure_sets)) != 1L) {
    abort(paste0(
      "Models returned different forecast columns: ",
      paste0(names(outputs), " (",
             vapply(measure_sets, paste, character(1), collapse = ", "),
             ")", collapse = "; "),
      ". No partial forecast is returned."))
  }

  stacked <- vec_rbind(!!!outputs)
  stacked <- dplyr_reconstruct_tsibble(stacked, history, key_nms, idx_nm)
  arrange(stacked, dplyr::across(all_of(key_nms)), .data[[idx_nm]])
}

validate_model_output <- function(out, model, assigned_keys,
                                  hist_sub, fut_sub, key_nms, idx_nm) {
  if (!is_tsibble(out)) {
    abort(paste0("Model `", model,
                 "` did not return a tsibble. No partial forecast is returned."))
  }
  if (!identical(key_vars(out), key_nms) || !identical(index_var(out), idx_nm)) {
    abort(paste0(
      "Model `", model,
      "` returned a tsibble with different key/index variables than the input. ",
      "No partial forecast is returned."))
  }
  if (length(measured_vars(out)) == 0L) {
    abort(paste0(
      "Model `", model,
      "` returned no forecast measure column. No partial forecast is returned."))
  }
  out_keys <- dplyr::distinct(as_tibble(out), dplyr::across(all_of(key_nms)))
  if (nrow(out_keys) != nrow(assigned_keys) ||
        nrow(anti_join(out_keys, assigned_keys, by = key_nms)) > 0 ||
        nrow(anti_join(assigned_keys, out_keys, by = key_nms)) > 0) {
    abort(paste0(
      "Model `", model,
      "` returned keys that do not match exactly its assigned series ",
      "(missing or duplicated keys). No partial forecast is returned."))
  }
  expected_grid <- as_tibble(fut_sub)[c(key_nms, idx_nm)]
  returned_grid <- as_tibble(out)[c(key_nms, idx_nm)]
  if (nrow(returned_grid) != nrow(expected_grid) ||
        nrow(anti_join(expected_grid, returned_grid,
                       by = c(key_nms, idx_nm))) > 0 ||
        nrow(anti_join(returned_grid, expected_grid,
                       by = c(key_nms, idx_nm))) > 0) {
    abort(paste0(
      "Model `", model,
      "` returned key/index rows that do not match exactly the future rows ",
      "of its assigned series. No partial forecast is returned."))
  }
  if (!identical(class(out[[idx_nm]]), class(hist_sub[[idx_nm]]))) {
    abort(paste0(
      "Model `", model,
      "` changed the index type. No partial forecast is returned."))
  }
  invisible(TRUE)
}

dplyr_reconstruct_tsibble <- function(stacked, template, key_nms, idx_nm) {
  # vec_rbind of same-schema tsibbles already keeps the tsibble structure;
  # rebuild defensively so the output key/index are guaranteed identical.
  if (is_tsibble(stacked) &&
        identical(key_vars(stacked), key_nms) &&
        identical(index_var(stacked), idx_nm)) {
    return(stacked)
  }
  as_tsibble(as_tibble(stacked), key = all_of(key_nms), index = all_of(idx_nm))
}
