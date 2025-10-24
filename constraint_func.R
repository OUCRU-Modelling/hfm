library(tidyverse)
library(scam)
library(mgcv)
library(serosv)
library(assertthat)

# ------ Demo function --------
#' Age-time varying seroprevalence
#' Fit age-stratified seroprevalence across multiple time points. Also try to monotonize age (or cohort) - specific seroprevalence.
#'
#' @param data - input data, must have`age`, `status`, time, group columns, where group column determines how data is aggregated
#' @param time_col - name of the column for time (default to `date`)
#' @param grouping_col - name of the column for time (default to `group`)
#' @param age_correct - a boolean, if `TRUE`, monotonize age-specific prevalence. Monotonize birth cohort-specific seroprevalence otherwise.
#' @param le - number of bins to generate age grid, used when monotonizing data
#' @param ci - confidence interval for smoothing
#'
#' @return a list of class time_age_model with 3 items
#'   \item{out}{a data.frame with dimension n_group x 9, where columns `info`, `sp`, `foi` store output for non-monotonized
#' data and `monotonized_info`, `monotonized_sp`,  `monotonized_foi`,  `monotonized_ci_mod` store output for monotnized data}
#'   \item{grouping_col}{name of the column for grouping}
#'   \item{age_correct}{a boolean indicating whether the data is monotonized across age or cohort}
#' @export
#'
#' @examples
time_age_model <- function(data, time_col="date", grouping_col="group", age_correct=F, le=512, ci = 0.95){

  # ---- helper functions -----
  shift_right <- \(n,x){ if(n == 1) x else dplyr::lag(x, n, default = NA)}
  # function to simulate data for monotonize process
  generate_data <- \(dat, mod, no_sim=100) {
    link_inv <- family(mod)$linkinv
    n <- nrow(dat) - length(coef(mod))
    p <- (1 - ci)/2

    pred <- predict(mod, dat, se.fit = TRUE)  %>%
      as_tibble()  %>%
      select(fit, se.fit) %>%
      mutate(
        ymin = link_inv(fit + qt(p, n) * se.fit),
        ymax = link_inv(fit + qt(1 - p, n) * se.fit),
        y = link_inv(fit)
      )  %>%
      select(-se.fit,-fit)

    dat %>%
      bind_cols(pred) %>%
      pivot_longer(c(ymin, ymax, y),
                   names_to = "ys",
                   values_to = "prevalence")
  }
  # function to monotonize data using serosv pava function
  monotonize_data <- \(dat, grp){
    dat %>%
      arrange(mean_time) %>%
      mutate(
        prevalence = serosv::pava(prevalence)$pai2
      )
  }

  # --- preprocess data ------
  age_range <- range(data$age)
  age_grid <- seq(age_range[1], age_range[2], length.out = le)

  # ---- gam model for age-stratified prevalence for each group -----
  gam_mods <- data %>%
    group_by(.data[[grouping_col]]) %>% nest() %>%
    mutate(
      mod = map(data, \(dat){
        gam(status ~ s(age), data = dat, family = binomial)
      }),
      mean_time = map_dbl(data, \(dat){mean(dat[[time_col]])}) %>% as.Date()
    ) %>%
    ungroup()

  # ----- branching based on age_correct ---
  # if age_correct is TRUE: enforce monotonic increase in prevalence overtime within age group
  # otherwise: enforce monotonic increase in prevalence within cohort
  if(age_correct == FALSE){
    # simulate data + monotonize data using scam
    scam_out <- gam_mods %>%
      select(!!sym(grouping_col), mod, mean_time) %>%
      mutate(
        # simulate data to fit scam model
        sim_data = map(mod, \(mod){
          data.frame(age = age_grid) %>% generate_data(mod)
        })
      ) %>%
      select(-mod) %>% unnest(sim_data) %>%
      group_by(age, ys) %>%
      # arrange(mean_time) %>% #############
      group_modify(monotonize_data) %>% ungroup()

    # modify monotonized data
    scam_out <- scam_out %>%
      pivot_wider(names_from = ys, values_from = prevalence) %>%
      group_by(!!sym(grouping_col), mean_time)  %>%
      nest()
  }else{
    dpy <- 365

    # simulate data to monotonize
    # return a data.frame of collection_time, age (at current collection time), cohort (age at first collection time)
    scam_data <- gam_mods %>%
      mutate(
        age = map(mean_time, \(.) {
          age_grid
        }),
        shift_no = (mean_time - min(mean_time)) / (dpy * mean(diff(age_grid))),
        cohort = map(shift_no, \(n) {
          shift_right(round(n), age_grid)
        }),
        sim_data = pmap(list(mod, age, cohort, mean_time),
                        \(mod, age, cohort, mean_time) {
                          data.frame(age = age, cohort = cohort) %>%
                            generate_data(mod)
                        })
      ) %>%
      select(!!sym(grouping_col), mean_time, sim_data) %>%
      unnest(sim_data)

    # ----- use scam model to monotonize cohort-stratifed prevalence over time----
    scam_out <- scam_data %>%
      filter(
        cohort < max(age) - diff(range(mean_time)) / dpy,
        !is.na(cohort)
      ) %>%
      group_by(cohort, ys) %>%
      group_modify(monotonize_data) %>%
      ungroup()


    # mapping to covert cohort to age
    cohort_age_mapping <- scam_data %>%
      select(col_time, age, cohort) %>%
      unique()

    # map cohort from monotized data to age (at collection time)
    scam_out <- scam_out %>%
      left_join(
        cohort_age_mapping,
        by = join_by(
          !!sym(grouping_col) == !!sym(grouping_col), cohort == cohort, age == age
        )
      ) %>%
      pivot_wider(names_from = ys, values_from = prevalence) %>%
      group_by(!!sym(grouping_col), mean_time)  %>%
      nest()
  }

  # ------ Fit the monotonized data ------
  out <- scam_out %>%
    mutate(
      monotonized_mod = map(data, \(dat){
        gam(y ~ s(age), family = betar, data = dat)
      }),
      # also have model for smooth ci
      monotonized_ci_mod = map(data, \(dat){
        list(
          "ymin" = gam(ymin ~ s(age), family = betar, data = dat),
          "ymax" = gam(ymax ~ s(age), family = betar, data = dat)
        )
      })
    ) %>%
    ungroup() %>%
    select(-data) %>%
    right_join(gam_mods,
               by = join_by(!!sym(grouping_col) == !!sym(grouping_col), mean_time == mean_time)) %>%
    select(-mean_time)

  # reformat output
  out <- out %>%
    # rename to follow the convention of other functions
    rename(
      df = data,
      info = mod,
      monotonized_info = monotonized_mod
    ) %>%
    # finally predict seroprevalence and foi for the input data
    mutate(
      sp = map2(df, info, \(dat, mod){
        predict(mod, list(age = dat$age), type="response")
      }),
      foi = map2(df, sp, \(dat, sp){
        est_foi(dat$age, sp)
      }),
      monotonized_sp = map2(df, monotonized_info, \(dat, mod){
        predict(mod, list(age = dat$age), type="response")
      }),
      monotonized_foi = map2(df, monotonized_sp, \(dat, sp){
        est_foi(dat$age, sp)
      })
    )

  model <- list()

  model$out <- out
  model$grouping_col <- grouping_col
  model$age_correct <- age_correct


  class(model) <- "time_age_model"

  model
}

#' Plot output for time_age_model
#'
#' @param x - a `time_age_model` object
#' @param facet - whether to facet the plot by group or not
#' @param modtype - indicate which model to plot, either "monotonized" or "non-monotonized"
#' @param le - number of bins to generate x axis, higher value return smoother plot
#' @param cex - adjust the of the data points (only when facet = TRUE)
#'
#' @return
#' @export
#'
#' @examples
plot.time_age_model <- function(x, ...){
  # check whether user specify facet
  facet <- if (is.null(list(...)[["facet"]])) TRUE else list(...)$facet
  cex <- if (is.null(list(...)[["cex"]])) 10 else list(...)$cex
  le <- if (is.null(list(...)[["le"]])) 100 else list(...)$le

  assert_that(
    is.logical(facet),
    msg = "facet argument must be of type logical"
  )

  # check which type of model user wants to visualize
  modtype <- if (is.null(list(...)[["modtype"]])) "monotonized" else list(...)$modtype
  assert_that(
    modtype == "monotonized" | modtype == "non-monotonized",
    msg = "modtype argument must be eithers 'monotonized' or 'non-monotonized'"
  )

  # compute the CI for sp
  out <- compute_ci.time_age_model(x, modtype = modtype, le = le)

  # get seroprev data and foi data for plotting
  sp_dat <- out %>% select(!!sym(x$grouping_col), sp_df) %>% unnest(sp_df)
  foi_dat <- out %>% select(!!sym(x$grouping_col), foi_df) %>% unnest(foi_df)

  # get input data points
  df_dat <- x$out %>%
    select(!!sym(x$grouping_col), df) %>%
    unnest(df) %>%
    mutate(age = round(age)) %>%
    group_by(!!sym(x$grouping_col), age) %>%
    summarize(
      pos = sum(status, na.rm = TRUE),
      tot = n(),
      .groups = "drop"
    ) %>%
    mutate(seroprev = pos/tot)

  ggplot() +
    geom_smooth(aes(
      x = x, y = y, ymin=ymin, ymax=ymax,
      color = if(facet) "sero" else as.factor(!!sym(x$grouping_col)),
      fill = if(facet) "ci" else as.factor(!!sym(x$grouping_col))
    ), stat = "identity", lwd=0.5, alpha=0.2, data = sp_dat) +
    geom_line(aes(
      x = x, y = y, color = if(facet) "foi" else as.factor(!!sym(x$grouping_col))
    ), linetype = "dashed", data = foi_dat) +
    ylim(c(0, 1)) +
    if (facet)
      list(
        geom_point(
          aes(x = age, y = seroprev, size = cex*pos/max(tot)), shape = 1,
          data = df_dat
        ),
        guides(shape = "none", size = "none"),
        serosv:::set_plot_style(),
        facet_wrap(vars(!!sym(x$grouping_col)))
      ) else
        labs(color = x$grouping_col, fill = x$grouping_col)
}

#' Compute confidence interval for time age model
#'
#' @param x - serosv models
#' @param ci - confidence interval
#' @param le - number of data for computing confidence interval
#' @param ... - arbitrary argument
#'
#' @return confidence interval dataframe with n_group x 3 cols, the columns are `group`, `sp_df`, `foi_df`
#' @export
compute_ci.time_age_model <- function(x, ci=0.95, le = 100, ...){
  # check which type of model user wants to visualize
  modtype <- if (is.null(list(...)[["modtype"]])) "monotonized" else list(...)$modtype
  assert_that(
    modtype == "monotonized" | modtype == "non-monotonized",
    msg = "modtype argument must be eithers 'monotonized' or 'non-monotonized'"
  )

  p <- (1 - ci) / 2

  # use model to generate seroprev (with CI) and FOI on a finer grid for plotting
  age_range <- range(bind_rows(x$out$df)$age)
  out <- x$out %>%
    mutate(
      age = map(df, \(dat){
        seq(age_range[1], age_range[2], length.out = le)
      })
    )

  # --- use the monotonized model for prediction and ci-----
  if(modtype == "monotonized"){
    out <- out %>%
      mutate(
        sp_df = pmap(list(monotonized_info, monotonized_ci_mod, age), \(mod, ci_mod, grid){
          data.frame(
            x = grid,
            y = predict(mod, list(age = grid), type = "response"),
            ymin = predict(ci_mod$ymin, list(age = grid), type = "response"),
            ymax = predict(ci_mod$ymax, list(age = grid), type = "response")
          )
        })
      )
  }else{
    # --- if user specify non-monotonized then simply compute CI from gam model-----
    out <- out %>%
      mutate(
        sp_df = map2(info, age, \(mod, grid){
          link_inv <- mod$family$linkinv
          dataset <- mod$model[,1:2]
          n <- nrow(dataset) - length(mod$coefficients)

          predict(mod, data.frame(age = grid), se.fit = TRUE)  %>%
            as_tibble()  %>%
            select(fit, se.fit) %>%
            mutate(
              x = grid,
              ymin = link_inv(fit + qt(    p, n) * se.fit),
              ymax = link_inv(fit + qt(1 - p, n) * se.fit),
              y = link_inv(fit)
            )  %>%
            select(- se.fit)
        })
      )
  }

  # --- Finally, compute FOI -----
  out <- out %>%
    mutate(
      foi_df = map2(age, sp_df, \(grid, sp){
        foi_x <- sort(unique(grid))
        foi_x <- foi_x[c(-1, -length(foi_x) )]

        tibble(
          x = foi_x,
          y = est_foi(grid, sp$y)
        )
      })
    ) %>%
    select(!!sym(x$grouping_col), sp_df, foi_df)
}
