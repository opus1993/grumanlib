library(testthat)
library(dplyr)
library(ggplot2)
library(yardstick)

# saveRDS(object = resample_idx, file = testthat::test_path("test_autoplot.rds"))
resample_idx <- readRDS(testthat::test_path("test_autoplot.rds"))

two_class_resamples <- bind_rows(
  lapply(resample_idx, function(idx) two_class_example[idx,]),
  .id = "Resample"
) %>%
  group_by(Resample)

# make it smaller, and order it in the same order as what ggplot2 displays
hpc_cv2 <- filter(hpc_cv, Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10")) %>%
  as_tibble() %>%
  group_by(Resample) %>%
  arrange(as.character(obs)) %>%
  ungroup()

# Confusion Matrix  ------------------------------------------------------------

test_that("Confusion Matrix Heatmap)", {
   df <- filter(hpc_cv, Resample == "Fold01")

   res1 <- conf_mat(df, obs, pred, dnn = c("Pred", "True"))
   expect_error(p1 <- ggconfusion_matrix(res1), NA)
   expect_identical(p1$labels$x, "True")
   expect_identical(p1$labels$y, "Pred")

# Defaults are used when there are no names
   res2 <- conf_mat(df, obs, pred, dnn = NULL)
   expect_error(p2 <- ggconfusion_matrix(res2), NA)
   expect_identical(p2$labels$x, "Truth")
   expect_identical(p2$labels$y, "Prediction")
 })

test_that("Confusion Matrix multi class", {
   res <- hpc_cv %>%
     filter(Resample == "Fold01") %>%
     conf_mat(obs, pred)

   expect_error(.plot <- ggconfusion_matrix(res), NA)
   expect_s3_class(.plot, "gg")

   .plot_data <- ggplot_build(.plot)

   # panes
   expect_equal(nrow(.plot_data$data[[1]]), length(res$table))

})
