library(tinytable)

idx_src <- letters[1:5]
idx_tar <- c("K", "T", "L")

aus <- data.frame(ctry = "AUS", name = idx_src, value = c(5, 10, 50, 45, 20))
aus_mod <- data.frame(ctry = "AUS", name = idx_tar, value = c(15, 75, 40))
usa <- data.frame(ctry = "USA", name = idx_tar, value = c(50, 60, 30))
union <- rbind(aus_mod, usa)

tt(aus) |>
  style_tt(
    i = 1:nrow(aus),
    j = 2,
    background = "teal"
  ) |>
  group_tt(
    j = list("Shared Mass Array" = 2:3)
  )
