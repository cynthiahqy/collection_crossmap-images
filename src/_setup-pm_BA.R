### {r manual-pm-equal}
## correspondence/concordance table
codes_BA <- dplyr::tribble(
    ~std_B, ~std_A,
    "A1", "x1111", # one-to-one
    "B2", "x2222", # many-to-one
    "B2", "x3333",
    "C3", "x4444", # one-to-many (4)
    "C4", "x4444",
    "C4", "x6666", # many-to-many
    "C5", "x4444",
    "C6", "x4444",
    "C7", "x5555", # one-to-many (3)
    "C8", "x5555",
)

## panel_map
weights_BA <- codes_BA |>
    dplyr::distinct(std_B, std_A) |>
    dplyr::group_by(std_A) |>
    dplyr::mutate(
        n_dest = dplyr::n(),
        weight = 1 / n_dest
    ) |>
    dplyr::ungroup()

pm_BA <- weights_BA |>
    dplyr::select(std_B, std_A, weight) |>
    dplyr::arrange(std_A)

set.seed(1832)
std_A_codes <- sort(unique(codes_BA$std_A))
data_A <-
    dplyr::tibble(
        std_A = std_A_codes,
        A_100 = 100,
        A_prod = round(abs(rnorm(length(std_A_codes)) * 10000), 0)
    )

error_highlight_col <- "#d88080"
