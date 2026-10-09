# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# BOOTSTRAP 95% + STAT_SK_BOOTSTRAP()
# Indikator : IPM
# Periode   : 2022-2024
# Replikasi : B = 1.999
# ============================================================

library(tidyverse)
library(ggplot2)

source("R/04_theme_palet_tim.R")

# ============================================================
# 1. MEMBACA DAN MEMERIKSA DATA
# ============================================================

data_bersih <- readRDS("data/data-bersih/atlas_sulsel.rds")

stopifnot(
  all(c("tahun", "kabupaten_kota", "ipm") %in% names(data_bersih))
)

data_ipm <- data_bersih %>%
  filter(tahun %in% 2022:2024) %>%
  select(kabupaten_kota, tahun, ipm) %>%
  filter(!is.na(kabupaten_kota), !is.na(ipm))

if (anyDuplicated(data_ipm[c("kabupaten_kota", "tahun")]) > 0) {
  stop("Terdapat duplikasi wilayah-tahun.")
}

cek_data <- data_ipm %>%
  count(kabupaten_kota, name = "jumlah_tahun")

if (nrow(cek_data) != 24 || any(cek_data$jumlah_tahun != 3)) {
  stop("Diperlukan 24 wilayah dengan data lengkap tahun 2022-2024.")
}

# ============================================================
# 2. FUNGSI PERHITUNGAN BOOTSTRAP
# ============================================================

hitung_bootstrap <- function(x, B = 1999, conf = 0.95) {
  
  if (length(x) < 2 || any(!is.finite(x))) {
    stop("Data harus memiliki minimal dua nilai numerik yang valid.")
  }
  
  if (B < 1999) stop("B minimal 1999.")
  if (conf <= 0 || conf >= 1) stop("conf harus antara 0 dan 1.")
  
  hasil_boot <- replicate(
    B,
    mean(sample(x, length(x), replace = TRUE))
  )
  
  alpha <- 1 - conf
  
  tibble(
    estimasi = mean(x),
    batas_bawah = unname(quantile(hasil_boot, alpha / 2)),
    batas_atas = unname(quantile(hasil_boot, 1 - alpha / 2)),
    jumlah_pengamatan = length(x),
    B = B,
    tingkat_kepercayaan = conf
  )
}

# ============================================================
# 3. MENGHITUNG BOOTSTRAP UNTUK 24 WILAYAH
# ============================================================

set.seed(123)

hasil_bootstrap <- data_ipm %>%
  group_by(kabupaten_kota) %>%
  summarise(
    hasil = list(hitung_bootstrap(ipm, B = 1999, conf = 0.95)),
    .groups = "drop"
  ) %>%
  unnest(hasil) %>%
  arrange(estimasi) %>%
  mutate(
    urutan = row_number(),
    kabupaten_kota = factor(
      kabupaten_kota,
      levels = kabupaten_kota
    )
  )

print(hasil_bootstrap)

# ============================================================
# 4. GRAFIK BOOTSTRAP UTAMA
# ============================================================

grafik_bootstrap <- ggplot(
  hasil_bootstrap,
  aes(x = estimasi, y = kabupaten_kota)
) +
  geom_errorbar(
    aes(xmin = batas_bawah, xmax = batas_atas),
    orientation = "y",
    width = 0.18,
    linewidth = 0.8,
    colour = palet_dasar["Netral"]
  ) +
  geom_point(
    size = 2.8,
    colour = palet_indikator["IPM"]
  ) +
  labs(
    title = "Ketidakpastian Estimasi IPM",
    subtitle = "Rata-rata 2022-2024 | Bootstrap 95% | B = 1.999",
    x = "Rata-rata Indeks Pembangunan Manusia (IPM)",
    y = NULL,
    caption = paste(
      "Sumber: BPS Provinsi Sulawesi Selatan | Diolah.",
      "Interval berdasarkan tiga nilai tahunan per wilayah;",
      "interpretasikan secara eksploratif."
    )
  ) +
  theme_tim() +
  theme(
    plot.title = element_text(
      face = "bold",
      colour = palet_dasar["Gelap"]
    ),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

print(grafik_bootstrap)

# ============================================================
# 5. MEMBUAT STAT_SK_BOOTSTRAP() DENGAN GGPROTO
# Layer tambahan untuk menghitung ringkasan per kelompok
# ============================================================

StatSKBootstrap <- ggproto(
  "StatSKBootstrap",
  Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales,
                           B = 1999,
                           conf = 0.95,
                           seed = 123) {
    
    x <- data$x
    y <- data$y
    
    valid <- is.finite(x) & is.finite(y)
    x <- x[valid]
    y <- y[valid]
    
    if (length(y) < 2) {
      return(data.frame())
    }
    
    if (B < 1999) stop("B minimal 1999.")
    if (conf <= 0 || conf >= 1) stop("conf harus antara 0 dan 1.")
    
    set.seed(seed)
    
    hasil_boot <- replicate(
      B,
      mean(sample(y, size = length(y), replace = TRUE))
    )
    
    alpha <- 1 - conf
    
    data.frame(
      x = mean(x),
      y = mean(y),
      ymin = unname(quantile(hasil_boot, alpha / 2)),
      ymax = unname(quantile(hasil_boot, 1 - alpha / 2))
    )
  }
)

stat_sk_bootstrap <- function(mapping = NULL,
                              data = NULL,
                              geom = "pointrange",
                              position = "identity",
                              ...,
                              B = 1999,
                              conf = 0.95,
                              seed = 123,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  
  layer(
    stat = StatSKBootstrap,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      B = B,
      conf = conf,
      seed = seed,
      na.rm = na.rm,
      ...
    )
  )
}

# ============================================================
# 6. MENYIMPAN HASIL BOOTSTRAP UTAMA
# ============================================================

dir.create(
  "keluaran/bootstrap",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  hasil_bootstrap %>%
    mutate(kabupaten_kota = as.character(kabupaten_kota)),
  "keluaran/bootstrap/hasil_bootstrap_ipm_2022_2024.csv"
)

ggsave(
  "keluaran/bootstrap/grafik_bootstrap_ipm_2022_2024.png",
  plot = grafik_bootstrap,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

