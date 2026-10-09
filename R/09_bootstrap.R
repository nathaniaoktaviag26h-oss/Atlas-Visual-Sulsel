# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# BOOTSTRAP 95% + STAT_SK_BOOTSTRAP()
# Indikator : IPM dan Kemiskinan
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
  all(c("tahun", "kabupaten_kota", "ipm", "kemiskinan") %in%
        names(data_bersih))
)

data_atlas <- data_bersih %>%
  filter(tahun %in% 2022:2024) %>%
  select(kabupaten_kota, tahun, ipm, kemiskinan) %>%
  filter(!is.na(kabupaten_kota))

if (anyDuplicated(data_atlas[c("kabupaten_kota", "tahun")]) > 0) {
  stop("Terdapat duplikasi kombinasi wilayah dan tahun.")
}

cek_tahun <- data_atlas %>%
  count(kabupaten_kota, name = "n_tahun")

if (nrow(cek_tahun) != 24 || any(cek_tahun$n_tahun != 3)) {
  stop("Diperlukan 24 wilayah dengan data tahun 2022-2024 lengkap.")
}

if (any(!is.finite(data_atlas$ipm)) ||
    any(!is.finite(data_atlas$kemiskinan))) {
  stop("Ada nilai indikator yang tidak valid atau tidak lengkap.")
}

# ============================================================
# 2. FUNGSI BOOTSTRAP
# ============================================================

hitung_bootstrap <- function(x, B = 1999, conf = 0.95) {
  
  x <- x[is.finite(x)]
  
  if (length(x) < 2) {
    stop("Diperlukan minimal dua pengamatan valid.")
  }
  
  if (B < 1999) stop("B minimal 1999.")
  if (conf <= 0 || conf >= 1) stop("conf harus antara 0 dan 1.")
  
  hasil_boot <- replicate(
    B,
    mean(sample(x, size = length(x), replace = TRUE))
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
# 3. HASIL BOOTSTRAP UNTUK TABEL
# ============================================================

set.seed(123)

hitung_semua_wilayah <- function(data, variabel, nama_indikator) {
  
  data %>%
    group_by(kabupaten_kota) %>%
    summarise(
      hasil = list(
        hitung_bootstrap(
          .data[[variabel]],
          B = 1999,
          conf = 0.95
        )
      ),
      .groups = "drop"
    ) %>%
    unnest(hasil) %>%
    mutate(indikator = nama_indikator) %>%
    arrange(estimasi)
}

hasil_ipm <- hitung_semua_wilayah(
  data_atlas, "ipm", "IPM"
)

hasil_kemiskinan <- hitung_semua_wilayah(
  data_atlas, "kemiskinan", "Kemiskinan"
)

hasil_bootstrap <- bind_rows(hasil_ipm, hasil_kemiskinan)

print(hasil_bootstrap)

# ============================================================
# 4. STAT_SK_BOOTSTRAP() DENGAN GGP ROTO
# ============================================================

StatSKBootstrap <- ggproto(
  "StatSKBootstrap",
  Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales,
                           B = 1999,
                           conf = 0.95,
                           seed = 123) {
    
    # x adalah indeks numerik wilayah
    # y adalah nilai indikator tahunan
    data <- data[
      is.finite(data$x) & is.finite(data$y),
      ,
      drop = FALSE
    ]
    
    if (nrow(data) < 2) {
      return(data.frame())
    }
    
    if (B < 1999) stop("B minimal 1999.")
    if (conf <= 0 || conf >= 1) stop("conf harus antara 0 dan 1.")
    
    # Replikasi dapat diulang secara konsisten
    set.seed(seed)
    
    nilai <- data$y
    n <- length(nilai)
    
    hasil_boot <- replicate(
      B,
      mean(sample(nilai, size = n, replace = TRUE))
    )
    
    alpha <- 1 - conf
    
    data.frame(
      x = mean(data$x),
      y = mean(nilai),
      ymin = unname(quantile(hasil_boot, alpha / 2)),
      ymax = unname(quantile(hasil_boot, 1 - alpha / 2)),
      n = n
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
# 5. MENYIAPKAN DATA UNTUK GRAFIK
# ============================================================

# Urutan wilayah berdasarkan rata-rata IPM
urutan_ipm <- data_atlas %>%
  group_by(kabupaten_kota) %>%
  summarise(rataan = mean(ipm), .groups = "drop") %>%
  arrange(rataan) %>%
  pull(kabupaten_kota)

data_grafik_ipm <- data_atlas %>%
  mutate(
    kabupaten_kota = factor(
      kabupaten_kota,
      levels = urutan_ipm
    ),
    urutan = as.numeric(kabupaten_kota)
  )

# Urutan wilayah berdasarkan rata-rata kemiskinan
urutan_kemiskinan <- data_atlas %>%
  group_by(kabupaten_kota) %>%
  summarise(rataan = mean(kemiskinan), .groups = "drop") %>%
  arrange(rataan) %>%
  pull(kabupaten_kota)

data_grafik_kemiskinan <- data_atlas %>%
  mutate(
    kabupaten_kota = factor(
      kabupaten_kota,
      levels = urutan_kemiskinan
    ),
    urutan = as.numeric(kabupaten_kota)
  )

# ============================================================
# 6. GRAFIK IPM MENGGUNAKAN STAT_SK_BOOTSTRAP()
# ============================================================

grafik_ipm <- ggplot(
  data_grafik_ipm,
  aes(x = urutan, y = ipm, group = kabupaten_kota)
) +
  stat_sk_bootstrap(
    B = 1999,
    conf = 0.95,
    seed = 123,
    colour = unname(palet_indikator["IPM"]),
    linewidth = 0.7,
    fatten = 2.5
  ) +
  scale_x_continuous(
    breaks = seq_along(urutan_ipm),
    labels = urutan_ipm,
    expand = expansion(add = 0.5)
  ) +
  coord_flip() +
  labs(
    title = "Ketidakpastian Estimasi IPM",
    subtitle = "Rata-rata 2022-2024 | Bootstrap 95% | B = 1.999",
    x = NULL,
    y = "Rata-rata Indeks Pembangunan Manusia (IPM)",
    caption = "Sumber: BPS Provinsi Sulawesi Selatan | Diolah"
  ) +
  theme_tim() +
  theme(
    plot.title = element_text(
      face = "bold",
      colour = unname(palet_dasar["Gelap"])
    ),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

print(grafik_ipm)

# ============================================================
# 7. GRAFIK KEMISKINAN MENGGUNAKAN STAT_SK_BOOTSTRAP()
# ============================================================

grafik_kemiskinan <- ggplot(
  data_grafik_kemiskinan,
  aes(x = urutan, y = kemiskinan, group = kabupaten_kota)
) +
  stat_sk_bootstrap(
    B = 1999,
    conf = 0.95,
    seed = 456,
    colour = unname(palet_indikator["Kemiskinan"]),
    linewidth = 0.7,
    fatten = 2.5
  ) +
  scale_x_continuous(
    breaks = seq_along(urutan_kemiskinan),
    labels = urutan_kemiskinan,
    expand = expansion(add = 0.5)
  ) +
  coord_flip() +
  labs(
    title = "Ketidakpastian Estimasi Kemiskinan",
    subtitle = "Rata-rata 2022-2024 | Bootstrap 95% | B = 1.999",
    x = NULL,
    y = "Rata-rata Persentase Penduduk Miskin (%)",
    caption = "Sumber: BPS Provinsi Sulawesi Selatan | Diolah"
  ) +
  theme_tim() +
  theme(
    plot.title = element_text(
      face = "bold",
      colour = unname(palet_dasar["Gelap"])
    ),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

print(grafik_kemiskinan)

# ============================================================
# 8. MENYIMPAN TABEL DAN GRAFIK
# ============================================================

dir.create(
  "keluaran/bootstrap",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  hasil_ipm,
  "keluaran/bootstrap/hasil_bootstrap_ipm_2022_2024.csv"
)

write_csv(
  hasil_kemiskinan,
  "keluaran/bootstrap/hasil_bootstrap_kemiskinan_2022_2024.csv"
)

ggsave(
  "keluaran/bootstrap/grafik_bootstrap_ipm_2022_2024.png",
  plot = grafik_ipm,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggsave(
  "keluaran/bootstrap/grafik_bootstrap_kemiskinan_2022_2024.png",
  plot = grafik_kemiskinan,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
