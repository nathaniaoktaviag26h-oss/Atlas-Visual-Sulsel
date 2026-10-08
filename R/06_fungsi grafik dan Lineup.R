# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# CP3 - FUNGSI GRAFIK DAN LINEUP
# ============================================================
#
# Capaian:
# 45. plot_tren() dengan {{}}
# 46. plot_scatter() dengan {{}}
# 47. plot_sebaran() dengan {{}}
# 48. Lineup 20 panel (1 asli + 19 permutasi)
# 49. Simpan posisi panel asli ke kunci.rds
# 50. Simpan lineup-1.png
#
# Seed lineup: 123
# ============================================================


# ============================================================
# 1. PACKAGE
# ============================================================

library(tidyverse)
library(nullabor)


# ============================================================
# 2. THEME DAN PALET CP2
# ============================================================

source("R/04_theme_palet_tim.R")


# ============================================================
# 3. DATA
# ============================================================

data_bersih <- readRDS(
  "data/data-bersih/atlas_sulsel.rds"
)


# ============================================================
# 4. FUNGSI GRAFIK CP3
# ============================================================


# ------------------------------------------------------------
# 4.1 FUNGSI PLOT TREN
#     Menggunakan {{ }} untuk nama kolom indikator
# ------------------------------------------------------------

plot_tren <- function(data, indikator) {
  
  ggplot(
    data,
    aes(
      x = tahun,
      y = {{ indikator }},
      group = kabupaten_kota
    )
  ) +
    geom_line(
      color = "#5B8DB8",
      linewidth = 0.7,
      alpha = 0.8
    ) +
    geom_point(
      color = "#163A5C",
      size = 2
    ) +
    scale_x_continuous(
      breaks = sort(unique(data$tahun))
    ) +
    labs(
      title = "Tren IPM Sulawesi Selatan",
      x = "Tahun",
      y = "IPM"
    ) +
    theme_tim()
}


# ------------------------------------------------------------
# 4.2 FUNGSI PLOT SCATTER
#     Menggunakan {{ }} untuk nama kolom
# ------------------------------------------------------------

plot_scatter <- function(data, x, y) {
  
  ggplot(
    data,
    aes(
      x = {{ x }},
      y = {{ y }}
    )
  ) +
    geom_point(
      color = "#5B8DB8",
      size = 2.5,
      alpha = 0.85
    ) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      linewidth = 0.8,
      color = "#163A5C",
      fill = palet_pita,
      alpha = 0.30
    ) +
    labs(
      title = "Hubungan Kemiskinan dan IPM",
      x = "Persentase Penduduk Miskin",
      y = "IPM"
    ) +
    theme_tim()
}

# ------------------------------------------------------------
# 4.3 FUNGSI PLOT SEBARAN
#     Fungsi ketiga menggunakan {{ }}
# ------------------------------------------------------------

plot_sebaran <- function(data, indikator) {
  
  ggplot(
    data,
    aes(
      x = {{ indikator }}
    )
  ) +
    geom_histogram(
      bins = 10,
      fill = "#5B8DB8",
      color = "white",
      alpha = 0.9
    ) +
    labs(
      title = "Sebaran IPM Kabupaten/Kota",
      x = "IPM",
      y = "Frekuensi"
    ) +
    theme_tim()
}


# ============================================================
# 5. UJI FUNGSI
# ============================================================


# ------------------------------------------------------------
# 5.1 Uji plot_tren()
# ------------------------------------------------------------

grafik_tren <- plot_tren(
  data_bersih,
  ipm
)

print(grafik_tren)


# ------------------------------------------------------------
# 5.2 Uji plot_scatter()
# ------------------------------------------------------------

grafik_scatter <- plot_scatter(
  data_bersih,
  kemiskinan,
  ipm
)

print(grafik_scatter)


# ------------------------------------------------------------
# 5.3 Uji plot_sebaran()
# ------------------------------------------------------------

grafik_sebaran <- plot_sebaran(
  data_bersih,
  ipm
)

print(grafik_sebaran)


# ============================================================
# 6. SIAPKAN DATA LINEUP
#    Menggunakan data tahun 2024
# ============================================================

data_lineup <- data_bersih %>%
  filter(tahun == 2024) %>%
  select(
    kabupaten_kota,
    ipm,
    kemiskinan
  ) %>%
  drop_na() %>%
  mutate(
    jenis_wilayah = case_when(
      str_detect(
        kabupaten_kota,
        regex("^Kota", ignore_case = TRUE)
      ) ~ "Kota",
      TRUE ~ "Kabupaten"
    )
  )


# ============================================================
# 7. BUAT LINEUP 20 PANEL
#    1 DATA ASLI + 19 DATA PERMUTASI
# ============================================================

set.seed(123)

lineup_20 <- lineup(
  null_permute("ipm"),
  true = data_lineup,
  n = 20
)


# ============================================================
# 8. SIAPKAN FOLDER OUTPUT
# ============================================================

dir.create(
  "keluaran/lineup",
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 9. SIMPAN POSISI DATA ASLI
#    KHUSUS PENELITI
# ============================================================

posisi_asli <- attr(
  lineup_20,
  "pos"
)

saveRDS(
  posisi_asli,
  "keluaran/lineup/kunci.rds"
)


# ============================================================
# 10. GRAFIK LINEUP
# ============================================================

grafik_lineup <- ggplot(
  lineup_20,
  aes(
    x = kemiskinan,
    y = ipm
  )
) +
  
  
  # ----------------------------------------------------------
# TITIK KABUPATEN
# ----------------------------------------------------------

geom_point(
  data = ~ dplyr::filter(
    .x,
    jenis_wilayah == "Kabupaten"
  ),
  aes(
    color = "Kabupaten",
    shape = "Kabupaten"
  ),
  size = 2.2,
  alpha = 0.85
) +
  
  
  # ----------------------------------------------------------
# TITIK KOTA
# ----------------------------------------------------------

geom_point(
  data = ~ dplyr::filter(
    .x,
    jenis_wilayah == "Kota"
  ),
  aes(
    color = "Kota",
    shape = "Kota"
  ),
  size = 2.5,
  alpha = 0.90
) +
  
  
  # ----------------------------------------------------------
# GARIS REGRESI DAN CONFIDENCE INTERVAL
# ----------------------------------------------------------

geom_smooth(
  method = "lm",
  se = TRUE,
  linewidth = 0.8,
  color = "#163A5C",
  fill = palet_pita,
  alpha = 0.30
) +
  
  
  # ----------------------------------------------------------
# 20 PANEL
# ----------------------------------------------------------

facet_wrap(
  ~ .sample,
  ncol = 5
) +
  
  
  # ----------------------------------------------------------
# WARNA JENIS WILAYAH
# ----------------------------------------------------------

scale_color_manual(
  name = "Jenis wilayah",
  values = c(
    "Kabupaten" = "#5B8DB8",
    "Kota" = "#E69F5B"
  )
) +
  
  
  # ----------------------------------------------------------
# BENTUK JENIS WILAYAH
# 16 = lingkaran
# 17 = segitiga
# ----------------------------------------------------------

scale_shape_manual(
  name = "Jenis wilayah",
  values = c(
    "Kabupaten" = 16,
    "Kota" = 17
  )
) +
  
  
  # ----------------------------------------------------------
# LABEL
# ----------------------------------------------------------

labs(
  title = "Manakah panel yang paling berbeda dari yang lain?",
  subtitle = paste(
    "Dari 20 panel, satu memuat data asli.",
    "Pilih panel yang menurut Anda paling berbeda."
  ),
  x = "Persentase Penduduk Miskin",
  y = "IPM"
) +
  
  
  # ----------------------------------------------------------
# THEME CP2
# ----------------------------------------------------------

theme_tim() +
  
  
  # ----------------------------------------------------------
# PERAPIAN LINEUP
# ----------------------------------------------------------

theme(
  
  plot.title = element_text(
    size = 16,
    face = "bold"
  ),
  
  plot.subtitle = element_text(
    size = 10
  ),
  
  legend.position = "top",
  
  legend.title = element_text(
    face = "bold"
  ),
  
  strip.text = element_text(
    size = 10,
    face = "bold"
  ),
  
  panel.border = element_rect(
    color = "#7A7A7A",
    fill = NA,
    linewidth = 0.7
  ),
  
  panel.spacing = unit(
    0.7,
    "lines"
  ),
  
  axis.text = element_text(
    size = 8
  ),
  
  axis.title = element_text(
    size = 10,
    face = "bold"
  ),
  
  legend.key = element_blank()
)


# ============================================================
# 11. TAMPILKAN LINEUP
# ============================================================

print(grafik_lineup)


# ============================================================
# 12. SIMPAN LINEUP
# ============================================================
#
# Gambar ini yang digunakan untuk observer.
# Tidak menampilkan posisi data asli.
# ============================================================

ggsave(
  filename = "keluaran/lineup-20 panel.png",
  plot = grafik_lineup,
  width = 14,
  height = 11,
  units = "in",
  dpi = 300
)

# ============================================================
# 13. CEK POSISI DATA ASLI
#     HANYA UNTUK PENELITI
# ============================================================

posisi_asli


# ============================================================
# 14. CEK KUNCI YANG TERSIMPAN
# ============================================================

readRDS(
  "keluaran/lineup/kunci.rds"
)
