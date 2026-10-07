# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# CP2 - GALERI GRAFIK
# ============================================================

# 1. PAKET ----------------------------------------------------
library(ggplot2)
library(dplyr)
library(stringr)
library(forcats)
library(patchwork)

# 2. TEMA & PALET ---------------------------------------------
source("R/04_theme_palet_tim.R")
theme_set(theme_tim())

# 3. DATA -----------------------------------------------------
data_bersih <- readRDS("data/data-bersih/atlas_sulsel.rds")

# 4. FUNGSI BANTU ---------------------------------------------
koma <- function(x, d = 1)
  format(round(x, d), nsmall = d, decimal.mark = ",")

nama_baku <- function(x) {
  x |>
    str_squish() |>
    str_to_title() |>
    str_replace_all("\\bDan\\b", "dan")
}

dir.create("keluaran", showWarnings = FALSE)

simpan <- function(grafik, nama, lebar, tinggi) {
  ggsave(
    file.path("keluaran", nama), grafik,
    width = lebar, height = tinggi,
    dpi = 300, bg = "white", limitsize = FALSE
  )
}

# 5. SIAPKAN DATA ---------------------------------------------
data_3tahun <- data_bersih |>
  mutate(kode_wilayah = as.character(kode_wilayah)) |>
  filter(
    str_detect(kode_wilayah, "^73\\d{2}$"),
    tahun %in% 2022:2024
  ) |>
  mutate(
    wilayah = nama_baku(kabupaten_kota),
    kelompok = if_else(
      kode_wilayah %in% c("7371", "7372", "7373"),
      "Kota", "Kabupaten"
    ),
    tahun = factor(tahun, levels = 2022:2024)
  )

stopifnot(
  n_distinct(data_3tahun$kode_wilayah) == 24,
  nrow(data_3tahun) == 72
)

colSums(is.na(data_3tahun[c("ipm", "kemiskinan")]))

urut_2024 <- data_3tahun |>
  filter(tahun == "2024") |>
  arrange(ipm) |>
  pull(wilayah)

data_3tahun <- data_3tahun |>
  mutate(
    wilayah_urut = factor(wilayah, levels = urut_2024),
    wilayah_facet = factor(wilayah, levels = rev(urut_2024))
  )

# Statistik median
med <- data_3tahun |>
  group_by(tahun) |>
  summarise(m = median(ipm), .groups = "drop")

m22 <- med$m[med$tahun == "2022"]
m24 <- med$m[med$tahun == "2024"]

arah <- if_else(
  m24 > m22, "naik",
  if_else(m24 < m22, "turun", "tidak berubah")
)

# ============================================================
# GRAFIK 1 — SEBARAN IPM
# ============================================================

grafik_1 <- ggplot(
  data_3tahun,
  aes(tahun, ipm, fill = tahun)
) +
  geom_boxplot(
    width = 0.55,
    alpha = 0.30,
    colour = palet_dasar["Gelap"],
    linewidth = 0.4,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(fill = tahun),
    shape = 21,
    colour = palet_dasar["Gelap"],
    width = 0.08,
    height = 0,
    size = 2.5,
    stroke = 0.6,
    alpha = 0.9,
    seed = 2026
  ) +
  scale_fill_manual(values = palet_tahun) +
  labs(
    title = "Sebaran IPM 24 kabupaten/kota, 2022–2024",
    subtitle = paste0(
      "Median IPM ", arah, " dari ", koma(m22),
      " (2022) ke ", koma(m24), " (2024)"
    ),
    x = "Tahun",
    y = "Indeks Pembangunan Manusia",
    caption = "Sumber: BPS Provinsi Sulawesi Selatan; olahan tim."
  ) +
  theme(legend.position = "none")

print(grafik_1)

# ============================================================
# GRAFIK 2 — SMALL MULTIPLES 24 WILAYAH
# ============================================================

rujukan <- data_3tahun |>
  group_by(tahun) |>
  summarise(ipm = median(ipm), .groups = "drop")

grafik_2 <- ggplot(
  data_3tahun,
  aes(tahun, ipm, group = wilayah)
) +
  geom_line(
    data = rujukan,
    aes(group = 1),
    colour = palet_dasar["Netral"],
    linetype = "dashed"
  ) +
  geom_line(
    colour = palet_indikator["IPM"],
    linewidth = 0.8
  ) +
  geom_point(
    colour = palet_indikator["IPM"],
    size = 2
  ) +
  facet_wrap(~wilayah_facet, ncol = 6) +
  scale_x_discrete(
    breaks = c("2022", "2024")
  ) +
  labs(
    title = "IPM meningkat di seluruh kabupaten/kota 2022–2024",
    subtitle = "Garis putus-putus menunjukkan median 24 wilayah",
    x = NULL,
    y = "Indeks Pembangunan Manusia",
    caption = "Sumber: BPS Provinsi Sulawesi Selatan; olahan tim."
  ) +
  theme(
    strip.text = element_text(size = 8)
  )

print(grafik_2)

# ============================================================
# GRAFIK 3 — HUBUNGAN IPM & KEMISKINAN
# ============================================================

grafik_3 <- ggplot(
  data_3tahun,
  aes(ipm, kemiskinan)
) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    level = 0.95,
    colour = palet_indikator["Kemiskinan"],
    fill = palet_pita,
    alpha = 0.75,
    linewidth = 1
  ) +
  geom_point(
    aes(fill = kelompok, shape = kelompok),
    colour = palet_dasar["Gelap"],
    size = 2.5,
    stroke = 0.5,
    alpha = 0.9
  ) +
  scale_fill_manual(
    values = c(
      "Kabupaten" = "#5B8DB8",
      "Kota" = "#E69F5B"
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      "Kabupaten" = 21,
      "Kota" = 24
    ),
    name = NULL
  ) +
  facet_wrap(~tahun, nrow = 1) +
  labs(
    title = "Wilayah ber-IPM lebih tinggi cenderung tingkat kemiskinannya lebih rendah",
    x = "Indeks Pembangunan Manusia",
    y = "Penduduk miskin (%)",
    caption = "Sumber: BPS Provinsi Sulawesi Selatan; olahan tim. Garis = regresi linear; pita = SK 95%."
  )

print(grafik_3)

# ============================================================
# GRAFIK 4 — RANKING IPM
# ============================================================

kiri <- floor(min(data_3tahun$ipm)) - 1

grafik_4 <- ggplot(
  data_3tahun,
  aes(ipm, wilayah_urut)
) +
  geom_segment(
    aes(
      x = kiri,
      xend = ipm,
      yend = wilayah_urut
    ),
    colour = palet_dasar["Netral"]
  ) +
  geom_point(
    aes(
      fill = kelompok,
      shape = kelompok
    ),
    colour = palet_dasar["Gelap"],
    size = 2.5,
    stroke = 0.7
  ) +
  scale_fill_manual(
    values = palet_jenis,
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      Kabupaten = 21,
      Kota = 24
    ),
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(kiri, NA)
  ) +
  facet_wrap(~tahun, nrow = 1) +
  labs(
    title = paste0(
      "IPM tertinggi pada 2024: ",
      paste(rev(tail(urut_2024, 3)), collapse = ", ")
    ),
    subtitle = "Peringkat 24 kab/kota mengikuti urutan IPM 2024",
    x = "Indeks Pembangunan Manusia",
    y = NULL,
    caption = "Sumber: BPS Provinsi Sulawesi Selatan; olahan tim."
  )

print(grafik_4)

# Khusus data Tahun 2024
data_ranking <- data_3tahun |>
  filter(tahun == "2024") |>
  mutate(
    wilayah_urut = fct_reorder(
      wilayah,
      ipm
    )
  )
kiri <- floor(min(data_ranking$ipm)) - 1
grafik_4_2024 <- ggplot(
  data_ranking,
  aes(ipm, wilayah_urut)
) +
  geom_segment(
    aes(
      x = kiri,
      xend = ipm,
      yend = wilayah_urut
    ),
    colour = palet_dasar["Netral"]
  ) +
  geom_point(
    aes(
      fill = kelompok,
      shape = kelompok
    ),
    colour = palet_dasar["Gelap"],
    size = 2.5,
    stroke = 0.7
  ) +
  scale_fill_manual(
    values = palet_jenis,
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      Kabupaten = 21,
      Kota = 24
    ),
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(kiri, NA)
  ) +
  labs(
    title = "Peringkat IPM memperlihatkan kesenjangan antarwilayah",
    subtitle = "24 kabupaten/kota di Sulawesi Selatan, 2024",
    x = "Indeks Pembangunan Manusia",
    y = NULL,
    caption = "Sumber: BPS Provinsi Sulawesi Selatan; olahan tim."
  )

print(grafik_4_2024)
# ============================================================
# GRAFIK 5 — MULTIPANEL 2 × 2
# ============================================================

gA <- grafik_1 +
  labs(
    title = "Sebaran IPM bergeser ke tingkat lebih tinggi",
    subtitle = NULL,
    caption = NULL
  ) +
  theme(
    plot.title = element_text(size = 9, face = "bold"),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6),
    legend.position = "none",
    plot.margin = margin(2, 4, 2, 2)
  )

gB <- grafik_3 +
  labs(
    title = "IPM lebih tinggi cenderung terkait kemiskinan lebih rendah",
    caption = NULL
  ) +
  theme(
    plot.title = element_text(size = 9, face = "bold"),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 4)
  )

gC <- grafik_2 +
  labs(
    title = "IPM meningkat di seluruh kabupaten/kota",
    subtitle = "Garis putus-putus = median 24 wilayah",
    caption = NULL
  ) +
  theme(
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(
      size = 6.5,
      colour = palet_dasar["Gelap"]
    ),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 5.5),
    strip.text = element_text(
      size = 5.5,
      face = "bold"
    ),
    legend.position = "none",
    plot.margin = margin(2, 4, 2, 2)
  )

gD <- grafik_4 +
  labs(
    title = "Perbedaan tingkat IPM antarwilayah masih terlihat",
    subtitle = NULL,
    caption = NULL
  ) +
  theme(
    plot.title = element_text(size = 9, face = "bold"),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 5.5),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 4)
  )

grafik_5 <- (gA | gB) / (gC | gD) +
  plot_layout(
    widths = c(1, 1),
    heights = c(0.75, 1.25)
  ) +
  plot_annotation(
    title = "IPM meningkat di seluruh kabupaten/kota, tetapi ketimpangan antarwilayah tetap lebar",
    subtitle = paste0(
      "Median IPM Sulsel 2022-2024 naik dari ", koma(m22),
      " menjadi ", koma(m24),
      "; tiga kota utama kokoh memimpin."
    ),
    caption = paste0(
      "● Kabupaten    ▲ Kota\n",
      "Sumber: BPS Provinsi Sulawesi Selatan; olahan tim."
    ),
    theme = theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        colour = palet_dasar["Aksen"]
      ),
      plot.subtitle = element_text(
        size = 8,
        colour = palet_dasar["Gelap"]
      ),
      plot.caption = element_text(
        size = 7,
        hjust = 0,
        colour = palet_dasar["Gelap"],
        margin = margin(t = 4)
      ),
      plot.margin = margin(5, 8, 3, 8)
    )
  )

print(grafik_5)
# ============================================================
# SIMPAN SEMUA GRAFIK
# ============================================================

simpan(grafik_1, "Grafik_1_Sebaran_IPM.png", 8, 6)
simpan(grafik_2, "Grafik_2_Tren_IPM.png", 15, 9)
simpan(grafik_3, "Grafik_3_IPM_Kemiskinan.png", 12, 5)
simpan(grafik_4, "Grafik_4_Peringkat_IPM.png", 14, 9)
simpan(grafik_5, "Grafik_5_Multipanel.png", 12, 8)
