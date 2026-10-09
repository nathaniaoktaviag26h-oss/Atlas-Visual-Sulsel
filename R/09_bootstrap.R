# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# BOOTSTRAP 95% DAN GRAFIK KETIDAKPASTIAN
# Indikator : IPM dan Kemiskinan
# Periode   : 2022-2024
# Replikasi : B = 1.999
# ============================================================

library(tidyverse)

source("R/04_theme_palet_tim.R")

# ============================================================
# 1. MEMBACA DATA
# ============================================================

data_bersih <- readRDS("data/data-bersih/atlas_sulsel.rds")

stopifnot(
  all(c("tahun", "kabupaten_kota", "ipm", "kemiskinan") %in%
        names(data_bersih))
)

data_ipm <- data_bersih %>%
  filter(tahun %in% 2022:2024) %>%
  select(kabupaten_kota, tahun, ipm) %>%
  drop_na()

data_kemiskinan <- data_bersih %>%
  filter(tahun %in% 2022:2024) %>%
  select(kabupaten_kota, tahun, kemiskinan) %>%
  drop_na()

# Periksa satu baris per wilayah per tahun
if (anyDuplicated(data_ipm[c("kabupaten_kota", "tahun")]) > 0 ||
    anyDuplicated(data_kemiskinan[c("kabupaten_kota", "tahun")]) > 0) {
  stop("Terdapat duplikasi wilayah-tahun.")
}

# Pastikan data lengkap untuk masing-masing indikator
cek_lengkap <- function(data, kolom_nilai) {
  data %>%
    count(kabupaten_kota, name = "n_tahun") %>%
    filter(n_tahun != 3)
}

if (nrow(cek_lengkap(data_ipm, "ipm")) > 0 ||
    nrow(cek_lengkap(data_kemiskinan, "kemiskinan")) > 0) {
  stop("Periksa data: setiap wilayah harus memiliki tiga tahun lengkap.")
}

# ============================================================
# 2. FUNGSI BOOTSTRAP
# ============================================================

hitung_bootstrap <- function(x, B = 1999, conf = 0.95) {
  
  x <- x[is.finite(x)]
  
  if (length(x) < 2) {
    stop("Minimal dua pengamatan valid diperlukan.")
  }
  
  if (B < 1999 || conf <= 0 || conf >= 1) {
    stop("Periksa nilai B dan conf.")
  }
  
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
# 3. BOOTSTRAP IPM SAJA
# ============================================================

set.seed(123)

hasil_ipm <- data_ipm %>%
  group_by(kabupaten_kota) %>%
  summarise(
    hasil = list(hitung_bootstrap(ipm, B = 1999)),
    .groups = "drop"
  ) %>%
  unnest(hasil) %>%
  arrange(estimasi) %>%
  mutate(
    nama_wilayah = factor(
      as.character(kabupaten_kota),
      levels = unique(as.character(kabupaten_kota))
    )
  )

# ============================================================
# 4. GRAFIK BOOTSTRAP IPM
# ============================================================

grafik_ipm <- ggplot(
  hasil_ipm,
  aes(x = estimasi, y = nama_wilayah)
) +
  geom_errorbar(
    aes(xmin = batas_bawah, xmax = batas_atas),
    orientation = "y",
    width = 0.18,
    linewidth = 0.8,
    colour = unname(palet_dasar["Netral"])
  ) +
  geom_point(
    size = 2.8,
    colour = unname(palet_indikator["IPM"])
  ) +
  labs(
    title = "Ketidakpastian Estimasi IPM",
    subtitle = "Rata-rata 2022-2024 | Bootstrap 95% | B = 1.999",
    x = "Rata-rata Indeks Pembangunan Manusia (IPM)",
    y = NULL,
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
# 5. BOOTSTRAP KEMISKINAN SAJA
# ============================================================

set.seed(456)

hasil_kemiskinan <- data_kemiskinan %>%
  group_by(kabupaten_kota) %>%
  summarise(
    hasil = list(hitung_bootstrap(kemiskinan, B = 1999)),
    .groups = "drop"
  ) %>%
  unnest(hasil) %>%
  arrange(estimasi) %>%
  mutate(
    nama_wilayah = factor(
      as.character(kabupaten_kota),
      levels = unique(as.character(kabupaten_kota))
    )
  )

# ============================================================
# 6. GRAFIK BOOTSTRAP KEMISKINAN
# ============================================================

grafik_kemiskinan <- ggplot(
  hasil_kemiskinan,
  aes(x = estimasi, y = nama_wilayah)
) +
  geom_errorbar(
    aes(xmin = batas_bawah, xmax = batas_atas),
    orientation = "y",
    width = 0.18,
    linewidth = 0.8,
    colour = unname(palet_dasar["Netral"])
  ) +
  geom_point(
    size = 2.8,
    colour = unname(palet_indikator["Kemiskinan"])
  ) +
  labs(
    title = "Ketidakpastian Estimasi Kemiskinan",
    subtitle = "Rata-rata 2022-2024 | Bootstrap 95% | B = 1.999",
    x = "Rata-rata Persentase Penduduk Miskin (%)",
    y = NULL,
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
# 7. MENYIMPAN HASIL SECARA TERPISAH
# ============================================================

dir.create(
  "keluaran/bootstrap",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  hasil_ipm %>%
    mutate(nama_wilayah = as.character(nama_wilayah)),
  "keluaran/bootstrap/hasil_bootstrap_ipm_2022_2024.csv"
)

write_csv(
  hasil_kemiskinan %>%
    mutate(nama_wilayah = as.character(nama_wilayah)),
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
