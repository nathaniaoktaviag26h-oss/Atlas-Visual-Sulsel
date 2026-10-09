# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# BOOTSTRAP 95% CI DAN GRAFIK KETIDAKPASTIAN
# Indikator : IPM
# Periode   : 2022-2024
# Replikasi : B = 1.999
# Unit      : Nilai IPM tahunan dalam setiap kabupaten/kota
# Grafik    : Titik + garis galat, diurutkan berdasarkan rataan
# ============================================================

library(tidyverse)

# ============================================================
# 1. MEMUAT TEMA DAN PALET
# ============================================================

source("R/04_theme_palet_tim.R")

# ============================================================
# 2. MEMBACA DATA
# ============================================================

data_bersih <- readRDS("data/data-bersih/atlas_sulsel.rds")

stopifnot(
  all(c("tahun", "kabupaten_kota", "ipm") %in% names(data_bersih))
)

# ============================================================
# 3. MENYIAPKAN DATA IPM 2022-2024
# ============================================================

data_ipm <- data_bersih %>%
  filter(tahun %in% 2022:2024) %>%
  select(kabupaten_kota, tahun, ipm) %>%
  filter(
    !is.na(kabupaten_kota),
    !is.na(tahun),
    !is.na(ipm)
  )

# Pastikan satu nilai per wilayah per tahun
if (anyDuplicated(data_ipm[c("kabupaten_kota", "tahun")]) > 0) {
  stop(
    "Terdapat duplikasi wilayah-tahun. ",
    "Periksa data sebelum melakukan bootstrap."
  )
}

# Pastikan tersedia 24 wilayah dengan tiga tahun lengkap
cek_data <- data_ipm %>%
  count(kabupaten_kota, name = "jumlah_tahun")

if (
  nrow(cek_data) != 24 ||
  any(cek_data$jumlah_tahun != 3) ||
  !all(2022:2024 %in% data_ipm$tahun)
) {
  stop(
    "Pastikan terdapat 24 kabupaten/kota ",
    "dengan data IPM lengkap tahun 2022-2024."
  )
}

# ============================================================
# 4. FUNGSI BOOTSTRAP PERSENTIL
# ============================================================

hitung_bootstrap <- function(x, B = 1999, conf = 0.95) {
  
  # Validasi input
  if (length(x) < 2 || any(!is.finite(x))) {
    stop("Data harus memiliki minimal dua nilai numerik yang valid.")
  }
  
  if (B < 1999) {
    stop("Jumlah replikasi bootstrap minimal 1.999.")
  }
  
  if (conf <= 0 || conf >= 1) {
    stop("conf harus berada di antara 0 dan 1.")
  }
  
  n <- length(x)
  
  # Resampling dengan pengembalian
  hasil_boot <- replicate(
    B,
    mean(sample(x, size = n, replace = TRUE))
  )
  
  alpha <- 1 - conf
  
  tibble(
    estimasi = mean(x),
    batas_bawah = unname(
      quantile(hasil_boot, probs = alpha / 2)
    ),
    batas_atas = unname(
      quantile(hasil_boot, probs = 1 - alpha / 2)
    ),
    jumlah_pengamatan = n,
    B = B,
    tingkat_kepercayaan = conf
  )
}

# ============================================================
# 5. MENGHITUNG BOOTSTRAP UNTUK SETIAP WILAYAH
# ============================================================

set.seed(123)

hasil_bootstrap <- data_ipm %>%
  group_by(kabupaten_kota) %>%
  summarise(
    hasil = list(
      hitung_bootstrap(
        x = ipm,
        B = 1999,
        conf = 0.95
      )
    ),
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

# Menampilkan hasil perhitungan
print(hasil_bootstrap)

# ============================================================
# 6. GRAFIK TITIK + GARIS GALAT
# ============================================================

grafik_bootstrap <- ggplot(
  hasil_bootstrap,
  aes(x = estimasi, y = kabupaten_kota)
) +
  geom_errorbar(
    aes(
      xmin = batas_bawah,
      xmax = batas_atas
    ),
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
    subtitle = paste0(
      "Rata-rata 2022-2024 | Interval kepercayaan 95%",
      " | Bootstrap B = 1.999"
    ),
    x = "Rata-rata Indeks Pembangunan Manusia (IPM)",
    y = NULL,
    caption = paste(
      "Sumber: BPS Provinsi Sulawesi Selatan | Diolah.",
      "Interval bootstrap berdasarkan tiga nilai tahunan per wilayah;",
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
# 7. MENYIMPAN TABEL DAN GRAFIK
# ============================================================

dir.create(
  "keluaran/bootstrap",
  recursive = TRUE,
  showWarnings = FALSE
)

# Simpan tabel hasil bootstrap
write_csv(
  hasil_bootstrap %>%
    mutate(
      kabupaten_kota = as.character(kabupaten_kota)
    ),
  "keluaran/bootstrap/hasil_bootstrap_ipm_2022_2024.csv"
)

# Simpan grafik resolusi tinggi
ggsave(
  filename = "keluaran/bootstrap/grafik_bootstrap_ipm_2022_2024.png",
  plot = grafik_bootstrap,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
