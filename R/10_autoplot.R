# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# CP3: AUTOPLOT() DAN METODE S3
# Indikator : IPM dan Kemiskinan
# Periode   : 2022-2024
# ============================================================

library(tidyverse)
library(ggplot2)

source("R/04_theme_palet_tim.R")
source("R/09_bootstrap.R")

# ============================================================
# 1. MEMERIKSA HASIL BOOTSTRAP
# ============================================================

if (!exists("hasil_ipm") || !exists("hasil_kemiskinan")) {
  stop(
    "Objek hasil bootstrap belum tersedia. ",
    "Jalankan source('R/09_bootstrap.R') terlebih dahulu."
  )
}

# ============================================================
# 2. MEMBUAT OBJEK ANALISIS KELAS atlas_fit
# ============================================================

new_atlas_fit <- function(hasil, indikator) {
  
  kolom_wajib <- c(
    "kabupaten_kota",
    "estimasi",
    "batas_bawah",
    "batas_atas",
    "B"
  )
  
  if (!all(kolom_wajib %in% names(hasil))) {
    stop("Kolom hasil bootstrap tidak lengkap.")
  }
  
  if (length(indikator) != 1 ||
      !indikator %in% c("IPM", "Kemiskinan")) {
    stop("Indikator harus IPM atau Kemiskinan.")
  }
  
  hasil <- hasil %>%
    filter(
      is.finite(estimasi),
      is.finite(batas_bawah),
      is.finite(batas_atas)
    ) %>%
    arrange(estimasi)
  
  if (nrow(hasil) == 0) {
    stop("Tidak ada hasil bootstrap yang valid.")
  }
  
  struktur <- list(
    hasil = hasil,
    indikator = indikator,
    periode = "2022-2024"
  )
  
  class(struktur) <- "atlas_fit"
  
  struktur
}

# ============================================================
# 3. METODE PRINT S3
# ============================================================

print.atlas_fit <- function(x, ...) {
  
  cat("Atlas Visual Sulawesi Selatan\n")
  cat("Kelas objek :", class(x)[1], "\n")
  cat("Indikator   :", x$indikator, "\n")
  cat("Periode     :", x$periode, "\n")
  cat("Jumlah wilayah:", nrow(x$hasil), "\n\n")
  
  print(
    x$hasil %>%
      select(
        kabupaten_kota,
        estimasi,
        batas_bawah,
        batas_atas,
        B
      )
  )
  
  invisible(x)
}

# ============================================================
# 4. METODE AUTOPLOT S3
# Titik dan garis galat memakai warna yang sama
# ============================================================

autoplot.atlas_fit <- function(object, ...) {
  
  hasil_plot <- object$hasil %>%
    mutate(
      nama_wilayah = as.character(kabupaten_kota)
    ) %>%
    arrange(estimasi) %>%
    mutate(
      nama_wilayah = factor(
        nama_wilayah,
        levels = unique(nama_wilayah)
      )
    )
  
  # Mengambil warna langsung dari palet proyek
  warna_titik <- unname(
    palet_indikator[object$indikator]
  )
  
  if (length(warna_titik) != 1 || is.na(warna_titik)) {
    stop("Warna indikator tidak ditemukan pada palet proyek.")
  }
  
  label_x <- if (object$indikator == "IPM") {
    "Rata-rata Indeks Pembangunan Manusia (IPM)"
  } else {
    "Rata-rata Persentase Penduduk Miskin (%)"
  }
  
  ggplot(
    hasil_plot,
    aes(x = estimasi, y = nama_wilayah)
  ) +
    
    # Garis galat berwarna sama dengan titik
    geom_errorbar(
      aes(
        xmin = batas_bawah,
        xmax = batas_atas
      ),
      orientation = "y",
      width = 0.18,
      linewidth = 0.8,
      colour = warna_titik
    ) +
    
    # Titik estimasi
    geom_point(
      shape = 16,
      size = 2.8,
      colour = warna_titik
    ) +
    
    labs(
      title = paste(
        "Ketidakpastian Estimasi",
        object$indikator
      ),
      subtitle = paste0(
        "Rata-rata ", object$periode,
        " | Interval kepercayaan 95% | B = 1.999"
      ),
      x = label_x,
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
        colour = unname(palet_dasar["Gelap"])
      ),
      
      axis.text.y = element_text(size = 8),
      
      panel.grid.major.x = element_line(
        colour = unname(palet_dasar["Netral"]),
        linewidth = 0.35
      ),
      
      panel.grid.major.y = element_line(
        colour = unname(palet_dasar["Netral"]),
        linewidth = 0.25,
        linetype = "dotted"
      ),
      
      panel.grid.minor = element_blank(),
      
      plot.caption = element_text(hjust = 0)
    )
}

# ============================================================
# 5. MEMBUAT OBJEK ANALISIS
# ============================================================

fit_ipm <- new_atlas_fit(
  hasil = hasil_ipm,
  indikator = "IPM"
)

fit_kemiskinan <- new_atlas_fit(
  hasil = hasil_kemiskinan,
  indikator = "Kemiskinan"
)

# ============================================================
# 6. MEMERIKSA DISPATCH S3
# ============================================================

print(fit_ipm)
print(fit_kemiskinan)

# Pastikan metode tersedia
print(getS3method("autoplot", "atlas_fit"))

# ============================================================
# 7. MEMBUAT GRAFIK AUTOPLOT SECARA TERPISAH
# ============================================================

grafik_autoplot_ipm <- autoplot(fit_ipm)

grafik_autoplot_kemiskinan <- autoplot(fit_kemiskinan)

print(grafik_autoplot_ipm)
print(grafik_autoplot_kemiskinan)

# ============================================================
# 8. MENYIMPAN GRAFIK
# ============================================================

dir.create(
  "keluaran/bootstrap",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = "keluaran/bootstrap/autoplot_ipm_2022_2024.png",
  plot = grafik_autoplot_ipm,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "keluaran/bootstrap/autoplot_kemiskinan_2022_2024.png",
  plot = grafik_autoplot_kemiskinan,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
#Uji
autoplot.atlas_fit(fit_kemiskinan)

