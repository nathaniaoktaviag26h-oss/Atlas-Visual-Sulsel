# ============================================================
# 07_buat_profil.R
# Profil 24 Kabupaten/Kota Sulawesi Selatan
# ============================================================

# ------------------------------------------------------------
# 1. Package
# ------------------------------------------------------------

library(tidyverse)
library(patchwork)

# ------------------------------------------------------------
# 2. Theme dan palet tim
# ------------------------------------------------------------

source("R/04_theme_palet_tim.R")

# ------------------------------------------------------------
# 3. Parameter profil
# ------------------------------------------------------------

tahun_profil <- 2022:2024

indikator_utama <- "ipm"
indikator_pendamping <- "kemiskinan"

sumber_data <- "BPS Provinsi Sulawesi Selatan"

# ------------------------------------------------------------
# 4. Membaca data
# ------------------------------------------------------------

data_atlas <- readRDS(
  "data/data-bersih/atlas_sulsel.rds"
)

# ------------------------------------------------------------
# 5. Persiapan dan validasi data
# ------------------------------------------------------------

data_atlas <- data_atlas %>%
  filter(tahun %in% tahun_profil)

# Validasi jumlah wilayah
stopifnot(
  n_distinct(data_atlas$kabupaten_kota) == 24
)

# Validasi jumlah observasi
stopifnot(
  nrow(data_atlas) == 72
)

# Validasi periode
stopifnot(
  all(tahun_profil %in% unique(data_atlas$tahun))
)

# ------------------------------------------------------------
# 6. Fungsi grafik tren
# ------------------------------------------------------------

plot_tren_profil <- function(
    data,
    indikator,
    judul,
    label_y,
    warna
) {
  
  ggplot(
    data,
    aes(
      x = tahun,
      y = {{ indikator }},
      group = 1
    )
  ) +
    
    geom_line(
      linewidth = 1,
      color = warna
    ) +
    
    geom_point(
      size = 3,
      color = warna
    ) +
    
    scale_x_continuous(
      breaks = tahun_profil
    ) +
    
    labs(
      title = judul,
      subtitle = paste0(
        "Periode ",
        min(tahun_profil),
        "–",
        max(tahun_profil)
      ),
      x = "Tahun",
      y = label_y,
      caption = paste(
        "Sumber:",
        sumber_data
      )
    ) +
    
    theme_tim()
}

# ------------------------------------------------------------
# 7. Fungsi membuat SATU profil kabupaten/kota
# ------------------------------------------------------------

profil_kab <- function(nama_wilayah) {
  
  # Ambil data satu wilayah
  data_wilayah <- data_atlas %>%
    filter(
      kabupaten_kota == nama_wilayah
    )
  
  # Validasi data wilayah
  if (nrow(data_wilayah) != length(tahun_profil)) {
    
    stop(
      paste0(
        "Data untuk ",
        nama_wilayah,
        " tidak lengkap. ",
        "Jumlah observasi: ",
        nrow(data_wilayah)
      )
    )
  }
  
  # ----------------------------------------------------------
  # Grafik IPM
  # ----------------------------------------------------------
  
  grafik_ipm <- plot_tren_profil(
    data = data_wilayah,
    indikator = ipm,
    judul = "Indeks Pembangunan Manusia (IPM)",
    label_y = "IPM",
    warna = "#1F4E79"
  )
  
  # ----------------------------------------------------------
  # Grafik Kemiskinan
  # ----------------------------------------------------------
  
  grafik_kemiskinan <- plot_tren_profil(
    data = data_wilayah,
    indikator = kemiskinan,
    judul = "Persentase Penduduk Miskin",
    label_y = "Kemiskinan (%)",
    warna = "#D95F02"
  )
  
  # ----------------------------------------------------------
  # Gabungkan kedua grafik
  # ----------------------------------------------------------
  
  profil <- grafik_ipm /
    grafik_kemiskinan +
    
    plot_annotation(
      title = paste(
        "Profil Statistik",
        nama_wilayah,
        "Sulawesi Selatan"
      ),
      subtitle = paste0(
        "Indikator pembangunan manusia dan kemiskinan | ",
        min(tahun_profil),
        "–",
        max(tahun_profil)
      )
    )
  
  return(profil)
}

# ------------------------------------------------------------
# 8. Fungsi membuat SEMUA 24 profil
# ------------------------------------------------------------

buat_semua_profil <- function() {
  
  # Membuat folder output
  dir.create(
    "keluaran/profil",
    showWarnings = FALSE,
    recursive = TRUE
  )
  
  # Daftar 24 kabupaten/kota
  daftar_wilayah <- data_atlas %>%
    distinct(kabupaten_kota) %>%
    arrange(kabupaten_kota) %>%
    pull(kabupaten_kota)
  
  # Validasi jumlah wilayah
  stopifnot(
    length(daftar_wilayah) == 24
  )
  
  # ----------------------------------------------------------
  # Membuat 24 profil dengan purrr::walk()
  # ----------------------------------------------------------
  
  purrr::walk(
    daftar_wilayah,
    function(wilayah) {
      
      message(
        "Membuat profil: ",
        wilayah
      )
      
      # Membuat profil
      profil <- profil_kab(wilayah)
      
      # Membuat nama file 
      nama_file <- wilayah %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(
          "[^a-z0-9]+",
          "_"
        ) %>%
        stringr::str_remove_all(
          "_$"
        )
      
      # Simpan profil
      ggsave(
        filename = file.path(
          "keluaran/profil",
          paste0(
            "profil_",
            nama_file,
            ".png"
          )
        ),
        plot = profil,
        width = 10,
        height = 8,
        dpi = 300
      )
    }
  )
  
  # Pesan selesai
  message(
    "SELESAI: ",
    length(daftar_wilayah),
    " profil kabupaten/kota telah dibuat."
  )
}

# ============================================================
# SELESAI
# ============================================================