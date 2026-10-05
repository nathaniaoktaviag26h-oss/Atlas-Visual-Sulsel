# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# 01 - PEMBERSIHAN DAN PENGGABUNGAN DATA
# Indikator:
#   1. Indeks Pembangunan Manusia (IPM)
#   2. Persentase Penduduk Miskin
# Periode: 2022-2024
# ============================================================


# ------------------------------------------------------------
# 1. MEMANGGIL PACKAGE
# ------------------------------------------------------------

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)


# ------------------------------------------------------------
# 2. MENENTUKAN LOKASI FOLDER
# ------------------------------------------------------------

folder_mentah <- "data/data-mentah"
folder_bersih <- "data/data-bersih"


# ------------------------------------------------------------
# 3. MEMBACA DATA IPM
# ------------------------------------------------------------

baca_ipm <- function(tahun) {
  
  file <- file.path(
    folder_mentah,
    paste0("IPM_", tahun, ".xlsx")
  )
  
  data <- read_excel(file)
  
  data <- data %>%
    rename(
      kabupaten_kota = `Kabupaten/Kota`,
      ipm = `Indeks Pembangunan Manusia`
    ) %>%
    mutate(
      kabupaten_kota = str_squish(kabupaten_kota),
      ipm = as.numeric(ipm),
      tahun = tahun
    ) %>%
    select(
      kabupaten_kota,
      tahun,
      ipm
    )
  
  return(data)
}


# Membaca IPM tahun 2022-2024
ipm <- map_dfr(
  2022:2024,
  baca_ipm
)


# ------------------------------------------------------------
# 4. MEMBACA DATA KEMISKINAN
# ------------------------------------------------------------

baca_kemiskinan <- function(tahun) {
  
  file <- file.path(
    folder_mentah,
    paste0("KEMISKINAN_", tahun, ".xlsx")
  )
  
  data <- read_excel(file)
  
  data <- data %>%
    rename(
      kabupaten_kota = `Kabupaten/Kota`,
      kemiskinan = `Kemiskinan`
    ) %>%
    mutate(
      kabupaten_kota = str_squish(kabupaten_kota),
      kemiskinan = as.numeric(kemiskinan),
      tahun = tahun
    ) %>%
    select(
      kabupaten_kota,
      tahun,
      kemiskinan
    )
  
  return(data)
}


# Membaca kemiskinan tahun 2022-2024
kemiskinan <- map_dfr(
  2022:2024,
  baca_kemiskinan
)


# ------------------------------------------------------------
# 5. MENYAMAKAN NAMA KABUPATEN/KOTA
# ------------------------------------------------------------

ipm <- ipm %>%
  mutate(
    kabupaten_kota = str_to_lower(str_squish(kabupaten_kota))
  )

kemiskinan <- kemiskinan %>%
  mutate(
    kabupaten_kota = str_to_lower(str_squish(kabupaten_kota))
  )


# ------------------------------------------------------------
# 6A. MENGGABUNGKAN IPM DAN KEMISKINAN
# ------------------------------------------------------------

data_bersih <- ipm %>%
  left_join(
    kemiskinan,
    by = c("kabupaten_kota", "tahun")
  ) %>%
  arrange(
    tahun,
    kabupaten_kota
  )

# ------------------------------------------------------------

# 6B. MENAMBAHKAN KODE WILAYAH
# ------------------------------------------------------------

kode_wilayah <- read_excel(
  "data/data-kode-wilayah/KODE WILAYAH SULSEL.xlsx"
)

kode_wilayah <- kode_wilayah %>%
  rename(
    kode_wilayah = `Kode Kab/Kota`,
    kabupaten_kota = `Kabupaten/Kota`
  ) %>%
  mutate(
    kode_wilayah = as.character(kode_wilayah),
    kabupaten_kota = str_to_lower(str_squish(kabupaten_kota))
  )

data_bersih <- data_bersih %>%
  left_join(
    kode_wilayah,
    by = "kabupaten_kota"
  ) %>%
  select(
    kode_wilayah,
    kabupaten_kota,
    tahun,
    ipm,
    kemiskinan
  )


# ------------------------------------------------------------

# ------------------------------------------------------------
# 7. CEK HASIL DATA
# ------------------------------------------------------------

print(data_bersih)

cat("\nJumlah baris:", nrow(data_bersih), "\n")
cat("Jumlah wilayah:", n_distinct(data_bersih$kabupaten_kota), "\n")
cat("Jumlah tahun:", n_distinct(data_bersih$tahun), "\n")


# ------------------------------------------------------------
# 8. CEK DATA KOSONG
# ------------------------------------------------------------

cat("\nJumlah NA setiap variabel:\n")

print(
  colSums(is.na(data_bersih))
)


# ------------------------------------------------------------
# 9. CEK DUPLIKASI
# ------------------------------------------------------------

duplikat <- data_bersih %>%
  count(
    kabupaten_kota,
    tahun
  ) %>%
  filter(n > 1)

cat("\nJumlah kombinasi wilayah-tahun yang duplikat:",
    nrow(duplikat), "\n")

if (nrow(duplikat) > 0) {
  print(duplikat)
}


# ------------------------------------------------------------
# 10. CEK JUMLAH WILAYAH PER TAHUN
# ------------------------------------------------------------

cek_wilayah <- data_bersih %>%
  count(tahun)

cat("\nJumlah wilayah setiap tahun:\n")

print(cek_wilayah)


# ------------------------------------------------------------
# 11. SIMPAN DATA BERSIH
# ------------------------------------------------------------

saveRDS(
  data_bersih,
  file.path(
    folder_bersih,
    "atlas_sulsel.rds"
  )
)

write.csv(
  data_bersih,
  file.path(
    folder_bersih,
    "atlas_sulsel.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 12. PESAN AKHIR
# ------------------------------------------------------------

cat("\n========================================\n")
cat("PEMBERSIHAN DATA SELESAI\n")
cat("========================================\n")
cat("Data tersimpan di:\n")
cat("data/data-bersih/atlas_sulsel.rds\n")
cat("data/data-bersih/atlas_sulsel.csv\n")


