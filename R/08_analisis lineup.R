# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# CP3 - ANALISIS HASIL LINEUP
# Poin 63-64
# ============================================================

library(tidyverse)

# 1. FILE ----------------------------------------------------

file_jawaban <- "data/jawaban_lineup.csv"
file_kunci   <- "keluaran/kunci_lineup/kunci.rds"
folder_out   <- "keluaran/analisis_lineup"

# 2. BACA DATA ------------------------------------------------

jawaban <- read_csv(
  file_jawaban,
  show_col_types = FALSE
)

# 3. IDENTIFIKASI KOLOM --------------------------------------

kolom_panel <- names(jawaban)[
  str_detect(
    names(jawaban),
    regex("Penel nomor berapa", ignore_case = TRUE)
  )
]

if (length(kolom_panel) != 1) {
  stop("Kolom pilihan panel tidak ditemukan.")
}

kolom_alasan <- setdiff(
  names(jawaban)[
    str_detect(
      names(jawaban),
      regex("alasan|mengapa|kenapa|berbeda", ignore_case = TRUE)
    )
  ],
  kolom_panel
)

if (length(kolom_alasan) == 0) {
  kolom_alasan <- setdiff(names(jawaban), kolom_panel)
  kolom_alasan <- kolom_alasan[length(kolom_alasan)]
}

# 4. SIAPKAN JAWABAN ------------------------------------------

jawaban_lineup <- jawaban %>%
  mutate(
    pilihan_panel = parse_number(.data[[kolom_panel]])
  )

if (anyNA(jawaban_lineup$pilihan_panel)) {
  stop("Ada pilihan panel yang tidak terbaca.")
}

# 5. PARAMETER LINEUP -----------------------------------------

K <- nrow(jawaban_lineup)
jumlah_panel <- 20
peluang_acak <- 1 / jumlah_panel
kunci <- as.numeric(readRDS(file_kunci))[1]

# 6. JUMLAH BENAR DAN P-VALUE -------------------------------

x <- sum(jawaban_lineup$pilihan_panel == kunci)

# H0: pengamat tidak dapat membedakan panel asli.
# X ~ Binomial(K, 1/20)

p_value_visual <- pbinom(
  x - 1,
  size = K,
  prob = peluang_acak,
  lower.tail = FALSE
)

hasil_lineup <- tibble(
  K = K,
  jumlah_panel = jumlah_panel,
  panel_asli = kunci,
  jumlah_benar_x = x,
  peluang_acak = peluang_acak,
  p_value_visual = p_value_visual
)

# 7. DISTRIBUSI PILIHAN PANEL -------------------------------

distribusi_panel <- jawaban_lineup %>%
  count(
    pilihan_panel,
    name = "jumlah_pengamat"
  ) %>%
  arrange(desc(jumlah_pengamat))

# 8. HASIL SETIAP PENGAMAT -----------------------------------

jawaban_hasil <- jawaban_lineup %>%
  mutate(
    benar = pilihan_panel == kunci
  )

# 9. INTERPRETASI HASIL ---------------------------------------

interpretasi <- if (p_value_visual < 0.05) {
  paste0(
    "Dari ", K, " pengamat, sebanyak ", x,
    " pengamat berhasil mengidentifikasi panel data asli. ",
    "Pada lineup dengan ", jumlah_panel,
    " panel, peluang memilih panel asli secara acak adalah 1/",
    jumlah_panel, ". Visual p-value sebesar ",
    format.pval(p_value_visual, digits = 4),
    ". Karena p-value < 0,05, terdapat bukti bahwa panel ",
    "data asli dapat dibedakan dari panel hasil permutasi ",
    "lebih baik daripada yang diharapkan secara acak."
  )
} else {
  paste0(
    "Dari ", K, " pengamat, sebanyak ", x,
    " pengamat berhasil mengidentifikasi panel data asli. ",
    "Pada lineup dengan ", jumlah_panel,
    " panel, peluang memilih panel asli secara acak adalah 1/",
    jumlah_panel, ". Visual p-value sebesar ",
    format.pval(p_value_visual, digits = 4),
    ". Karena p-value >= 0,05, belum terdapat bukti yang ",
    "cukup bahwa panel data asli dapat dibedakan dari panel ",
    "hasil permutasi lebih baik daripada yang diharapkan ",
    "secara acak."
  )
}

# 10. ALASAN PENGAMAT ----------------------------------------

alasan_pengamat <- jawaban %>%
  transmute(
    alasan = .data[[kolom_alasan]]
  ) %>%
  filter(
    !is.na(alasan),
    str_trim(alasan) != ""
  )

ringkasan_alasan <- if (nrow(alasan_pengamat) > 0) {
  paste(
    paste0(
      seq_len(nrow(alasan_pengamat)),
      ". ",
      alasan_pengamat$alasan
    ),
    collapse = "\n"
  )
} else {
  "Tidak terdapat alasan pengamat yang dapat diidentifikasi."
}

# 11. SIMPAN HASIL -------------------------------------------

dir.create(
  folder_out,
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  hasil_lineup,
  file.path(folder_out, "hasil_lineup.csv")
)

write_csv(
  distribusi_panel,
  file.path(folder_out, "distribusi_panel.csv")
)

write_csv(
  jawaban_hasil,
  file.path(folder_out, "jawaban_dengan_kunci.csv")
)

write_csv(
  alasan_pengamat,
  file.path(folder_out, "alasan_pengamat.csv")
)

writeLines(
  interpretasi,
  file.path(folder_out, "interpretasi_lineup.txt")
)

writeLines(
  ringkasan_alasan,
  file.path(folder_out, "ringkasan_alasan_pengamat.txt")
)

# 12. HASIL DI CONSOLE ---------------------------------------

cat("\n=== HASIL LINEUP ===\n")
cat("K =", K, "\n")
cat("Panel asli =", kunci, "\n")
cat("Jumlah benar (x) =", x, "\n")
cat("Visual p-value =", format.pval(p_value_visual, digits = 5), "\n")

cat("\n=== INTERPRETASI ===\n")
cat(interpretasi, "\n")

cat("\n=== ALASAN PENGAMAT ===\n")
cat(ringkasan_alasan, "\n")

cat("\nHasil tersimpan di:", folder_out, "\n")