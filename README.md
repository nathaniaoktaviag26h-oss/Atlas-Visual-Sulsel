# Sulawesi Selatan dalam Angka Visual

**Atlas Visual Sulawesi Selatan · Tim 1 · Komputasi Statistika Lanjut**

## Anggota Tim
- **NATHANIA OKTAVIA GUNAWAN** — H062261002
- **DHIYA IZDIHAR HARYADI** — H062261003

## 1. Tentang Proyek

Proyek ini menyajikan atlas visual **Indeks Pembangunan Manusia (IPM)** dan **persentase penduduk miskin** pada 24 kabupaten/kota di Provinsi Sulawesi Selatan selama 2022–2024. Analisis mencakup pembersihan data, peta tematik, makeover dan galeri grafik, profil wilayah, inferensi visual menggunakan *lineup*, serta interval bootstrap.

IPM dipilih sebagai indikator utama untuk menggambarkan capaian pembangunan manusia, sedangkan persentase penduduk miskin memberi konteks kesejahteraan ekonomi. Hubungan kedua indikator dibaca secara deskriptif dan tidak dianggap sebagai bukti sebab-akibat.

## 2. Data dan Sumber

- **IPM dan kemiskinan:** publikasi BPS Provinsi Sulawesi Selatan.
- **Batas administratif:** geoBoundaries ADM2. Informasi sitasi dan penggunaan tersedia di `data/data-batas-peta/CITATION-AND-USE-geoBoundaries.txt`.
- **Kode wilayah:** `data/data-kode-wilayah/KODE WILAYAH SULSEL.xlsx`.
- **Cakupan:** 24 kabupaten/kota, 2022–2024; total 72 observasi wilayah-tahun.
- **Data bersih:** `data/data-bersih/atlas_sulsel.csv` dan `data/data-bersih/atlas_sulsel.rds`.

## 3. Struktur Repositori

```text
Atlas-Visual-Sulsel/
├── Atlas-Visual-Sulsel.Rproj
├── README.md
├── _quarto.yml
├── atlas.qmd
├── atlas-style1.css
├── infografis.pdf
├── data/
│   ├── data-mentah/
│   ├── data-bersih/
│   ├── data-batas-peta/
│   ├── data-kode-wilayah/
│   └── jawaban_lineup.csv
├── R/
│   ├── 01_membersihkan_data.R
│   ├── 02_menyiapkan_peta.R
│   ├── 03_makeover grafik.R
│   ├── 04_theme_palet_tim.R
│   ├── 05_galeri.R
│   ├── 06_fungsi grafik dan Lineup.R
│   ├── 07_buat_profil.R
│   ├── 08_analisis lineup.R
│   ├── 09_bootstrap.R
│   ├── 10_autoplot.R
│   └── 11_infografis_kebijakan.R
└── keluaran/
    ├── analisis_lineup/
    ├── bootstrap/
    ├── profil/
    ├── kunci_lineup/
    ├── lineup-20 panel.png
    ├── peta_ipm_2022_2024.png
    ├── peta_kemiskinan_2022_2024.png
    └── ... (grafik lainnya)
```

## 4. Alur Analisis

| Script | Fungsi |
|---|---|
| `R/01_membersihkan_data.R` | Membersihkan dan menggabungkan data |
| `R/02_menyiapkan_peta.R` | Menyiapkan data spasial dan peta |
| `R/03_makeover grafik.R` | Membandingkan desain grafik sebelum dan sesudah perbaikan |
| `R/04_theme_palet_tim.R` | Menyediakan tema dan palet warna |
| `R/05_galeri.R` | Menghasilkan galeri grafik eksploratif |
| `R/06_fungsi grafik dan Lineup.R` | Menyediakan fungsi terkait grafik dan lineup |
| `R/07_buat_profil.R` | Membuat profil visual 24 kabupaten/kota |
| `R/08_analisis lineup.R` | Menganalisis jawaban lineup |
| `R/09_bootstrap.R` | Menghitung estimasi dan interval bootstrap 95% (`B = 1.999`) |
| `R/10_autoplot.R` | Membentuk objek S3, membuat grafik estimasi dan interval, serta menyimpan hasil |
| `R/11_infografis_kebijakan.R` | Membuat infografis kebijakan satu halaman dalam PDF |

Keluaran disimpan dalam subfolder `keluaran/` sesuai jenis analisis. Script dijalankan dari direktori utama proyek agar path relatif dapat ditemukan.

## 5. Menjalankan Proyek

1. Buka `Atlas-Visual-Sulsel.Rproj` di RStudio.
2. Pastikan paket yang digunakan oleh script sudah terpasang.
3. Untuk membangun ulang hasil dari data mentah, jalankan script sesuai urutan tabel di atas. Jalankan satu per satu dan periksa Console karena beberapa script menggunakan keluaran dari tahap sebelumnya.
4. `R/10_autoplot.R` juga menjalankan proses bootstrap. Jika sudah menjalankan `R/09_bootstrap.R`, perhatikan bahwa bootstrap dapat dijalankan lagi saat `R/10_autoplot.R` di-*source*.
5. Untuk membuat infografis, jalankan:

   ```r
   source("R/11_infografis_kebijakan.R")
   ```

   Dengan `folder_simpan <- "."`, PDF disimpan sebagai `infografis.pdf` di direktori utama proyek.

6. Untuk merender atlas, klik **Render** pada `atlas.qmd` atau jalankan:

   ```bash
   quarto render atlas.qmd
   ```

   Konfigurasi `_quarto.yml` mengatur hasil HTML ke `docs/`, dengan gaya dari `atlas-style1.css`. Pastikan gambar yang dirujuk oleh dokumen telah tersedia di `keluaran/`.

**Catatan:** nama `R/03_makeover grafik.R` dan `R/06_fungsi grafik dan Lineup.R` mengandung spasi. Gunakan nama berkas persis seperti yang tercantum.

## 6. Visualisasi dan Keterbatasan Interpretasi

Peta memperlihatkan variasi spasial; grafik dan profil wilayah membantu membandingkan nilai antardaerah dan antarwaktu; *lineup* membandingkan pola data nyata dengan panel pembanding; sedangkan bootstrap memberikan interval estimasi 95%.

Data hanya mencakup tiga tahun dan tiga nilai tahunan per wilayah. Karena itu, hasil bootstrap perlu ditafsirkan secara eksploratif sesuai skema resampling, bukan sebagai ukuran presisi tren jangka panjang. Hasil *lineup* juga dipengaruhi rancangan panel, jumlah pengamat, dan prosedur penilaian. Grafik korelasi atau tren tidak membuktikan hubungan kausal.

## 7. Dokumentasi Fungsi S3

Sistem S3 di R memungkinkan fungsi generik, seperti `print()` dan `autoplot()`, menjalankan metode sesuai kelas objek. Pada proyek ini, kelas objek analisis adalah `atlas_fit`; fungsi dan metodenya didefinisikan di `R/10_autoplot.R`.

### `new_atlas_fit(hasil, indikator)`
Membentuk objek kelas `atlas_fit`.

- `hasil`: data frame hasil bootstrap dengan kolom `kabupaten_kota`, `estimasi`, `batas_bawah`, `batas_atas`, dan `B`.
- `indikator`: `"IPM"` atau `"Kemiskinan"`.
- **Keluaran:** list berisi hasil yang telah difilter/diurutkan, nama indikator, periode `"2022-2024"`, dan kelas `"atlas_fit"`.
- Fungsi menghentikan proses jika kolom wajib atau indikator tidak valid, atau tidak ada baris hasil yang valid.

### `print.atlas_fit(x, ...)`
Metode S3 yang dipanggil oleh `print()` untuk menampilkan ringkasan objek.

- `x`: objek kelas `atlas_fit`.
- `...`: argumen tambahan untuk kompatibilitas dengan generik `print()`.
- **Keluaran:** ringkasan proyek, indikator, periode, jumlah wilayah, estimasi, dan interval.

### `autoplot.atlas_fit(object, ...)`
Metode S3 untuk memvisualisasikan estimasi dan interval.

- `object`: objek kelas `atlas_fit`.
- `...`: argumen tambahan.
- **Keluaran:** objek grafik `ggplot` berisi titik estimasi dan garis interval dengan tema/palet proyek.

### Contoh penggunaan

`R/10_autoplot.R` membuat objek `fit_ipm` dan `fit_kemiskinan`. Namun, menjalankan `source()` pada script ini akan menjalankan seluruh isi script, bukan hanya memuat definisi fungsi.

```r
source("R/10_autoplot.R")

print(fit_ipm)
print(fit_kemiskinan)

grafik_ipm <- autoplot(fit_ipm)
grafik_kemiskinan <- autoplot(fit_kemiskinan)

print(grafik_ipm)
print(grafik_kemiskinan)
```

README ini menjelaskan penggunaan fungsi. Jika tugas meminta dokumentasi bantuan formal untuk paket R, dokumentasi `roxygen2` dan berkas `man/*.Rd` perlu dibuat terpisah.

## 8. Reproduksibilitas dan Deklarasi Penggunaan AI

Untuk mereproduksi hasil, gunakan data dan script dari versi repositori yang sama, jalankan dari direktori utama, pastikan paket R dan Quarto tersedia, dan periksa pesan Console serta file keluaran. Perubahan data, kode, parameter, atau versi paket dapat menghasilkan keluaran berbeda.

AI generatif digunakan untuk membantu penyuntingan bahasa, penyusunan dokumentasi, dan dukungan teknis. Tim tetap bertanggung jawab memeriksa sumber data, memvalidasi angka dan interpretasi, meninjau kode, serta menyetujui isi akhir atlas.
