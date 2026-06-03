# TÜİK Havayolu Yolcu Trafiği Zaman Serisi Analizi ve Öngörü Projesi

Marmara Üniversitesi Yönetim Bilişim Sistemleri (MIS) - Zaman Serisi Analizi ve Gelecek Dönem Tahmin Projesi.

## 📌 Proje Hakkında
Bu proje, Türkiye İstatistik Kurumu (TÜİK) tarafından yayımlanan **Aylık Toplam Havayolu Yolcu Taşımacılığı** (Ocak 2022 - Aralık 2025) verilerini kullanarak zaman serisi modellemesi ve öngörü analizleri gerçekleştirmektedir. Projenin temel amacı; serideki trend ve mevsimsel (seasonality) dalgalanmaları analiz etmek, farklı tahmin modellerinin doğruluk oranlarını karşılaştırmak ve **Ocak 2026** dönemi için nokta atışı bir öngörü üretmektir.

## 🗂️ Proje Yapısı (Directory Tree)
Proje, hiyerarşik ve reproducibility (tekrar üretilebilirlik) standartlarına tam uyumlu olarak kurgulanmıştır:

```text
tuik-forecasting-project/
│
├── final_report.R          # Tüm veri analizi, modelleme ve dokümantasyon kodları
├── final_report.html       # Çıktı olarak üretilen dinamik ve interaktif HTML raporu
├── README.md               # Proje genel dökümantasyonu ve kılavuzu
│
└── outputs/
    └── figures/            # Modellerin performans ve analiz grafiklerinin (.png) dizini
        ├── actual_series_plot.png
        ├── naive_forecast_plot.png
        ├── moving_average_plot.png
        ├── weighted_moving_average_plot.png
        ├── regression_forecast_plot.png
        └── exponential_smoothing_plot.png
