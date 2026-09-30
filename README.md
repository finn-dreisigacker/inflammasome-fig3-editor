# Fig. 3 – Inflammasom-Kinetik · Editor

Interaktiver Editor für die schematische Abbildung zur Inflammasom-Kinetik nach
Belastung (Manuskript: Figure 5). Schematisch und spekulativ – keine Studiendaten,
y-Achse nicht maßstabsgetreu.

**Web-Version:** https://finn-dreisigacker.github.io/inflammasome-fig3-editor/

Die Web-Version läuft komplett im Browser ([Shinylive](https://posit-dev.github.io/r-shinylive/),
R via WebAssembly); der erste Start dauert daher einige Sekunden.
**Download PNG + SVG (ZIP)** liefert die Abbildung (PNG 600 dpi, SVG) plus
`fig3_params.csv` mit den aktuellen Einstellungen. Änderungen werden im Web nicht
gespeichert – das `fig3_params.csv` aus dem ZIP hier ersetzen und pushen, um die
Startwerte der Seite zu ändern.

## Dateien

| Datei | Inhalt |
| --- | --- |
| `fig3_app/app.R` | Shiny-Editor (UI + Server) |
| `fig3_functions.R` | Kurven-, Plot- und Exportfunktionen |
| `fig3_params.csv` | alle einstellbaren Werte (Startzustand der App) |
| `.github/workflows/deploy.yml` | Build mit Shinylive + Deploy auf GitHub Pages |

## Lokal starten

```r
# Pakete: shiny, shinyWidgets, dplyr, tidyr, purrr, tibble, readr, ggplot2,
#         patchwork, ggtext, ragg, svglite, zip
shiny::runApp("fig3_app")
```

Lokal gibt es zusätzlich **Speichern** (schreibt `fig3_params.csv`) und
**Export PNG + SVG** (schreibt in diesen Ordner).
