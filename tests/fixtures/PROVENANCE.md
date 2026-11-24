# tests/fixtures/

| File | Content | Source | License | Regenerate |
|------|---------|--------|---------|------------|
| `ops-resize.sexp` | 280 resize vectors: LCG test images (`:noise`, `:alpha-steps`) in 4 pixel formats × 5 filters, as an FNV-1a checksum plus the pixels when they are short | computed by `generate-ops.py`, twice: by REF (its transcription of the rules in `src/ops.lisp`) and by Pillow 13.0.0 `Image.resize(size, filter)` | MIT (generated facts) | `uv run --with pillow==13.0.0 python -I tests/fixtures/generate-ops.py` |
| `ops-geometry.sexp` | 30 crop and rotate vectors on byte grids (byte *i* is *i* mod 256) | REF, checked against Pillow 13.0.0 `Image.crop` / `Image.transpose` | MIT (generated facts) | same script |
| `ops-fit.sexp` | 19 `fit-dimensions` rows | REF (exact rationals); compared with Pillow 13.0.0 `Image.thumbnail` | MIT (generated facts) | same script |

The generator lives next to the fixtures. Regenerate rather than hand-edit. It stops when REF and Pillow disagree, except in the cases listed below. Pillow is under the MIT-CMU licence; only its outputs are recorded here. The resampler rules in `src/ops.lisp` restate the algorithm of Pillow's `src/libImaging/Resample.c` (separable convolution, 22-bit fixed point) and its premultiplication (`Convert.c`) in our own words.

## Where our rules differ from Pillow 13.0.0

- **Nearest neighbour.** We pick source pixel `floor((2x + 1) W / (2 width))` with exact rationals, so a destination centre that falls exactly on a source pixel edge takes the right / lower pixel. Pillow adds `W / width` to a double once per pixel (`ImagingScaleAffine`), and the accumulated error sometimes lands just below that edge. Of the 4096 (in, out) size pairs from 1 to 64, 624 give a different index somewhere. Twelve `ops-resize.sexp` rows differ from Pillow (`pillow-agrees` is `nil`); the other 268 rows, every filter but `:nearest` included, are byte-identical to Pillow.
- **Fit rounding.** `fit-dimensions` gives the bound to the binding side and rounds the other side to nearest, halves up, with exact rationals. Pillow's `thumbnail` picks whichever of floor and ceiling keeps the aspect ratio closer, with floats. The rows differ once: 1080 × 1920 into 1000 × 1000 is 563 × 1000 for us and 562 × 1000 for Pillow (562.5 exactly). Pillow's `thumbnail` also pre-reduces (`reducing_gap=2.0`) and defaults to bicubic; `fit-image` does neither and defaults to Lanczos-3, as the image brief records.
- **Pass order** is Pillow 13's: vertical first when the height shrinks by more than twice what the width does. Pillow 12 and earlier always ran the horizontal pass first, which rounds differently in those cases, so regenerate with Pillow 13.0.0 exactly.

Hand-written rows in `tests/ops-test.lisp` were cross-checked the same way: against REF, and against Pillow wherever our rule and Pillow's agree.
