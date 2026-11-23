# image-protocol

Image decoding and encoding for [cl-stack](https://github.com/egao1980/cl-stack): one CLOS API over several format backends at once. Brief: [`cl-stack/docs/capabilities/image.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/image.md) ([#275](https://github.com/egao1980/cl-stack/issues/275)); research: [`docs/research/image-libraries.md`](https://github.com/egao1980/cl-stack/blob/main/docs/research/image-libraries.md).

```lisp
(stack-image:decode-image #p"photo.jpg" :orient t)      ; ⇒ #S(IMAGE :WIDTH … :FORMAT :RGB …)
(stack-image:probe-image octets)                         ; size / format, no pixels decoded
(stack-image:encode-image image :png)                    ; ⇒ octets
(stack-image:convert-image image :rgba)
```

| | |
|---|---|
| Pixels | `image`: interleaved, row-major, top-left, no stride, **straight alpha**, depth 8 (16 / float reserved) |
| Untrusted by default | `*image-limits*` (16384² / 2²⁸ px / 1000 frames / 512 MiB / 256 MiB input). Backends check them after the header probe, **before allocating**. `*enabled-formats*` defaults to PNG, JPEG, GIF and BMP. |
| Detection | `sniff-format` uses an extensible table of masked signatures. TGA is never sniffed. A declared format that disagrees with the data gives the warning `image-format-mismatch`. |
| Dispatch | Highest backend priority first, `*backend-preference*` overrides. No silent fallback: `try-next-backend` and `use-value` are restarts. |
| Conversion | Done in the protocol for every backend: BT.601 gray, rounded 16→8, alpha added as 255 |
| Metadata | EXIF orientation is read (`exif-orientation`) and not applied unless `:orient t` / `apply-orientation` |
| Animation | `decode-frames` → composited `image-frame`s with `delay-ms`, plus `loop-count`; `map-frames` |
| Encoding | `:quality :compression-level :subsampling :progressive :loop-count :backend-options`, with defaults JPEG 90 and PNG 6 (subsampling is the backend's unless asked). Unknown keys are errors. An option a backend can't honour gives the warning `image-option-ignored`, or the error `image-unsupported-option` under `*strict-options*`. |

## Editing operations

Pillow's most used calls. They are eager and functional: each returns a new `image` and leaves its input unchanged. When there is nothing to do, the input itself comes back. They work on 8-bit images of any pixel format and keep the format and metadata. Pure Lisp, so they run with any backend or none.

```lisp
(stack-image:resize-image img 800 600)                     ; :filter :lanczos3 (default)
(stack-image:resize-image img 80 60 :filter :bilinear :reducing-gap 2)
(stack-image:thumbnail img 128 128)                        ; keep aspect, never enlarge
(stack-image:fit-image img :max-width 1536 :max-height 1536) ; one or both sides; :enlarge t
(stack-image:crop-image img 10 20 100 50)                  ; x y width height
(stack-image:rotate-image img 90)                          ; counter-clockwise, multiples of 90
(stack-image:flip-image img :horizontal)                   ; or :vertical
(stack-image:composite-image background logo :x 16 :y 16)  ; Porter-Duff over
```

| Operation | Pillow | Notes |
|-----------|--------|-------|
| `resize-image image w h &key filter reducing-gap linear` | `resize((w, h), filter, reducing_gap=…)` | Filters (`+resize-filters+`): `:nearest` `:box` `:bilinear` `:bicubic` `:lanczos3`. Same separable convolution as Pillow: the kernel widens with the reduction, is clipped at the edges and normalised, and each pass is clamped to 0–255. Samples match Pillow 11.3 within 1; Pillow rounds to 8 bits between passes. Colour is resampled premultiplied by alpha, so transparent pixels do not bleed. `:linear t` resamples in linear light. `:reducing-gap` box-reduces by an integer factor first. The output size is checked against `*image-limits*` |
| `thumbnail image max-w max-h &key filter reducing-gap` | `thumbnail((max-w, max-h))` | Pillow's size rounding; `reducing-gap` defaults to 2 as in Pillow. The filter defaults to Lanczos3 here (Pillow: bicubic) |
| `fit-image image &key max-width max-height enlarge filter` | — | Scale to one or both bounds, rounded half up (#3) |
| `crop-image image x y w h` | `crop((x, y, x + w, y + h))` | The region must lie inside the image |
| `rotate-image image degrees` | `rotate(degrees, expand=True)` / `transpose(ROTATE_*)` | Multiples of 90, counter-clockwise; negative values turn clockwise |
| `flip-image image :horizontal / :vertical` | `transpose(FLIP_LEFT_RIGHT / FLIP_TOP_BOTTOM)` | |
| `composite-image dst src &key x y operator` | `alpha_composite(dst, src)` | `:over` only. Exact integer arithmetic, so results equal Pillow's. The result is in DST's format, SRC is converted to match, and the parts outside DST are clipped (offsets may be negative) |

A bad argument signals `image-invalid-argument`, with `image-invalid-argument-operation`, `-argument`, `-value` and `-reason`. A depth-16 image signals `image-unsupported-feature`. Arbitrary-angle rotation, drawing and text are not covered.

Backends: [`image-backend-stb`](https://github.com/egao1980/image-backend-stb) (native, patched stb) and [`image-backend-lisp`](https://github.com/egao1980/image-backend-lisp) (pure CL).

## License

MIT
