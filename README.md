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
| `resize-image image w h &key filter reducing-gap linear backend` | `resize((w, h), filter, reducing_gap=…)` | Filters (`+resize-filters+`): `:nearest` `:box` `:bilinear` `:bicubic` `:lanczos3`. Samples equal Pillow 11.3's, byte for byte: the same separable convolution in 22-bit fixed point (the kernel widens with the reduction, is clipped at the edges and normalised), each pass rounded to 8 bits, colour premultiplied by alpha in 8 bits (so transparent pixels do not bleed), and `:nearest` picking the same pixel at exact ties. `:reducing-gap` box-reduces by an integer factor first (a float gap counts as the decimal it prints as, so `1.7`, a single float, is Python's `1.7`); as in Pillow, not for images with alpha (Pillow resizes their premultiplied copy without the gap). `:linear t` resamples in linear light, in floats (Pillow has no equivalent). The output size is checked against `*image-limits*` |
| `thumbnail image max-w max-h &key filter reducing-gap backend` | `thumbnail((max-w, max-h))` | Pillow's size rounding (in doubles, so near ties go the same way); `reducing-gap` defaults to 2 as in Pillow, so an image with alpha is resized without it. The filter defaults to Lanczos3 here (Pillow: bicubic) |
| `fit-image image &key max-width max-height enlarge filter backend` | — | Scale to one or both bounds, rounded half up (#3) |
| `crop-image image x y w h` | `crop((x, y, x + w, y + h))` | The region must lie inside the image |
| `rotate-image image degrees` | `rotate(degrees, expand=True)` / `transpose(ROTATE_*)` | Multiples of 90, counter-clockwise; negative values turn clockwise |
| `flip-image image :horizontal / :vertical` | `transpose(FLIP_LEFT_RIGHT / FLIP_TOP_BOTTOM)` | |
| `composite-image dst src &key x y operator` | `alpha_composite(dst, src)` | `:over` only. Pillow's 7-bit fixed-point blend, so results equal Pillow's byte for byte. The result is in DST's format, SRC is converted to match, and the parts outside DST are clipped (offsets may be negative) |

Parity is tested against Pillow 11.3 byte for byte: `tests/pillow-parity.py` writes `tests/pillow-parity.sexp` (276 resizes of noise images in every format, with every filter, with and without a reducing gap, decimal gaps such as `1.7` included; 300 thumbnail sizes; composites with alpha 0, 255 and in between). `pillow-parity.py --random N` writes a stress set that `IMAGE_PROTOCOL_PARITY=file cl-repo test` runs instead.

A bad argument signals `image-invalid-argument`, with `image-invalid-argument-operation`, `-argument`, `-value` and `-reason`. A depth-16 image signals `image-unsupported-feature`. Arbitrary-angle rotation, drawing and text are not covered.

### Native fast path (0.3.0)

A backend can resize natively by specialising `backend-resize`. The pure-Lisp resampler stays the default and the reference, and Pillow parity holds only for it. So a backend resizes only when the caller names it: registering it, for decoding say, changes nothing.

```lisp
(stack-image:resize-image img 800 600 :filter :bicubic :backend stb)  ; this call
(let ((stack-image:*resize-backends* (list stb)))                     ; a scope
  (stack-image:thumbnail img 256 256))
```

| | |
|---|---|
| Designator | `:backend` of `resize-image`, `thumbnail` and `fit-image`, default `*resize-backends*` (NIL): NIL, a backend, or a list of backends tried in order. Anything else is an `image-invalid-argument` (argument `backend`) |
| Hook | `(backend-resize backend image width height &key filter reducing-gap linear)`. All three keys are always passed. `reducing-gap` is the one the pure-Lisp path applies, so it is NIL for images with alpha and for `:nearest`. NIL declines, and the method on `image-backend` always declines |
| Answer | A fresh `width` × `height` image in the input's format at depth 8. The result is a copy of the input (metadata, frame slots) with the answer's pixels. Any other answer signals `image-corrupt` with the backend |
| Fallback | When every backend declines, the result is 0.2.0's, sample for sample. The restart `try-next-backend` around each call treats that backend as declining |
| Unchanged | Every 0.2.0 check runs first. A same-size resize returns the image itself without calling a backend. No global state |

The exact rules (R1–R8) are in `src/ops.lisp`, "Backend fast path", and the tests are in `tests/resize-hook-test.lisp`. [`image-backend-stb`](https://github.com/egao1980/image-backend-stb) provides a method on `stb_image_resize2`, tested for closeness to this resampler, not byte equality.

Backends: [`image-backend-stb`](https://github.com/egao1980/image-backend-stb) (native, patched stb) and [`image-backend-lisp`](https://github.com/egao1980/image-backend-lisp) (pure CL).

## License

MIT
