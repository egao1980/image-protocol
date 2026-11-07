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

Backends: [`image-backend-stb`](https://github.com/egao1980/image-backend-stb) (native, patched stb) and [`image-backend-lisp`](https://github.com/egao1980/image-backend-lisp) (pure CL).

## License

MIT
