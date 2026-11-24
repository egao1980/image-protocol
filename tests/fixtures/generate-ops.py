"""Generate the image-ops fixtures (tests/fixtures/ops-*.sexp).

    uv run --with pillow==13.0.0 python -I tests/fixtures/generate-ops.py

Two independent oracles:

* REF: a direct transcription of the rules in src/ops.lisp (plain Python
  floats are IEEE doubles; integers are exact).
* Pillow 13.0.0 (MIT-CMU licence): Image.resize(size, filter) with no
  reducing_gap, Image.crop, Image.transpose, Image.thumbnail.

Every resize row must agree between REF and Pillow, except where
PROVENANCE.md says our rule differs on purpose (nearest-neighbour index
rounding, fit-image rounding). The script stops on any other mismatch.
"""

import math
import os
import sys
from fractions import Fraction

import PIL
from PIL import Image

assert PIL.__version__ == "13.0.0", PIL.__version__

HERE = os.path.dirname(os.path.abspath(__file__))

FORMATS = {":gray": ("L", 1), ":gray-alpha": ("LA", 2), ":rgb": ("RGB", 3), ":rgba": ("RGBA", 4)}
PIL_FILTERS = {
    ":nearest": Image.Resampling.NEAREST,
    ":box": Image.Resampling.BOX,
    ":bilinear": Image.Resampling.BILINEAR,
    ":bicubic": Image.Resampling.BICUBIC,
    ":lanczos3": Image.Resampling.LANCZOS,
}
ALPHA_STEPS = [0, 1, 64, 128, 254, 255]

# --------------------------------------------------------------------------
# Test images and checksum (mirrored in tests/ops-test.lisp)


def lcg_bytes(n, seed):
    out = bytearray(n)
    state = seed
    for i in range(n):
        state = (state * 1103515245 + 12345) % (1 << 31)
        out[i] = (state >> 16) & 255
    return out


def pattern(kind, w, h, c, seed):
    """:noise: every byte from the LCG. :alpha-steps: the same, but the alpha
    byte of pixel p is ALPHA_STEPS[p mod 6]."""
    data = lcg_bytes(w * h * c, seed)
    if kind == ":alpha-steps":
        assert c in (2, 4)
        for p in range(w * h):
            data[p * c + c - 1] = ALPHA_STEPS[p % 6]
    return data


def grid(w, h, c):
    """Byte i is i mod 256."""
    return bytearray(i % 256 for i in range(w * h * c))


def fnv1a(data):
    h = 2166136261
    for b in data:
        h = ((h ^ b) * 16777619) & 0xFFFFFFFF
    return h


# --------------------------------------------------------------------------
# REF: the rules of src/ops.lisp


def k_box(x):
    return 1.0 if (x > -0.5 and x <= 0.5) else 0.0


def k_bilinear(x):
    if x < 0.0:
        x = -x
    return 1.0 - x if x < 1.0 else 0.0


def k_bicubic(x):
    a = -0.5
    if x < 0.0:
        x = -x
    if x < 1.0:
        return ((a + 2.0) * x - (a + 3.0)) * x * x + 1
    if x < 2.0:
        return (((x - 5) * x + 8) * x - 4) * a
    return 0.0


def sinc(x):
    if x == 0.0:
        return 1.0
    x = x * math.pi
    return math.sin(x) / x


def k_lanczos3(x):
    if -3.0 <= x < 3.0:
        return sinc(x) * sinc(x / 3)
    return 0.0


KERNELS = {":box": (k_box, 0.5), ":bilinear": (k_bilinear, 1.0),
           ":bicubic": (k_bicubic, 2.0), ":lanczos3": (k_lanczos3, 3.0)}


def coefficients(n_in, n_out, filt):
    kernel, support0 = KERNELS[filt]
    scale = n_in / n_out
    filterscale = max(scale, 1.0)
    support = support0 * filterscale
    inv = 1.0 / filterscale
    rows = []
    for i in range(n_out):
        center = (i + 0.5) * scale
        lo = max(0, int(center - support + 0.5))
        hi = min(n_in, int(center + support + 0.5))
        ws = [kernel(((lo + j) - center + 0.5) * inv) for j in range(hi - lo)]
        total = 0.0
        for w in ws:
            total += w
        if total != 0.0:
            ws = [w / total for w in ws]
        q = [int(w * 4194304 - 0.5) if w < 0 else int(w * 4194304 + 0.5) for w in ws]
        rows.append((lo, q))
    return rows


def clip8(s):
    v = s >> 22
    return 0 if v < 0 else 255 if v > 255 else v


def pass_x(src, w, h, c, n_out, filt):
    co = coefficients(w, n_out, filt)
    out = bytearray(n_out * h * c)
    for y in range(h):
        for i, (lo, q) in enumerate(co):
            for ch in range(c):
                s = 1 << 21
                for j, qq in enumerate(q):
                    s += src[(y * w + lo + j) * c + ch] * qq
                out[(y * n_out + i) * c + ch] = clip8(s)
    return out


def pass_y(src, w, h, c, n_out, filt):
    co = coefficients(h, n_out, filt)
    out = bytearray(w * n_out * c)
    for i, (lo, q) in enumerate(co):
        for x in range(w):
            for ch in range(c):
                s = 1 << 21
                for j, qq in enumerate(q):
                    s += src[((lo + j) * w + x) * c + ch] * qq
                out[(i * w + x) * c + ch] = clip8(s)
    return out


def premultiply(data, c):
    out = bytearray(data)
    for p in range(len(data) // c):
        a = data[p * c + c - 1]
        for ch in range(c - 1):
            t = data[p * c + ch] * a + 128
            out[p * c + ch] = ((t >> 8) + t) >> 8
    return out


def unpremultiply(data, c):
    out = bytearray(data)
    for p in range(len(data) // c):
        a = data[p * c + c - 1]
        if a not in (0, 255):
            for ch in range(c - 1):
                out[p * c + ch] = min(255, (255 * data[p * c + ch]) // a)
    return out


def nearest_index(n_in, n_out, i):
    return ((2 * i + 1) * n_in) // (2 * n_out)


def ref_resize(src, w, h, c, nw, nh, filt):
    if (w, h) == (nw, nh):
        return bytearray(src)
    if filt == ":nearest":
        out = bytearray(nw * nh * c)
        for y in range(nh):
            sy = nearest_index(h, nh, y)
            for x in range(nw):
                sx = nearest_index(w, nw, x)
                out[(y * nw + x) * c:(y * nw + x + 1) * c] = src[(sy * w + sx) * c:(sy * w + sx + 1) * c]
        return out
    alpha = c in (2, 4)
    data = premultiply(src, c) if alpha else bytearray(src)
    x_first = not (h - nh > 0 and h - nh > 2 * (w - nw))
    if x_first:
        if nw != w:
            data = pass_x(data, w, h, c, nw, filt)
        if nh != h:
            data = pass_y(data, nw, h, c, nh, filt)
    else:
        if nh != h:
            data = pass_y(data, w, h, c, nh, filt)
        if nw != w:
            data = pass_x(data, w, nh, c, nw, filt)
    return unpremultiply(data, c) if alpha else data


def ref_crop(src, w, h, c, x, y, cw, ch):
    out = bytearray()
    for row in range(y, y + ch):
        out += src[(row * w + x) * c:(row * w + x + cw) * c]
    return out


def ref_rotate(src, w, h, c, degrees):
    """Clockwise. Destination (x, y) of the W' x H' result reads source (sx, sy)."""
    d = degrees % 360
    nw, nh = (h, w) if d in (90, 270) else (w, h)
    out = bytearray(nw * nh * c)
    for y in range(nh):
        for x in range(nw):
            if d == 0:
                sx, sy = x, y
            elif d == 90:
                sx, sy = y, h - 1 - x
            elif d == 180:
                sx, sy = w - 1 - x, h - 1 - y
            else:
                sx, sy = w - 1 - y, x
            out[(y * nw + x) * c:(y * nw + x + 1) * c] = src[(sy * w + sx) * c:(sy * w + sx + 1) * c]
    return nw, nh, out


def ref_fit(w, h, mw, mh):
    if (mw is None or w <= mw) and (mh is None or h <= mh):
        return w, h
    rw = Fraction(mw, w) if mw is not None else None
    rh = Fraction(mh, h) if mh is not None else None
    if rh is None or (rw is not None and rw <= rh):
        return mw, max(1, math.floor(Fraction(h * mw, w) + Fraction(1, 2)))
    return max(1, math.floor(Fraction(w * mh, h) + Fraction(1, 2))), mh


# --------------------------------------------------------------------------
# Pillow


def pil_image(fmt, w, h, data):
    return Image.frombytes(FORMATS[fmt][0], (w, h), bytes(data))


def pil_resize(fmt, w, h, data, nw, nh, filt):
    return bytearray(pil_image(fmt, w, h, data).resize((nw, nh), PIL_FILTERS[filt]).tobytes())


# --------------------------------------------------------------------------
# Rows


RESIZE_SIZES = [
    (7, 5, 3, 2), (5, 3, 11, 7), (9, 4, 9, 2), (4, 9, 2, 9), (6, 6, 4, 9),
    (13, 11, 5, 7), (16, 12, 4, 3), (3, 2, 1, 1), (1, 1, 4, 3), (20, 30, 19, 4),
    (40, 7, 6, 7), (64, 48, 23, 17),
]
GOLDEN_SIZES = [(97, 61, 40, 25), (31, 17, 128, 70)]


def sexp_bytes(data):
    return "(" + " ".join(str(b) for b in data) + ")"


def resize_rows():
    rows, notes = [], []
    seed = 1
    cases = []
    for fmt in FORMATS:
        for filt in PIL_FILTERS:
            for (w, h, nw, nh) in RESIZE_SIZES:
                cases.append((fmt, filt, ":noise", w, h, nw, nh, seed))
                seed += 1
    for fmt in (":gray-alpha", ":rgba"):
        for filt in PIL_FILTERS:
            for (w, h, nw, nh) in [(12, 6, 5, 3), (6, 4, 13, 9)]:
                cases.append((fmt, filt, ":alpha-steps", w, h, nw, nh, seed))
                seed += 1
    for fmt in (":rgb", ":rgba"):
        for filt in PIL_FILTERS:
            for (w, h, nw, nh) in GOLDEN_SIZES:
                cases.append((fmt, filt, ":noise", w, h, nw, nh, seed))
                seed += 1
    nearest_diff = 0
    for (fmt, filt, kind, w, h, nw, nh, s) in cases:
        c = FORMATS[fmt][1]
        src = pattern(kind, w, h, c, s)
        ref = ref_resize(src, w, h, c, nw, nh, filt)
        pil = pil_resize(fmt, w, h, src, nw, nh, filt)
        agrees = ref == pil
        if not agrees:
            if filt != ":nearest":
                sys.exit(f"REF and Pillow differ: {fmt} {filt} {kind} {w}x{h} -> {nw}x{nh} seed {s}")
            nearest_diff += 1
        pixels = sexp_bytes(ref) if len(ref) <= 48 else "nil"
        rows.append(f" ({fmt} {filt} {kind} {w} {h} {s} {nw} {nh} {fnv1a(ref)} {'t' if agrees else 'nil'} {pixels})")
    notes.append(f"resize rows: {len(rows)}; nearest rows where Pillow's index differs: {nearest_diff}")
    return rows, notes


def nearest_survey():
    """Count (n_in, n_out) pairs up to 64 where Pillow's accumulated
    floating-point index differs from floor((2i+1) n_in / (2 n_out))."""
    diff = []
    for n_in in range(1, 65):
        for n_out in range(1, 65):
            a0 = n_in / n_out
            xo = 0.0 + a0 * 0.5
            for i in range(n_out):
                if int(xo) != nearest_index(n_in, n_out, i):
                    diff.append((n_in, n_out))
                    break
                xo += a0
    return diff


GEOMETRY = [
    # (op, format, w, h, args)
    (":crop", ":gray", 4, 3, (1, 1, 2, 2)),
    (":crop", ":gray", 4, 3, (0, 0, 4, 1)),
    (":crop", ":gray", 4, 3, (3, 0, 1, 3)),
    (":crop", ":gray-alpha", 3, 3, (1, 0, 2, 3)),
    (":crop", ":rgb", 3, 2, (1, 1, 2, 1)),
    (":crop", ":rgb", 5, 4, (2, 1, 3, 3)),
    (":crop", ":rgba", 4, 4, (1, 2, 3, 2)),
    (":crop", ":rgba", 2, 2, (1, 1, 1, 1)),
]
for fmt in FORMATS:
    for d in (90, 180, 270, -90, 450):
        GEOMETRY.append((":rotate", fmt, 3, 2, (d,)))
GEOMETRY.append((":rotate", ":gray", 1, 4, (90,)))
GEOMETRY.append((":rotate", ":rgb", 4, 1, (270,)))


def geometry_rows():
    rows = []
    for (op, fmt, w, h, args) in GEOMETRY:
        c = FORMATS[fmt][1]
        src = grid(w, h, c)
        im = pil_image(fmt, w, h, src)
        if op == ":crop":
            x, y, cw, ch = args
            ref = ref_crop(src, w, h, c, x, y, cw, ch)
            pil = im.crop((x, y, x + cw, y + ch))
            nw, nh = cw, ch
        else:
            (d,) = args
            nw, nh, ref = ref_rotate(src, w, h, c, d)
            pil = im.transpose({90: Image.Transpose.ROTATE_270, 180: Image.Transpose.ROTATE_180,
                                270: Image.Transpose.ROTATE_90}[d % 360])
        if pil.size != (nw, nh) or bytearray(pil.tobytes()) != ref:
            sys.exit(f"REF and Pillow differ: {op} {fmt} {w}x{h} {args}")
        rows.append(f" ({op} {fmt} {w} {h} {sexp_bytes(args)} {nw} {nh} {sexp_bytes(ref)})")
    return rows


FITS = [
    (4000, 3000, 1500, 1500), (3000, 4000, 1536, 1536), (6000, 4000, 1568, 1568),
    (100, 50, None, 10), (100, 50, 30, None), (101, 50, 30, None), (10, 5, 3, None),
    (3, 1000, 10, 10), (1000, 3, 10, 10), (200, 100, 100, 50), (200, 100, 100, 80),
    (200, 100, 150, 50), (640, 480, 320, 320), (480, 640, 320, 320), (7, 3, 2, 2),
    (5, 5, 3, 4), (1920, 1080, 1280, 720), (1080, 1920, 1000, 1000), (999, 1, 10, 10),
]
FITS_IDENTITY = [(100, 50, 100, 50), (100, 50, 200, 200), (100, 50, None, None), (100, 50, 100, None),
                 (1, 1, 1, 1)]


def lisp(v):
    return "nil" if v is None else str(v)


def fit_rows():
    rows, notes = [], []
    differ = []
    for (w, h, mw, mh) in FITS:
        nw, nh = ref_fit(w, h, mw, mh)
        assert (mw is None or nw <= mw) and (mh is None or nh <= mh) and nw >= 1 and nh >= 1
        if mw is not None and mh is not None:
            im = Image.new("L", (w, h))
            im.thumbnail((mw, mh))
            if im.size != (nw, nh):
                differ.append(((w, h, mw, mh), (nw, nh), im.size))
        rows.append(f" ({w} {h} {lisp(mw)} {lisp(mh)} {nw} {nh})")
    for (w, h, mw, mh) in FITS_IDENTITY:
        assert ref_fit(w, h, mw, mh) == (w, h)
    notes.append(f"fit rows: {len(rows)}; Pillow thumbnail differs on: {differ}")
    return rows, notes


def write(name, header, rows):
    with open(os.path.join(HERE, name), "w", encoding="utf-8") as out:
        out.write(header)
        out.write("(\n")
        out.write("\n".join(rows))
        out.write(")\n")


def main():
    rrows, notes = resize_rows()
    write("ops-resize.sexp",
          ";; Generated by generate-ops.py (REF rules + Pillow 13.0.0). Do not edit.\n"
          ";; (format filter pattern src-w src-h seed dst-w dst-h fnv1a pillow-agrees pixels-or-nil)\n",
          rrows)
    write("ops-geometry.sexp",
          ";; Generated by generate-ops.py (REF rules + Pillow 13.0.0). Do not edit.\n"
          ";; (op format src-w src-h args dst-w dst-h pixels); source byte i is i mod 256\n",
          geometry_rows())
    frows, fnotes = fit_rows()
    write("ops-fit.sexp",
          ";; Generated by generate-ops.py (exact rationals). Do not edit.\n"
          ";; (width height max-width max-height result-width result-height)\n",
          frows)
    survey = nearest_survey()
    for n in notes + fnotes:
        print(n)
    print(f"nearest survey 1..64: {len(survey)} of 4096 (in, out) pairs differ from Pillow; first: {survey[:8]}")


if __name__ == "__main__":
    main()
