"""Pillow parity cases for tests/parity-test.lisp (image-protocol#2).

    uv run --with pillow==11.3.0 python -I tests/pillow-parity.py > tests/pillow-parity.sexp

With `--random N [SEED]` it writes N random resize cases instead (sizes,
formats, filters and gaps drawn at random), for a local stress run:

    uv run --with pillow==11.3.0 python -I tests/pillow-parity.py --random 3000 > /tmp/parity.sexp
    IMAGE_PROTOCOL_PARITY=/tmp/parity.sexp cl-repo test

Gaps are written as the Lisp test reads them: 2, 3, 3/2, or a decimal
such as 1.7 that the Lisp reader makes a single float.

Input images are noise from the LCG in NOISE (the Lisp test has the same
one); alpha below 32 becomes 0 and above 223 becomes 255, so fully
transparent and opaque pixels are common. Each line is one case:

  (:resize FORMAT W H OUT-W OUT-H FILTER GAP SEED "hex of Pillow's tobytes()")
  (:thumbnail W H MAX-W MAX-H OUT-W OUT-H)
  (:composite W H DST-SEED SRC-SEED "hex")
"""
import random
import sys

import PIL
from PIL import Image

R = Image.Resampling
FILTERS = [("nearest", R.NEAREST), ("box", R.BOX), ("bilinear", R.BILINEAR),
           ("bicubic", R.BICUBIC), ("lanczos3", R.LANCZOS)]
MODES = [("gray", "L", 1), ("gray-alpha", "LA", 2), ("rgb", "RGB", 3), ("rgba", "RGBA", 4)]


def noise(n, seed):
    s = seed
    out = bytearray(n)
    for i in range(n):
        s = (s * 1103515245 + 12345) & 0x7FFFFFFF
        out[i] = (s >> 16) & 0xFF
    return out


def make(mode, channels, w, h, seed):
    data = noise(w * h * channels, seed)
    if mode in ("LA", "RGBA"):
        for p in range(channels - 1, len(data), channels):
            a = data[p]
            data[p] = 0 if a < 32 else 255 if a > 223 else a
    return Image.frombytes(mode, (w, h), bytes(data))


def gap_text(gap):
    """The gap as the Lisp test writes it: 2, 3, 3/2, or a decimal such as
    1.7, which the Lisp reader makes a single float (1.70000005)."""
    if gap is None:
        return "nil"
    return {2.0: "2", 3.0: "3", 1.5: "3/2"}.get(gap, repr(gap))


def resize_case(fmt, mode, c, w, h, ow, oh, fname, f, gap, seed):
    im = make(mode, c, w, h, seed)
    out = im.resize((ow, oh), f, reducing_gap=gap)
    print(f'(:resize :{fmt} {w} {h} {ow} {oh} :{fname} {gap_text(gap)} {seed} "{out.tobytes().hex()}")')


def fixed_cases():
    sizes = [(37, 23, 5, 4), (64, 48, 9, 7), (50, 50, 9, 4), (31, 9, 7, 9),
             (3, 2, 7, 5), (20, 5, 5, 7), (12, 9, 12, 4)]
    gap_sizes = {(37, 23, 5, 4): [2.0], (64, 48, 9, 7): [2.0, 3.0],
                 (50, 50, 9, 4): [2.0], (31, 9, 7, 9): [1.5]}
    seed = 1
    for fmt, mode, c in MODES:
        for fname, f in FILTERS:
            for size in sizes:
                gaps = [None] if fname == "nearest" else [None] + gap_sizes.get(size, [])
                for gap in gaps:
                    resize_case(fmt, mode, c, *size, fname, f, gap, seed)
                    seed += 1
    # Decimal gaps where Pillow's int(w / out_w / gap) differs from the
    # quotient with the single float a Lisp user writes (1.7 -> 1.70000005).
    decimal_gaps = [(34, 3, 10, 3, 1.7), (33, 6, 3, 2, 1.1), (12, 12, 1, 5, 1.2),
                    (91, 7, 5, 3, 1.3), (81, 20, 3, 4, 2.7), (117, 9, 5, 2, 1.8),
                    (24, 16, 1, 3, 2.4)]
    for fmt, mode, c in [m for m in MODES if m[0] in ("gray", "rgb")]:
        for fname, f in FILTERS[1:]:
            for w, h, ow, oh, gap in decimal_gaps:
                resize_case(fmt, mode, c, w, h, ow, oh, fname, f, gap, seed)
                seed += 1
    # Reductions of 4x and more, with and without the gap (image-protocol#9):
    # a box reduce by 2, by 4, and by 4 with partial blocks on both edges.
    large_reductions = [(96, 64, 24, 16), (160, 120, 20, 15), (130, 70, 16, 8)]
    for fmt, mode, c in [m for m in MODES if m[0] in ("gray", "rgb")]:
        for fname, f in [("bicubic", R.BICUBIC), ("lanczos3", R.LANCZOS)]:
            for size in large_reductions:
                for gap in [None, 2.0]:
                    resize_case(fmt, mode, c, *size, fname, f, gap, seed)
                    seed += 1
    rng = random.Random(2)
    for _ in range(300):
        w, h, mw, mh = (rng.randint(1, 40) for _ in range(4))
        im = Image.new("L", (w, h))
        im.thumbnail((mw, mh))
        print(f"(:thumbnail {w} {h} {mw} {mh} {im.size[0]} {im.size[1]})")
    for dseed, sseed in [(7, 8), (9, 10), (11, 12)]:
        dst = make("RGBA", 4, 24, 10, dseed)
        src = make("RGBA", 4, 24, 10, sseed)
        print(f'(:composite 24 10 {dseed} {sseed} "{Image.alpha_composite(dst, src).tobytes().hex()}")')


def random_cases(n, seed):
    rng = random.Random(seed)
    for i in range(n):
        fmt, mode, c = rng.choice(MODES)
        fname, f = rng.choice(FILTERS)
        big = rng.random() < 0.1
        w, h = (rng.randint(90, 400) if big else rng.randint(1, 90) for _ in range(2))
        ow, oh = rng.randint(1, 24), rng.randint(1, 24)
        if (w, h) == (ow, oh):
            ow += 1
        gap = None if fname == "nearest" else rng.choice([None, 2.0, 3.0, 1.5, 1.1, 1.7, 2.7])
        resize_case(fmt, mode, c, w, h, ow, oh, fname, f, gap, 1000 + i)
    for _ in range(n):
        w, h, mw, mh = (rng.randint(1, 2000) for _ in range(4))
        im = Image.new("1", (w, h))
        im.thumbnail((mw, mh), reducing_gap=None)
        print(f"(:thumbnail {w} {h} {mw} {mh} {im.size[0]} {im.size[1]})")
    for i in range(max(1, n // 100)):
        w, h = rng.randint(1, 40), rng.randint(1, 40)
        dst = make("RGBA", 4, w, h, 5000 + 2 * i)
        src = make("RGBA", 4, w, h, 5001 + 2 * i)
        print(f'(:composite {w} {h} {5000 + 2 * i} {5001 + 2 * i} "{Image.alpha_composite(dst, src).tobytes().hex()}")')


if __name__ == "__main__":
    print(f";;; Generated by tests/pillow-parity.py with Pillow {PIL.__version__}; do not edit.")
    if len(sys.argv) > 1 and sys.argv[1] == "--random":
        random_cases(int(sys.argv[2]), int(sys.argv[3]) if len(sys.argv) > 3 else 1)
    else:
        fixed_cases()
