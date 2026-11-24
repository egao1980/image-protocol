"""Pillow reference values for tests/ops-test.lisp (image-protocol#2).

    uv run --with pillow==11.3.0 python -I tests/pillow-reference.py
"""
import json
from PIL import Image
R = Image.Resampling
def gray(w, h):
    im = Image.new("L", (w, h)); im.putdata([(x * 37 + y * 91) % 256 for y in range(h) for x in range(w)]); return im
out = {}
g = gray(7, 5)
for name, f in [("box", R.BOX), ("bilinear", R.BILINEAR), ("bicubic", R.BICUBIC), ("lanczos3", R.LANCZOS)]:
    out[f"down_{name}"] = list(g.resize((3, 2), f).getdata())
    out[f"up_{name}"] = list(gray(3, 2).resize((5, 4), f).getdata())
big = gray(40, 30)
out["gap_lanczos"] = list(big.resize((7, 5), R.LANCZOS, reducing_gap=2.0).getdata())
out["nogap_lanczos"] = list(big.resize((7, 5), R.LANCZOS).getdata())
# RGBA premultiplied
rgba = Image.new("RGBA", (4, 1)); rgba.putdata([(255,0,0,255),(0,255,0,64),(0,0,255,0),(255,255,255,200)])
out["rgba_bilinear_2x1"] = [list(p) for p in rgba.resize((2, 1), R.BILINEAR).getdata()]
# thumbnail sizes
sizes = []
for (w, h, mw, mh) in [(400,200,100,100),(50,20,100,100),(1000,333,128,128),(333,1000,128,128),(640,480,100,50),(3,1000,10,10),(1001,1000,100,100)]:
    im = Image.new("L", (w, h)); im.thumbnail((mw, mh)); sizes.append([w, h, mw, mh, list(im.size)])
out["thumb"] = sizes
# alpha_composite
dst = Image.new("RGBA", (3, 1)); dst.putdata([(10,20,30,255),(200,100,50,128),(0,0,0,0)])
src = Image.new("RGBA", (3, 1)); src.putdata([(255,255,255,77),(0,128,255,200),(90,80,70,33)])
out["over"] = [list(p) for p in Image.alpha_composite(dst, src).getdata()]
print(json.dumps(out))
