#include "nanovg_wrapper.h"

void nvgRGB_(unsigned char r, unsigned char g, unsigned char b, NVGcolor *out) {
  *out = nvgRGB(r, g, b);
}

void nvgRGBf_(float r, float g, float b, NVGcolor *out) {
  *out = nvgRGBf(r, g, b);
}

void nvgRGBA_(unsigned char r, unsigned char g, unsigned char b,
              unsigned char a, NVGcolor *out) {
  *out = nvgRGBA(r, g, b, a);
}

void nvgRGBAf_(float r, float g, float b, float a, NVGcolor *out) {
  *out = nvgRGBAf(r, g, b, a);
}

void nvgLerpRGBA_(NVGcolor* c0, NVGcolor* c1, float u, NVGcolor *out) {
  *out = nvgLerpRGBA(*c0, *c1, u);
}

void nvgTransRGBA_(NVGcolor* c0, unsigned char a, NVGcolor *out) {
  *out = nvgTransRGBA(*c0, a);
}

void nvgTransRGBAf_(NVGcolor* c0, float a, NVGcolor *out) {
  *out = nvgTransRGBAf(*c0, a);
}

void nvgHSL_(float h, float s, float l, NVGcolor *out) {
  *out = nvgHSL(h, s, l);
}

void nvgHSLA_(float h, float s, float l, unsigned char a, NVGcolor *out) {
  *out = nvgHSLA(h, s, l, a);
}

void nvgLinearGradient_(NVGcontext *ctx, float sx, float sy, float ex, float ey,
                        NVGcolor* icol, NVGcolor* ocol, NVGpaint *out) {
  *out = nvgLinearGradient(ctx, sx, sy, ex, ey, *icol, *ocol);
}

void nvgBoxGradient_(NVGcontext *ctx, float x, float y, float w, float h,
                     float r, float f, NVGcolor* icol, NVGcolor* ocol,
                     NVGpaint *out) {
  *out = nvgBoxGradient(ctx, x, y, w, h, r, f, *icol, *ocol);
}

void nvgRadialGradient_(NVGcontext *ctx, float cx, float cy, float inr,
                        float outr, NVGcolor* icol, NVGcolor* ocol,
                        NVGpaint *out) {
  *out = nvgRadialGradient(ctx, cx, cy, inr, outr, *icol, *ocol);
}

void nvgImagePattern_(NVGcontext *ctx, float ox, float oy, float ex, float ey,
                      float angle, int image, float alpha, NVGpaint *out) {
  *out = nvgImagePattern(ctx, ox, oy, ex, ey, angle, image, alpha);
}

// c2hs can theoretically generate those, but it’s too much Setup.hs mess before Cabal 1.24
void nvgStrokePaint_(NVGcontext *ctx, NVGpaint *paint) {
  return nvgStrokePaint(ctx, *paint);
}
void nvgStrokeColor_(NVGcontext *ctx, NVGcolor *color) {
  return nvgStrokeColor(ctx, *color);
}

void nvgFillPaint_(NVGcontext *ctx, NVGpaint *paint) {
  return nvgFillPaint(ctx, *paint);
}

void nvgFillColor_(NVGcontext *ctx, NVGcolor *color) {
  return nvgFillColor(ctx, *color);
}
