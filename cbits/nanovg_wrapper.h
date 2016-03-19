#ifndef NANOVG_WRAPPER_H
#define NANOVG_WRAPPER_H

#include "nanovg.h"

void nvgRGB_(unsigned char r, unsigned char g, unsigned char b, NVGcolor* out);

void nvgRGBf_(float r, float g, float b, NVGcolor* out);

void nvgRGBA_(unsigned char r, unsigned char g, unsigned char b, unsigned char a, NVGcolor* out);

void nvgRGBAf_(float r, float g, float b, float a, NVGcolor* out);

void nvgLerpRGBA_(NVGcolor* c0, NVGcolor* c1, float u, NVGcolor* out);

void nvgTransRGBA_(NVGcolor* c0, unsigned char a, NVGcolor* out);

void nvgTransRGBAf_(NVGcolor* c0, float a, NVGcolor* out);

void nvgHSL_(float h, float s, float l, NVGcolor* out);

void nvgHSLA_(float h, float s, float l, unsigned char a, NVGcolor* out);

void nvgLinearGradient_(NVGcontext* ctx, float sx, float sy, float ex, float ey,
                        NVGcolor* icol, NVGcolor* ocol, NVGpaint* out);

void nvgBoxGradient_(NVGcontext* ctx, float x, float y, float w, float h,
                     float r, float f, NVGcolor* icol, NVGcolor* ocol, NVGpaint* out);

void nvgRadialGradient_(NVGcontext* ctx, float cx, float cy, float inr, float outr,
                        NVGcolor* icol, NVGcolor* ocol, NVGpaint* out);

void nvgImagePattern_(NVGcontext* ctx, float ox, float oy, float ex, float ey,
                      float angle, int image, float alpha, NVGpaint* out);

void nvgStrokePaint_(NVGcontext *ctx, NVGpaint *paint);

void nvgStrokeColor_(NVGcontext *ctx, NVGcolor *color);

void nvgFillPaint_(NVGcontext *ctx, NVGpaint *paint);

void nvgFillColor_(NVGcontext *ctx, NVGcolor *color);

#endif // NANOVG_WRAPPER_H
