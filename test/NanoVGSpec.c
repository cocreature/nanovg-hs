
#include "nanovg.h"

void inline_c_NanoVGSpec_0_df4148b45b426a534ab3d5cdd17865b8b3e9182c(float * cInPtr_inline_c_0, float * cOutPtr_inline_c_1, int n_inline_c_2) {

      float* in = cInPtr_inline_c_0;
      float* out = cOutPtr_inline_c_1;
      for (int i = 0; i < n_inline_c_2; ++i) {
        for (int j = 0; j < 6; ++j) {
          out[6*i+j] = in[6*i+j];
        }
      }
    
}


void inline_c_NanoVGSpec_1_1b535a31826c527af6f71272ebb477556ed8af0f(float * cInPtr_inline_c_0, float * cOutPtr_inline_c_1, int n_inline_c_2) {

    float* in = cInPtr_inline_c_0;
    float* out = cOutPtr_inline_c_1;
    for (int i = 0; i < n_inline_c_2; ++i) {
      out[2*i] = in[2*i];
      out[2*i+1] = in[2*i+1];
    }
    
}


void inline_c_NanoVGSpec_2_c7afee80798b912241a301b61c7d8cc11c02b205(NVGcolor * cInPtr_inline_c_0, NVGcolor * cOutPtr_inline_c_1, int n_inline_c_2) {

    NVGcolor* in = cInPtr_inline_c_0;
    NVGcolor* out = cOutPtr_inline_c_1;
    for (int i = 0; i < n_inline_c_2; ++i) {
      out[i].r = in[i].r;
      out[i].g = in[i].g;
      out[i].b = in[i].b;
      out[i].a = in[i].a;
    }
    
}


void inline_c_NanoVGSpec_3_9013abdecf112a5122b710a60f884bbeecc25595(NVGpaint * cInPtr_inline_c_0, NVGpaint * cOutPtr_inline_c_1, int n_inline_c_2) {

       NVGpaint* in = cInPtr_inline_c_0;
       NVGpaint* out = cOutPtr_inline_c_1;
       for (int i = 0; i < n_inline_c_2; ++i) {
           for (int j = 0; j < 6; ++j) {
             out[i].xform[j] = in[i].xform[j];
           }
           out[i].extent[0] = in[i].extent[0];
           out[i].extent[1] = in[i].extent[1];
           out[i].radius = in[i].radius;
           out[i].feather = in[i].feather;
           out[i].innerColor = in[i].innerColor;
           out[i].outerColor = in[i].outerColor;
           out[i].image = in[i].image;
       }
     
}


void inline_c_NanoVGSpec_4_ff75ac86be53956ce5c18541f11daa395302bc69(NVGglyphPosition * cInPtr_inline_c_0, NVGglyphPosition * cOutPtr_inline_c_1, int n_inline_c_2) {

      NVGglyphPosition* in = cInPtr_inline_c_0;
      NVGglyphPosition* out = cOutPtr_inline_c_1;
      for (int i = 0; i < n_inline_c_2; ++i) {
        out[i].str = in[i].str;
        out[i].x = in[i].x;
        out[i].minx = in[i].minx;
        out[i].maxx = in[i].maxx;
      }     
    
}


void inline_c_NanoVGSpec_5_c2a4b2347a68231f86f11ec7f3f7ed736b8342a1(NVGtextRow * cInPtr_inline_c_0, NVGtextRow * cOutPtr_inline_c_1, int n_inline_c_2) {

      NVGtextRow* in = cInPtr_inline_c_0;
      NVGtextRow* out = cOutPtr_inline_c_1;
      for (int i = 0; i < n_inline_c_2; ++i) {
        out[i].start = in[i].start;
        out[i].end = in[i].end;
        out[i].next = in[i].next;
        out[i].width = in[i].width;
        out[i].minx = in[i].minx;
        out[i].maxx = in[i].maxx;
      }
    
}


void inline_c_NanoVGSpec_6_dc5e0871d245ac0a9a509674eff1e16900be5175(float * cInPtr_inline_c_0, float * cOutPtr_inline_c_1, int n_inline_c_2) {

      float* in = cInPtr_inline_c_0;
      float* out = cOutPtr_inline_c_1;
      for (int i = 0; i < n_inline_c_2; ++i) {
        for (int j = 0; j < 4; ++j) {
          out[4*i+j] = in[4*i+j];
        }
      }
    
}

