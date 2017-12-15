#if defined(GLES_3)
#define NANOVG_GLES3_IMPLEMENTATION
#else
#define NANOVG_GL3_IMPLEMENTATION
#endif
// This is used to link the implementation
#if defined(darwin_HOST_OS)
#include <OpenGL/gl3.h>
#else
#include "GL/glew.h"
#endif
#include "nanovg.h"
#include "nanovg_gl.h"
