#define NANOVG_GL3_IMPLEMENTATION
// This is used to link the implementation
#if defined(darwin_HOST_OS)
#include <OpenGL/gl3.h>
#else
#include "GL/glew.h"
#endif
#include "nanovg.h"
#include "nanovg_gl.h"
