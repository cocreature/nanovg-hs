#define NANOVG_GL3_IMPLEMENTATION
// This is used to link the implementation
#if defined(mingw32_HOST_OS)
#include "GL/glew.h"
#elif defined(darwin_HOST_OS)
#include <OpenGL/gl3.h>
#endif

#include "nanovg.h"
#include "nanovg_gl.h"
