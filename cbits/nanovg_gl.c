#define NANOVG_GL3_IMPLEMENTATION
// This is used to link the implementation
#if defined(darwin_HOST_OS)
#include <OpenGL/gl3.h>
#include "nanovg.h"
#include "nanovg_gl.h"
#else
#include "GL/glew.h"
#include "nanovg.h"
#include "nanovg_gl.h"
#endif
