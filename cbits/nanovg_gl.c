#if defined(GLES_3)
#define NANOVG_GLES3_IMPLEMENTATION
#elif defined(GL_2)
#define NANOVG_GL2_IMPLEMENTATION
#else
#define NANOVG_GL3_IMPLEMENTATION
#endif
// This is used to link the implementation
#if defined(darwin_HOST_OS)
#if defined(GL_2)
#include <OpenGL/gl.h>
#else
#include <OpenGL/gl3.h>
#endif
#else
#include "GL/glew.h"
#endif
#include "nanovg.h"
#include "nanovg_gl.h"
