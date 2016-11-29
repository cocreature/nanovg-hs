#if defined(mingw32_HOST_OS)
#include <GL/glew.h>
#endif
#include <stdio.h>
#include "nanovg.h"
#include "math.h"

void initGlew() {
	#if defined(mingw32_HOST_OS)
    glewExperimental = GL_TRUE;
    if(glewInit() != GLEW_OK) {
        printf("Could not init glew.\n");
        /* return -1; */
    }
    #endif
}
