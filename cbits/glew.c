#if !defined(darwin_HOST_OS)
#include <GL/glew.h>
#include <stdio.h>
#include "nanovg.h"
#include "math.h"

void initGlew() {
    glewExperimental = GL_TRUE;
    if(glewInit() != GLEW_OK) {
        printf("Could not init glew.\n");
        /* return -1; */
    }
}

#else
void initGlew() {
    // This space intentionally left blank.
}
#endif
