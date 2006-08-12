class Renderbuffer {
public:
  GLuint Handle;

  Renderbuffer() {
    Handle = 0;
    if (GLEW_EXT_framebuffer_object) {
      glGenRenderbuffersEXT(1, &Handle);
    }
  }

  ~Renderbuffer() {
    if (Handle != 0) {
      if (GLEW_EXT_framebuffer_object) {
        glDeleteRenderbuffersEXT(1, &Handle);
      }
      Handle = 0;
    }
  }
};