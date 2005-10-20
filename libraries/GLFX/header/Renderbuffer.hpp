class Renderbuffer {
public:
  GLuint Handle;

  Renderbuffer() {
    Handle = 0;
    glGenRenderbuffersEXT(1, &Handle);
  }

  ~Renderbuffer() {
    if (Handle != 0) {
      glDeleteRenderbuffersEXT(1, &Handle);
      Handle = 0;
    }
  }
};