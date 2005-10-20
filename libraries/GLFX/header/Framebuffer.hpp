class Framebuffer {
public:
  GLuint Handle;
  int Image;
  Texture* AttachedTexture;
  int Width, Height;

  static inline void doBind(GLuint handle) {
    // this extension is retarded
    GLuint temp;
    glGetIntegerv( GL_FRAMEBUFFER_BINDING_EXT, (GLint*)&temp);
    if (temp == handle) return;
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, handle);
  }

  inline void recreate() {
    GL::endDraw();
    unbind();
    create();
    delete AttachedTexture;
    AttachedTexture = GL::createTexture(Width, Height, false);
    AttachedTexture->flipVertical();
    setNamedTag(Image, Texture, AttachedTexture);
    doBind(Handle);
    attachTexture(*AttachedTexture);
    doBind(0);
  }

  inline void create() {
    GL::endDraw();
    Handle = 0;
    glGenFramebuffersEXT(1, &Handle);
    Global->checkError();
  }

  Framebuffer() {
    Handle = 0;
    create();
  }

  inline bool isValid() {
    GL::endDraw();
    return glIsFramebufferEXT(Handle) != 0;
  }

  inline void bind() {
    GL::endDraw();
    GL::disableTextures();
    if (this->isValid()) {
    } else {
      recreate();
    }
    doBind(Handle);
//    Global->checkError();
  }

  inline static void unbind() {
    GL::endDraw();
    GL::disableTextures();
    doBind(0);
//    Global->checkError();
  }

  inline void attachTexture(Texture& tex) {
    GL::endDraw();
    GL::disableTextures();
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, tex.Handle, 0);
    Global->checkError();
  }

  inline static void detachTexture() {
    GL::endDraw();
    GL::disableTextures();
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, 0, 0);
    Global->checkError();
  }

  ~Framebuffer() {
    GL::endDraw();
    bind();
    detachTexture();
    unbind();
    if (Handle != 0) {
      glDeleteFramebuffersEXT(1, &Handle);
      Handle = 0;
    }
  }
};