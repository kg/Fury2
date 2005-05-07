namespace GL {
  extern int blendMode;
  extern bool texturesEnabled[4];
  extern bool fogEnabled;
  extern GLuint activeTexture[4];
  extern GLenum drawMode;
  extern Pixel vertexColor;
  extern Pixel fogColor;
  extern Pixel blendColor;

  inline Texture* getTexture(int Image) {
    return reinterpret_cast<Texture*>(getNamedTag(Image, Texture));
  }

  extern void init();

  extern void flushImageHeap();

  extern Texture* createTexture(int width, int height);
  extern Texture* createTextureFromImage(int image);
  extern Texture* createTextureFromFramebuffer(int image);
  extern void copyImageToImage(int from, int to);
  extern void copyImageToFramebuffer(int image);
  extern void copyFramebufferToImage(int image);
  extern void copyFramebufferToTexture(Texture *tex, int image);
  extern void uploadImageToTexture(Texture *tex, int image);
  extern void selectImageAsTexture(int image);
  extern void selectTexture(GLuint handle);
  extern void destroyTexture(GLuint handle);

  extern void beginDraw(GLenum type);
  extern void endDraw();

  extern void enableTextures();
  extern void disableTextures();

  extern void enableFog();
  extern void disableFog();
  extern void setFogOpacity(float opacity);

  extern void setVertexColor(Pixel color);
  extern void setFogColor(Pixel color);
  extern void setBlendColor(Pixel color);
  extern void setTextureColor(Pixel color);

  inline void setFog(Pixel color) {
    if (color[::Alpha]) {
      enableFog();
      setFogColor(color);
      setFogOpacity(color[::Alpha] / 255.0f);
    } else {
      disableFog();
    }
  }

  extern void drawLine(FPoint& start, FPoint& end);
  extern void drawGradientLine(FPoint& start, FPoint& end, Pixel startColor, Pixel endColor);
  extern void drawRectangle(FX::Rectangle& rect);
  extern void drawGradientRectangle(FX::Rectangle& rect, Pixel colorTL, Pixel colorTR, Pixel colorBL, Pixel colorBR);
  extern void drawTexturedRectangle(FX::Rectangle& rect, float U1, float V1, float U2, float V2);
  extern void drawTexturedRectangleF(float X, float Y, float W, float H, float U1, float V1, float U2, float V2);
  extern void drawTexturedRectangleTiledF(float X, float Y, float W, float H, float SW, float SH, float U1, float V1, float U2, float V2);
  extern void draw2TexturedRectangle(FX::Rectangle& rect, float U1, float V1, float U2, float V2);
  extern void draw2TexturedRectangle(FX::Rectangle& rect, float U11, float V11, float U12, float V12, float U21, float V21, float U22, float V22);
  extern void drawBox(FX::Rectangle& box);
  inline void drawRectangle(FX::Rectangle* rect) {
    drawRectangle(*rect);
  }
  inline void drawGradientRectangle(FX::Rectangle* rect, Pixel colorTL, Pixel colorTR, Pixel colorBL, Pixel colorBR) {
    drawGradientRectangle(*rect, colorTL, colorTR, colorBL, colorBR);
  }
  inline void drawTexturedRectangle(FX::Rectangle* rect, float U1, float V1, float U2, float V2) {
    drawTexturedRectangle(*rect, U1, V1, U2, V2);
  }
  inline void drawBox(FX::Rectangle* box) {
    drawBox(*box);
  }

  template <int Stage> void selectImageAsTextureN(int image) {
    switchTextureStage<Stage>();
    Texture* tex = 0;
    GLuint handle = 0;
    tex = getTexture(image);
    if (tex) handle = tex->Handle;
    if (handle == 0) {
      if (checkNamedTag(image, Context)) {
        // this is a context
        endDraw();
        tex = createTextureFromFramebuffer(image);
        if (tex) handle = tex->Handle;
      } else {
        // this is an image
        endDraw();
        tex = createTextureFromImage(image);
        if (tex) handle = tex->Handle;
      }
    } else {
      if (checkNamedTag(image, Context)) {
        // this is a context
        if (SoftFX::GetImageDirty(image)) {
          // the texture is out of sync with the framebuffer
          endDraw();
          copyFramebufferToTexture(tex, image);
        }
      } else {
        // this is an image
        if (SoftFX::GetImageDirty(image)) {
          // this image's texture is out of sync with system memory
          endDraw();
          uploadImageToTexture(tex, image);
        }
      }
    }
    selectTextureN<Stage>(handle);
  }

  template <int Stage> void selectTextureN(GLuint handle) {
    if (handle != activeTexture[Stage]) {
      endDraw();
      switchTextureStage<Stage>();
      glBindTexture(GL_TEXTURE_2D, handle);
      activeTexture[Stage] = handle;
    }
  }

  template <int Stage> void enableTexture() {
    switchTextureStage<Stage>();
    if (texturesEnabled[Stage]) return;
    endDraw();
    glEnable(GL_TEXTURE_2D);
    texturesEnabled[Stage] = true;
  }

  template <int Stage> void disableTexture() {
    switchTextureStage<Stage>();
    if (!texturesEnabled[Stage]) return;
    endDraw();
    glDisable(GL_TEXTURE_2D);
    texturesEnabled[Stage] = false;
  }

  template <class Mode> inline void setBlendMode() {
    if (blendMode != Mode::id) {
      endDraw();
      Mode::Set();
      blendMode = Mode::id;
    }
  }

  template <class Mode> inline void setScaleMode() {
    endDraw();
    Mode::Set();
  }

  inline void selectContext(int Image) {
	  if (checkNamedTag(Image, Context)) {
      if (Global->Context != (HGLRC)getNamedTag(Image, Context)) {
        Global->Context = (HGLRC)getNamedTag(Image, Context);
        wglMakeCurrent(Global->DC, Global->Context);
      }
    }
  }

  template <int Stage> inline void switchTextureStage() {
    if (GLEW_ARB_multitexture) {
      glActiveTextureARB(GL_TEXTURE0_ARB);
    }
  }

  template <> inline void switchTextureStage<0>() {
    if (GLEW_ARB_multitexture) {
      glActiveTextureARB(GL_TEXTURE0_ARB);
    }
  }

  template <> inline void switchTextureStage<1>() {
    if (GLEW_ARB_multitexture) {
      glActiveTextureARB(GL_TEXTURE1_ARB);
    }
  }

  template <> inline void switchTextureStage<2>() {
    if (GLEW_ARB_multitexture) {
      glActiveTextureARB(GL_TEXTURE2_ARB);
    }
  }

  template <> inline void switchTextureStage<3>() {
    if (GLEW_ARB_multitexture) {
      glActiveTextureARB(GL_TEXTURE3_ARB);
    }
  }
}