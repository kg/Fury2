#include "../header/GLFX.hpp"

namespace GL {
  int blendMode = -1;
  int scaleMode = -1;
  bool texturesEnabled[4] = {false, false, false, false};
  bool fogEnabled = false;
  GLuint activeTexture[4] = {-1, -1, -1, -1};
  GLenum drawMode = -1;
  Pixel vertexColor = Pixel(0xFFFFFFFF);
  Pixel fogColor = Pixel(0x0);
  Pixel blendColor = Pixel(0x0);

  void init() {
    Global->Context = 0;
    blendMode = -1;
    scaleMode = -1;
    for (int i = 0; i < 4; i++) 
      texturesEnabled[i] = false;
    fogEnabled = false;
    for (int i = 0; i < 4; i++) 
      activeTexture[i] = -1;
    drawMode = -1;
    vertexColor = Pixel(0xFFFFFFFF);
    fogColor = Pixel(0x0);
    blendColor = Pixel(0x0);
    if (GLEW_EXT_blend_color) {
      glBlendColorEXT(0, 0, 0, 0);
    }
    glColor4ub(255, 255, 255, 255);
    setBlendMode<BlendModes::Normal>();
    setScaleMode<ScaleModes::Linear>();
  }

  void flushImageHeap() {
    GLuint handle;
    int image;
    for (unsigned int i = 0; i < Global->ImageHeap.size(); ++i) {
      image = Global->ImageHeap[i];
      handle = getNamedTag(image, Texture);
      if (handle != 0) {
        destroyTexture(handle);
        setNamedTag(image, Texture, 0);
      }
    }
    Global->ImageHeap.clear();
  }

  GLuint createTexture(int width, int height) {
    if (width < 1) return 0;
    if (height < 1) return 0;
    endDraw();
    GLuint handle = 0;
    glGenTextures(1, &handle);
    selectTexture(handle);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    int texWidth = powerOfTwo(width);
    int texHeight = powerOfTwo(height);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, texWidth, texHeight, 0, GL_BGRA_EXT, GL_UNSIGNED_BYTE, 0);
    return handle;
  }

  void copyFramebufferToImage(int image) {
    int width = SoftFX::GetImageWidth(image);
    int height = SoftFX::GetImageHeight(image);
    int yOffset = (Global->OutputHeight - height);
    void* ptr = SoftFX::GetImagePointer(image, 0, 0);
    if (width < 1) return;
    if (height < 1) return;
    if (ptr == Null) return;
    endDraw();
    glFlush();
    for (int y = 0; y < height; ++y) {
      glReadPixels(0, height - y - 1 + yOffset, width, 1, GL_BGRA_EXT, GL_UNSIGNED_BYTE, SoftFX::GetImagePointer(image, 0, y));
    }
  }

  void copyImageToFramebuffer(int image) {
    int width = SoftFX::GetImageWidth(image);
    int height = SoftFX::GetImageHeight(image);
    void* ptr = SoftFX::GetImagePointer(image, 0, 0);
    if (width < 1) return;
    if (height < 1) return;
    if (ptr == Null) return;
    endDraw();
    glDisable(GL_BLEND);
    glRasterPos2i(0, 0);
    glPixelZoom(1, -1);
    glDrawPixels(width, height, GL_BGRA_EXT, GL_UNSIGNED_BYTE, ptr);
    glPixelZoom(1, 1);
    glEnable(GL_BLEND);
  }

  void copyFramebufferToTexture(GLuint handle, int image) {
    int width = SoftFX::GetImageWidth(image);
    int height = SoftFX::GetImageHeight(image);
    int yOffset = (Global->OutputHeight - height);
    FX::Rectangle rect = FX::Rectangle(0, 0, width, height);
    selectTexture(handle);
    endDraw();
    glFlush();
    glDisable(GL_BLEND);
    glPixelZoom(1, -1);
    glRasterPos2i(0, 0);
    glCopyPixels(0, yOffset, width, height, GL_COLOR);
    glPixelZoom(1, 1);
    glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, yOffset, width, height);
    glPixelZoom(1, -1);
    glRasterPos2i(0, 0);
    glCopyPixels(0, yOffset, width, height, GL_COLOR);
    glPixelZoom(1, 1);
    glEnable(GL_BLEND);
    SoftFX::SetImageDirty(image, 0);
  }

  void uploadImageToTexture(GLuint handle, int image) {
    int width = SoftFX::GetImageWidth(image);
    int height = SoftFX::GetImageHeight(image);
    void* ptr = SoftFX::GetImagePointer(image, 0, 0);
    if (width < 1) return;
    if (height < 1) return;
    if (ptr == Null) return;
    selectTexture(handle);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, GL_BGRA_EXT, GL_UNSIGNED_BYTE, ptr);
    SoftFX::SetImageDirty(image, 0);
  }

  GLuint createTextureFromImage(int image) {
    int width = SoftFX::GetImageWidth(image);
    int height = SoftFX::GetImageHeight(image);
    if (width < 1) return 0;
    if (height < 1) return 0;
    GLuint handle = createTexture(width, height);
    setNamedTag(image, Texture, handle);
    uploadImageToTexture(handle, image);
    Global->ImageHeap.push_back(image);
    return handle;
  }

  GLuint createTextureFromFramebuffer(int image) {
    int width = SoftFX::GetImageWidth(image);
    int height = SoftFX::GetImageHeight(image);
    if (width < 1) return 0;
    if (height < 1) return 0;
    GLuint handle = createTexture(width, height);
    setNamedTag(image, Texture, handle);
    copyFramebufferToTexture(handle, image);
    Global->ImageHeap.push_back(image);
    return handle;
  }

  void selectTexture(GLuint handle) {
    selectTextureN<0>(handle);
  }

  void selectImageAsTexture(int image) {
    selectImageAsTextureN<0>(image);
  }

  void destroyTexture(GLuint handle) {
    endDraw();
    glDeleteTextures(1, &handle);
  }

  void beginDraw(GLenum type) {
    if (drawMode == type) return;
    if (drawMode != -1) {
      endDraw();
    }
    glBegin(type);
    drawMode = type;
  }

  void endDraw() {
    if (drawMode != -1) {
      glEnd();
      drawMode = -1;
    }
  }

  void enableTextures() {
    enableTexture<0>();
  }

  void disableTextures() {
    disableTexture<0>();
  }

  void enableFog() {
    if (fogEnabled) return;
    endDraw();
    glEnable(GL_FOG);
    glFogi(GL_FOG_MODE, GL_LINEAR);
    fogEnabled = true;
  }

  void disableFog() {
    if (!fogEnabled) return;
    endDraw();
    glDisable(GL_FOG);
    fogEnabled = false;
  }

  void setVertexColor(Pixel color) {
    vertexColor = color;
    glColor4ub(vertexColor[::Red], vertexColor[::Green], vertexColor[::Blue], vertexColor[::Alpha]);
  }

  void setTextureColor(Pixel color) {
    endDraw();
    float fv[4] = {color[::Red] / 255.0f, color[::Green] / 255.0f, color[::Blue] / 255.0f, color[::Alpha] / 255.0f};
    glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, fv);
  }

  void setFogColor(Pixel color) {
    if (color != fogColor) {
      endDraw();
      fogColor = color;
      float fv[4] = {fogColor[::Red] / 255.0f, fogColor[::Green] / 255.0f, fogColor[::Blue] / 255.0f, fogColor[::Alpha] / 255.0f};
      glFogfv(GL_FOG_COLOR, fv);
    }
  }

  void setFogOpacity(float opacity) {
    endDraw();
    opacity = 1 - opacity;
    glFogf(GL_FOG_END, opacity);
    glFogf(GL_FOG_START, opacity - 1);
  }

  void setBlendColor(Pixel color) {
    if (color != blendColor) {
      endDraw();
      blendColor = color;
      if (GLEW_EXT_blend_color) {
        glBlendColorEXT(blendColor[::Red] / 255.0f, blendColor[::Green] / 255.0f, blendColor[::Blue] / 255.0f, blendColor[::Alpha] / 255.0f);
      }
    }
  }

  void drawLine(FPoint& start, FPoint& end) {
    beginDraw(GL_LINES);
    if (start.X > end.X) {
      glVertex2f(end.X, end.Y);
      glVertex2f(start.X, start.Y);
    } else {
      glVertex2f(start.X, start.Y);
      glVertex2f(end.X, end.Y);
    }
  }

  void drawGradientLine(FPoint& start, FPoint& end, Pixel startColor, Pixel endColor) {
    beginDraw(GL_LINES);
    if (start.X > end.X) {
      setVertexColor(endColor);
      glVertex2f(end.X, end.Y);
      setVertexColor(startColor);
      glVertex2f(start.X, start.Y);
    } else {
      setVertexColor(startColor);
      glVertex2f(start.X, start.Y);
      setVertexColor(endColor);
      glVertex2f(end.X, end.Y);
    }
  }

  void drawRectangle(FX::Rectangle& rect) {
    beginDraw(GL_QUADS);
    glVertex2i(rect.Left, rect.Top);
    glVertex2i(rect.Left + rect.Width, rect.Top);
    glVertex2i(rect.Left + rect.Width, rect.Top + rect.Height);
    glVertex2i(rect.Left, rect.Top + rect.Height);
  }

  void drawGradientRectangle(FX::Rectangle& rect, Pixel colorTL, Pixel colorTR, Pixel colorBL, Pixel colorBR) {
    beginDraw(GL_QUADS);
    setVertexColor(colorTL);
    glVertex2i(rect.Left, rect.Top);
    setVertexColor(colorTR);
    glVertex2i(rect.Left + rect.Width, rect.Top);
    setVertexColor(colorBR);
    glVertex2i(rect.Left + rect.Width, rect.Top + rect.Height);
    setVertexColor(colorBL);
    glVertex2i(rect.Left, rect.Top + rect.Height);
  }

  void drawTexturedRectangle(FX::Rectangle& rect, float U1, float V1, float U2, float V2) {
    beginDraw(GL_QUADS);
    glTexCoord2f(U1, V1);
    glVertex2i(rect.Left, rect.Top);
    glTexCoord2f(U2, V1);
    glVertex2i(rect.Left + rect.Width, rect.Top);
    glTexCoord2f(U2, V2);
    glVertex2i(rect.Left + rect.Width, rect.Top + rect.Height);
    glTexCoord2f(U1, V2);
    glVertex2i(rect.Left, rect.Top + rect.Height);
  }

  void drawTexturedRectangleF(float X, float Y, float W, float H, float U1, float V1, float U2, float V2) {
    beginDraw(GL_QUADS);
    glTexCoord2f(U1, V1);
    glVertex2f(X, Y);
    glTexCoord2f(U2, V1);
    glVertex2f(X + W, Y);
    glTexCoord2f(U2, V2);
    glVertex2f(X + W, Y + H);
    glTexCoord2f(U1, V2);
    glVertex2f(X, Y + H);
  }

  void draw2TexturedRectangle(FX::Rectangle& rect, float U1, float V1, float U2, float V2) {
    beginDraw(GL_QUADS);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U1, V1);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U1, V1);
    glVertex2i(rect.Left, rect.Top);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U2, V1);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U2, V1);
    glVertex2i(rect.Left + rect.Width, rect.Top);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U2, V2);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U2, V2);
    glVertex2i(rect.Left + rect.Width, rect.Top + rect.Height);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U1, V2);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U1, V2);
    glVertex2i(rect.Left, rect.Top + rect.Height);
  }

  void draw2TexturedRectangle(FX::Rectangle& rect, float U11, float V11, float U12, float V12, float U21, float V21, float U22, float V22) {
    beginDraw(GL_QUADS);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U11, V11);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U21, V21);
    glVertex2i(rect.Left, rect.Top);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U12, V11);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U22, V21);
    glVertex2i(rect.Left + rect.Width, rect.Top);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U12, V12);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U22, V22);
    glVertex2i(rect.Left + rect.Width, rect.Top + rect.Height);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U11, V12);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U21, V22);
    glVertex2i(rect.Left, rect.Top + rect.Height);
  }

  void drawBox(FX::Rectangle& box) {
    beginDraw(GL_LINES);
    glVertex2i(box.Left, box.Top + 1);
    glVertex2i(box.Left + box.Width, box.Top + 1);
    glVertex2i(box.Left + box.Width - 1, box.Top + 1);
    glVertex2i(box.Left + box.Width - 1, box.Top + box.Height);
    glVertex2i(box.Left, box.Top + box.Height - 1);
    glVertex2i(box.Left + box.Width - 1, box.Top + box.Height);
    glVertex2i(box.Left, box.Top + box.Height - 1);
    glVertex2i(box.Left, box.Top + 1);
  }
}