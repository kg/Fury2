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
    Texture* handle;
    int image;
    for (unsigned int i = 0; i < Global->ImageHeap.size(); ++i) {
      image = Global->ImageHeap[i];
      handle = getTexture(image);
      if (handle) {
        delete handle;
        setNamedTag(image, Texture, 0);
      }
    }
    Global->ImageHeap.clear();
    for (unsigned int i = 0; i < Global->SmallImageCache.size(); ++i) {
      handle = Global->SmallImageCache[i]->CacheTexture;
      if (handle) {
        delete handle;
      }
      delete Global->SmallImageCache[i];
    }
    Global->SmallImageCache.clear();
  }

  Texture* createSmallTexture(int width, int height) {
    if (width > SmallTextureSize) return 0;
    if (height > SmallTextureSize) return 0;
    int x, y, w, h;
    w = powerOfTwo(width);
    h = powerOfTwo(height);
    for (unsigned int i = 0; i < Global->SmallImageCache.size(); i++) {
      if (Global->SmallImageCache[i]->findFreeSpot(w, h, x, y)) {
        return Global->SmallImageCache[i]->fillSpot(x, y, width, height);
      }
    }
    Texture* tex = createTexture(CacheTextureSize, CacheTextureSize);
    TextureGroup* newGroup = new TextureGroup(tex);
    Global->SmallImageCache.push_back(newGroup);
    return newGroup->fillSpot(0, 0, width, height);
  }

  Texture* createTexture(int width, int height, bool enableCache) {
    if (width < 1) return 0;
    if (height < 1) return 0;
    endDraw();
    GLuint handle = 0;
    if (enableCache) {
      if ((width <= SmallTextureSize) && (height <= SmallTextureSize)) {
        Texture* tex = createSmallTexture(width, height);
        if (tex) return tex;
      }
    }
    glGenTextures(1, &handle);
    selectTexture(handle);
    setScaleMode<ScaleModes::Linear>();
    int texWidth = powerOfTwo(width);
    int texHeight = powerOfTwo(height);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, texWidth, texHeight, 0, GL_BGRA_EXT, GL_UNSIGNED_BYTE, 0);
    return new Texture(handle, 0, 0, width, height, 1.0f / texWidth, 1.0f / texHeight);
  }

  Texture* createTexture(int width, int height) {
    return createTexture(width, height, true);
  }

  void copyImageToImage(int from, int to) {
    if (from == to) return;
    if (checkNamedTag(from, Context)) {
      // from context
      if (checkNamedTag(to, Context)) {
        // to context
      } else {
        // to image
        Texture* tex = getTexture(to);
        if (tex == 0) tex = createTextureFromImage(to, true);
        copyFramebufferToTexture(tex, to);
      }
    } else {
      // from image
      if (checkNamedTag(to, Context)) {
        // to context
        // copyTextureToFramebuffer(getTexture(from), from);
        copyImageToFramebuffer(from);
      } else {
        // to image
      }
    }
    SoftFX::SetImageDirty(to, 0);
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

  void copyFramebufferToTexture(Texture *tex, int image) {
    int width = SoftFX::GetImageWidth(image);
    int height = SoftFX::GetImageHeight(image);
    int yOffset = (Global->OutputHeight - height);
    FX::Rectangle rect = FX::Rectangle(0, 0, width, height);
    enableTexture<0>();
    disableTexture<1>();
    selectTexture(tex->Handle);
    endDraw();
    glFlush();
    setBlendMode<BlendModes::Normal>();
    glDisable(GL_BLEND);
    glCopyTexSubImage2D(GL_TEXTURE_2D, 0, tex->Left, tex->Top, 0, yOffset, width, height);
    drawTexturedRectangleF(0, 0, width, height, tex->U1, tex->V1, tex->U2, tex->V2);
    endDraw();
    glCopyTexSubImage2D(GL_TEXTURE_2D, 0, tex->Left, tex->Top, 0, yOffset, width, height);
    drawTexturedRectangleF(0, 0, width, height, tex->U1, tex->V1, tex->U2, tex->V2);
    endDraw();
    glEnable(GL_BLEND);
    SoftFX::SetImageDirty(image, 0);
  }

  void uploadImageToTexture(Texture *tex, int image) {
    int width = SoftFX::GetImageWidth(image);
    int height = SoftFX::GetImageHeight(image);
    void* ptr = SoftFX::GetImagePointer(image, 0, 0);
    if (width < 1) return;
    if (height < 1) return;
    if (ptr == Null) return;
    selectTexture(tex->Handle);
    glTexSubImage2D(GL_TEXTURE_2D, 0, tex->Left, tex->Top, width, height, GL_BGRA_EXT, GL_UNSIGNED_BYTE, ptr);
    if (tex->IsolatedTexture != 0) {
      selectTexture(tex->IsolatedTexture->Handle);
      glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, GL_BGRA_EXT, GL_UNSIGNED_BYTE, ptr);
    }
    SoftFX::SetImageDirty(image, 0);
  }

  Texture* createTextureFromImage(int image, bool enableCache) {
    int width = SoftFX::GetImageWidth(image);
    int height = SoftFX::GetImageHeight(image);
    if (width < 1) return 0;
    if (height < 1) return 0;
    Texture* tex = createTexture(width, height, enableCache);
    setNamedTag(image, Texture, tex);
    uploadImageToTexture(tex, image);
    Global->ImageHeap.push_back(image);
    return tex;
  }

  Texture* createTextureFromFramebuffer(int image, bool enableCache) {
    int width = SoftFX::GetImageWidth(image);
    int height = SoftFX::GetImageHeight(image);
    if (width < 1) return 0;
    if (height < 1) return 0;
    Texture* tex = createTexture(width, height, enableCache);
    setNamedTag(image, Texture, tex);
    copyFramebufferToTexture(tex, image);
    Global->ImageHeap.push_back(image);
    return tex;
  }

  void selectTexture(GLuint handle) {
    selectTextureN<0>(handle);
  }

  void selectImageAsTexture(int image) {
    selectImageAsTextureN<0>(image);
  }

  void selectImageAsIsolatedTexture(int image) {
    selectImageAsIsolatedTextureN<0>(image);
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

  void drawArray(GLenum type, Vertex* pointer, int count) {
    if (drawMode != -1) {
      endDraw();
    }
    glVertexPointer(2, GL_FLOAT, 8, pointer);
    glEnableClientState(GL_VERTEX_ARRAY);
    glDrawArrays(type, 0, count);
    glDisableClientState(GL_VERTEX_ARRAY);
  }

  void drawArray(GLenum type, Vertex1T* pointer, int count) {
    if (drawMode != -1) {
      endDraw();
    }
    glVertexPointer(2, GL_FLOAT, 16, pointer);
    glTexCoordPointer(2, GL_FLOAT, 16, ((Byte*)pointer) + 8);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glDrawArrays(type, 0, count);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
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
    glFogf(GL_FOG_END, 1 - opacity);
    glFogf(GL_FOG_START, -opacity);
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
    float x = rect.Left, y = rect.Top, x2 = rect.Left + rect.Width, y2 = rect.Top + rect.Height;
    glVertex2f(x, y);
    glVertex2f(x2, y);
    glVertex2f(x2, y2);
    glVertex2f(x, y2);
  }

  void drawGradientRectangle(FX::Rectangle& rect, Pixel colorTL, Pixel colorTR, Pixel colorBL, Pixel colorBR) {
    beginDraw(GL_QUADS);
    float x = rect.Left, y = rect.Top, x2 = rect.Left + rect.Width, y2 = rect.Top + rect.Height;
    setVertexColor(colorTL);
    glVertex2f(x, y);
    setVertexColor(colorTR);
    glVertex2f(x2, y);
    setVertexColor(colorBR);
    glVertex2f(x2, y2);
    setVertexColor(colorBL);
    glVertex2f(x, y2);
  }

  void drawTexturedRectangle(FX::Rectangle& rect, float U1, float V1, float U2, float V2) {
    beginDraw(GL_QUADS);
    float x = rect.Left, y = rect.Top, x2 = rect.Left + rect.Width, y2 = rect.Top + rect.Height;
    glTexCoord2f(U1, V1);
    glVertex2f(x, y);
    glTexCoord2f(U2, V1);
    glVertex2f(x2, y);
    glTexCoord2f(U2, V2);
    glVertex2f(x2, y2);
    glTexCoord2f(U1, V2);
    glVertex2f(x, y2);
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

  void drawTexturedLineF(float X1, float Y1, float X2, float Y2, float U1, float V1, float U2, float V2) {
    beginDraw(GL_LINES);
    glTexCoord2f(U1, V1);
    glVertex2f(X1, Y1);
    glTexCoord2f(U2, V2);
    glVertex2f(X2, Y2);
  }

  void drawTexturedRectangleTiledF(float X, float Y, float W, float H, float SW, float SH, float U1, float V1, float U2, float V2) {
    float cx, cy, ex = X + W, ey = Y + H;
    float cx2, cy2;
    beginDraw(GL_QUADS);
    for (cy = Y; cy < ey; cy += SH) {
      for (cx = X; cx < ex; cx += SW) {
        cx2 = cx + SW;
        cy2 = cy + SH;
        if (cx2 > ex) cx2 = ex;
        if (cy2 > ey) cy2 = ey;
        glTexCoord2f(U1, V1);
        glVertex2f(cx, cy);
        glTexCoord2f(U2, V1);
        glVertex2f(cx2, cy);
        glTexCoord2f(U2, V2);
        glVertex2f(cx2, cy2);
        glTexCoord2f(U1, V2);
        glVertex2f(cx, cy2);
      }
    }
  }

  void draw2TexturedRectangle(FX::Rectangle& rect, float U1, float V1, float U2, float V2) {
    beginDraw(GL_QUADS);
    float x = rect.Left, y = rect.Top, x2 = rect.Left + rect.Width, y2 = rect.Top + rect.Height;
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U1, V1);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U1, V1);
    glVertex2f(x, y);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U2, V1);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U2, V1);
    glVertex2f(x2, y);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U2, V2);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U2, V2);
    glVertex2f(x2, y2);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U1, V2);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U1, V2);
    glVertex2f(x, y2);
  }

  void draw2TexturedRectangle(FX::Rectangle& rect, float U11, float V11, float U12, float V12, float U21, float V21, float U22, float V22) {
    beginDraw(GL_QUADS);
    float x = rect.Left, y = rect.Top, x2 = rect.Left + rect.Width, y2 = rect.Top + rect.Height;
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U11, V11);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U21, V21);
    glVertex2f(x, y);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U12, V11);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U22, V21);
    glVertex2f(x2, y);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U12, V12);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U22, V22);
    glVertex2f(x2, y2);
    glMultiTexCoord2fARB(GL_TEXTURE0_ARB, U11, V12);
    glMultiTexCoord2fARB(GL_TEXTURE1_ARB, U21, V22);
    glVertex2f(x, y2);
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