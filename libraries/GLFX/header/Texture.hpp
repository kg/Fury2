namespace GL {
  void freeTexture(GLuint handle);
}

struct Texture {
  GLuint Handle;
  Texture* IsolatedTexture;
  float U1, V1, U2, V2, XScale, YScale;
  int Left, Top, Width, Height;
  bool MatteOptimized;
  bool Owner;
  bool FlippedVertically;
  int ScaleMode;

  Texture(GLuint handle = 0, int left = 0, int top = 0, int width = 0, int height = 0, float xs = 0, float ys = 0, bool owner = true) {
    Handle = handle;
    IsolatedTexture = 0;
    XScale = xs;
    YScale = ys;
    Left = left;
    Top = top;
    Width = width;
    Height = height;
    U1 = left * XScale;
    V1 = top * YScale;
    U2 = (left + width) * XScale;
    V2 = (top + height) * YScale;
    MatteOptimized = false;
    FlippedVertically = false;
    Owner = owner;
    ScaleMode = -1;
  }

  ~Texture() {
    if (IsolatedTexture != 0) {
      delete IsolatedTexture;
      IsolatedTexture = 0;
    }
    if (Owner) {
      if (Handle != 0) {
        GL::freeTexture(Handle);
        glDeleteTextures(1, &Handle);
        Handle = 0;
      }
    }
  }

  inline void flipVertical() {
    FlippedVertically = !FlippedVertically;
    if (FlippedVertically) {
      V2 = Top * abs(YScale);
      V1 = (Top + Height) * abs(YScale);
    } else  {
      V1 = Top * abs(YScale);
      V2 = (Top + Height) * abs(YScale);
    }
    YScale = -YScale;
  }

  inline float U(float X) {
    return U1 + (X * XScale);
  }

  inline float V(float Y) {
    return V1 + (Y * YScale);
  }

  bool isInvalid() {
    glEnd();
    GLboolean temporary;
    glAreTexturesResident(1, &Handle, &temporary);
    GLenum e = glGetError();
    if (e == GL_INVALID_VALUE) {
      return true;
    }
    return false;
  }
};

struct TextureGroupItem {
  int Left, Top, Width, Height;
  Texture* Pointer;
};

struct TextureGroup {
public:
  Texture* CacheTexture;
  std::vector<TextureGroupItem> Items;

  TextureGroup(Texture* texture) {
    CacheTexture = texture;
  }

  inline bool findFreeSpot(int width, int height, int& x, int& y) {
    FX::Rectangle newItem, theItem;
    x = y = 0;
    newItem.setValues(x, y, width, height);
    bool valid;
    while (true) {
      valid = true;
      for (unsigned int i = 0; i < Items.size(); i++) {
        newItem.Left = x;
        newItem.Top = y;
        theItem.setValues(Items[i].Left, Items[i].Top, Items[i].Width, Items[i].Height);
        if (theItem.intersect(newItem)) {
          if (Items[i].Pointer) {
            valid = false;
            break;
          }
        }
      }
      if (valid) {
        return true;
      }
      x += width;
      if ((x + width) > CacheTexture->Width) {
        x = 0;
        y += height;
      }
      if ((y + height) > CacheTexture->Height) {
        return false;
      }
    }
  }

  inline Texture* fillSpot(int x, int y, int w, int h) {
    Texture* newTexture = new Texture(CacheTexture->Handle, x, y, w, h, CacheTexture->XScale, CacheTexture->YScale, false);
    TextureGroupItem newItem;
    newItem.Left = x;
    newItem.Top = y;
    newItem.Width = w;
    newItem.Height = h;
    newItem.Pointer = newTexture;
    Items.push_back(newItem);
    return newTexture;
  }

  inline void freeSpot(Texture* texture) {
    for (unsigned int i = 0; i < Items.size(); i++) {
      if (Items[i].Pointer == texture) {
        Items[i].Pointer = 0;
        break;
      }
    }
    return;
  }
};