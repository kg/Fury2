class GLFXGlobal {
public:
  zeroinit<HWND> Window;
  HDC DC;
  HGLRC Context;
  zeroinit<bool> Windowed;
  std::vector<int> ImageHeap;
  std::vector<TextureGroup*> SmallImageCache;
  Texture* RenderTexture;
  int RenderFunction;
  int Framebuffer;
  int OutputWidth, OutputHeight;
  int ScaleMode;

  GLFXGlobal() {
    DC = Null;
    Context = Null;
    OutputWidth = 0;
    OutputHeight = 0;
    ScaleMode = 0;
    Framebuffer = 0;
    RenderTexture = 0;
    RenderFunction = 0;
  }

  ~GLFXGlobal() {
    if (RenderTexture != Null) {
      delete RenderTexture;
      RenderTexture = Null;
    }
    if (Context != Null) {
      wglDeleteContext(Context);
      Context = Null;
    }
  }
};