class GLFXGlobal {
public:
  zeroinit<HWND> Window;
  HDC DC;
  HGLRC Context;
  zeroinit<bool> Windowed;
  std::vector<int> ImageHeap;
  std::vector<TextureGroup*> SmallImageCache;
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
  }

  ~GLFXGlobal() {
    if (Context != Null) {
      wglDeleteContext(Context);
      Context = Null;
    }
  }
};