class GLFXGlobal {
public:
  zeroinit<HWND> Window;
  HDC DC;
  HGLRC Context;
  zeroinit<bool> Windowed;
  std::vector<int> ImageHeap;
  std::vector<TextureGroup*> SmallImageCache;
  std::map<std::string, GLSL::Program*> Shaders;
  Texture* RenderTexture;
  Texture* MeshTexture;
  int RadialImage;
  int RenderFunction;
  int Framebuffer;
  int OutputWidth, OutputHeight;
  int ScaleMode;
  GLSL::FragmentShader* GlobalShader;
  GLSL::FragmentShader* NullShader;
  ShaderLoadCallback* _ShaderLoadCallback;
  ShaderFailCallback* _ShaderFailCallback;
  float FloatLookup[256];

  GLFXGlobal() {
    DC = Null;
    Context = Null;
    OutputWidth = 0;
    OutputHeight = 0;
    ScaleMode = 0;
    Framebuffer = 0;
    RenderTexture = 0;
    MeshTexture = 0;
    RadialImage = 0;
    RenderFunction = 0;
    _ShaderLoadCallback = 0;
    _ShaderFailCallback = 0;
    GlobalShader = 0;
    NullShader = 0;
    for (int i = 0; i < 255; i++) {
      FloatLookup[i] = float(i) / 255.0f;
    }
    FloatLookup[0] = 0.0f;
    FloatLookup[255] = 1.0f;
  }

  inline void ColorToFloat4(Pixel color, float* out) {
    out[0] = FloatLookup[color[::Red]];
    out[1] = FloatLookup[color[::Green]];
    out[2] = FloatLookup[color[::Blue]];
    out[3] = FloatLookup[color[::Alpha]];
  }
  
  void CleanupShaders();

  GLSL::Program* GetShader(std::string& key);

  GLSL::FragmentShader* LoadShader(const char* filename);

  inline void GenerateRadialImage() {
    if (RadialImage) {
    } else {
      RadialImage = SoftFX::AllocateImage(256, 256);
      FX::Rectangle rctArea;
      rctArea.setValues(-256, -256, 512, 512);
      SoftFX::FilterSimple_Gradient_Radial(RadialImage, &rctArea, Pixel(255, 255, 255, 255), Pixel(255, 255, 255, 0));
    }
  }

  GLenum checkError();

  ~GLFXGlobal() {
    CleanupShaders();
    if (RadialImage != Null) {
      SoftFX::DeallocateImage(RadialImage);
      RadialImage = Null;
    }
    if (RenderTexture != Null) {
      delete RenderTexture;
      RenderTexture = Null;
    }
    if (MeshTexture != Null) {
      delete MeshTexture;
      MeshTexture = Null;
    }
    if (Context != Null) {
      wglDeleteContext(Context);
      Context = Null;
    }
    _ShaderLoadCallback = Null;
  }
};