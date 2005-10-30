#define IMPORT_INITIALIZER
#define IMPORT_DECLARATIONS
#include "../header/GLFX.hpp"
#include <assert.h>

GLFXGlobal* Global;

int __cdecl DllMain(int hModule, int ul_reason_for_call, void* lpReserved)
{
	return 1;
}

Export int GLGetFeatureSupport(const char* FeatureName) {
  if (0 == strcmpi(FeatureName, "glsl")) {
    return GLSL::isSupported();
  } else if (0 == strcmpi(FeatureName, "framebuffer_objects")) {
    return (GLEW_EXT_framebuffer_object != 0);
  } else if (0 == strcmpi(FeatureName, "subtractive_blending")) {
    return (GLEW_EXT_blend_subtract != 0);
  } else if (0 == strcmpi(FeatureName, "advanced_blending")) {
    return (GLEW_ARB_texture_env_combine != 0);
  } else if (0 == strcmpi(FeatureName, "advanced_opacity")) {
    return (GLEW_EXT_blend_color != 0);
  } else if (0 == strcmpi(FeatureName, "multitexturing")) {
    return (GLEW_ARB_multitexture != 0);
  } else if (0 == strcmpi(FeatureName, "lightmap_blending")) {
    return (GLEW_ARB_texture_env_combine != 0) && (GLEW_ARB_multitexture != 0);
  } else if (0 == strcmpi(FeatureName, "deformation")) {
    return (GLSL::isSupported()) && (GLEW_ATI_texture_float != 0);
  } else if (0 == strcmpi(FeatureName, "convolution")) {
    return (GLSL::isSupported());
  } else if (0 == strcmpi(FeatureName, "masking")) {
    return (GLEW_ARB_texture_env_combine != 0) && (GLEW_ARB_multitexture != 0);
  } else if (0 == strcmpi(FeatureName, "grayscale")) {
    return GLSL::isSupported();
  } else if (0 == strcmpi(FeatureName, "invert")) {
    return GLSL::isSupported();
  } else if (0 == strcmpi(FeatureName, "normal_maps")) {
    return GLSL::isSupported();
  } else {
    return -1;
  }
}

Export int GLGetStringLength(const char* String) {
  return strlen(String);
}

Export void* GLAllocateBytes(int Size) {
  return malloc(Size);
}

Export int GLSetShaderLoadCallback(ShaderLoadCallback* callback) {
  if (!Global) return Failure;
  Global->_ShaderLoadCallback = callback;
  return Success;
}

Export GLSL::Program* GLGetShader(char* name) {
  return Global->GetShader(std::string(name));
}

Export int GLCopySurface(int from, int to) {
  if (!Global) return Failure;
  if (from) {
    if (to) {
      GL::copyImageToImage(from, to);
      return Success;
    }
  }
  return Failure;
}

Export int GLSetOutputSize(int width, int height) {
  if (!Global) return Failure;
  Global->OutputWidth = width;
  Global->OutputHeight = height;
  return Success;
}

Export int GLSetScaleMode(int mode) {
  if (!Global) return Failure;
  Global->ScaleMode = mode;
  return Success;
}

Export int GLFlip() {
  if (!Global) return Failure;
  assert(Global->Framebuffer != 0);
  assert(Global->DC != 0);
  bool unlock = false;
  GL::endDraw();
  if (SoftFX::GetImageLocked(Global->Framebuffer) == 0) {
    unlock = true;
    SoftFX::LockImage(Global->Framebuffer);
  }
  GL::disableTextures();
  GL::selectTexture(0);
  GL::setBlendMode<BlendModes::Normal>();
  GL::setBlendColor(White);
  GL::setVertexColor(White);
  Texture *tex;
  int width = SoftFX::GetImageWidth(Global->Framebuffer);
  int height = SoftFX::GetImageHeight(Global->Framebuffer);
  if ((width != Global->OutputWidth) || (height != Global->OutputHeight)) {
    FX::Rectangle Area = FX::Rectangle(0, 0, Global->OutputWidth, Global->OutputHeight);
    GL::enableTextures();
    GL::selectImageAsTexture(Global->Framebuffer);
    tex = GL::getTexture(Global->Framebuffer);
    if (Global->ScaleMode == 1) {
      GL::setScaleMode<ScaleModes::Bilinear>();
    } else {
      GL::setScaleMode<ScaleModes::Linear>();
    }
    GL::drawTexturedRectangle(Area, tex->U1, tex->V1, tex->U2, tex->V2);
    GL::endDraw();
    SwapBuffers(Global->DC);
    Area.Width = width;
    Area.Height = height;
    GL::drawTexturedRectangle(Area, tex->U1, tex->V1, tex->U2, tex->V2);
    GL::endDraw();
  } else {
    SwapBuffers(Global->DC);
  }
  if (unlock) {
    SoftFX::UnlockImage(Global->Framebuffer);
  }
  return Success;
}

Export int GLInit(HWND Window, HDC DC) {
  if (Global != Null) return Failure;
  Global = new GLFXGlobal();
  Global->Window = Window;
  Global->DC = DC;
	SoftFX::Load();
	InstallOverrides();
  return Success;
}

Export int GLShutdown() {
	UninstallOverrides();
  if (Global == Null) return Failure;
	SoftFX::Unload();
	delete Global;
  Global = Null;
  return Success;
}

void GLFXGlobal::CleanupShaders() {
  if (this->GlobalShader != Null) {
    delete this->GlobalShader;
    this->GlobalShader = Null;
  }
  std::map<std::string, GLSL::Program*>::iterator iter = this->Shaders.begin();
  std::map<std::string, GLSL::Program*>::iterator end = this->Shaders.end();
  while (iter != end) {
    delete iter->second;
    ++iter;
  }
  this->Shaders.clear();
}

GLenum GLFXGlobal::checkError() {
  GLenum e = glGetError();
  if (e) {
    int x = abs(-1);
  }
  return e;
}

GLSL::Program* GLFXGlobal::GetShader(std::string& key) {
  if (GLSL::isSupported()) {
    GLSL::Program* program = 0;
    std::map<std::string, GLSL::Program*>::iterator iter = Shaders.find(key);
    if (iter != Shaders.end()) {
      program = iter->second;
      if (program) {
        if (program->isLinked()) {
        } else {
          delete program;
          program = 0;
          iter->second = 0;
        }
      }
    }
    if (program == 0) {
      GLSL::FragmentShader* shader = this->LoadShader(key.c_str());
      if (shader) {
        GLSL::FragmentShader* global = Global->GlobalShader;
        if (global) {
          if (global->isCompiled()) {
          } else {
            global = Null;
          }
        }
        if (global == Null) {
          global = this->LoadShader("global");
          Global->GlobalShader = global;
        }
        program = new GLSL::Program();
        if (global) {
          program->attach(*global);
        }
        program->attach(*shader);
        program->link();
        Shaders[key] = program;
      }
    }
    return program;
  }
  return 0;
}

GLSL::FragmentShader* GLFXGlobal::LoadShader(const char* filename) {
  if (!_ShaderLoadCallback) return 0;
  char* text = 0;
  _ShaderLoadCallback(filename, &text);
  if (text) {
    GLSL::FragmentShader* shader = new GLSL::FragmentShader();
    shader->addSource(text);
    shader->compile();
    free(text);
    text = 0;
    return shader;
  }
  return 0;
}
