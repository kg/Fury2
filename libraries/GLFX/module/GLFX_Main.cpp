#define IMPORT_INITIALIZER
#define IMPORT_DECLARATIONS
#include "../header/GLFX.hpp"
#include <assert.h>

GLFXGlobal* Global;

int __cdecl DllMain(int hModule, int ul_reason_for_call, void* lpReserved)
{
	return 1;
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
  glFlush();
  if (SoftFX::GetImageLocked(Global->Framebuffer) == 0) {
    unlock = true;
    SoftFX::LockImage(Global->Framebuffer);
  }
  glFlush();
  GL::disableTextures();
  GL::selectTexture(0);
  GL::setBlendMode<BlendModes::Normal>();
  GL::setBlendColor(White);
  GL::setVertexColor(White);
  int width = SoftFX::GetImageWidth(Global->Framebuffer);
  int height = SoftFX::GetImageHeight(Global->Framebuffer);
  if ((width != Global->OutputWidth) || (height != Global->OutputHeight)) {
    int texWidth = powerOfTwo(width);
    int texHeight = powerOfTwo(height);
    FX::Rectangle Area = FX::Rectangle(0, 0, Global->OutputWidth, Global->OutputHeight);
    GL::enableTextures();
    GL::selectImageAsTexture(Global->Framebuffer);
    if (Global->ScaleMode == 1) {
      GL::setScaleMode<ScaleModes::Bilinear>();
    } else {
      GL::setScaleMode<ScaleModes::Linear>();
    }
    GL::drawTexturedRectangle(Area, 0, 0, width / (float)texWidth, height / (float)texHeight);
    GL::endDraw();
    glFlush();
    SwapBuffers(Global->DC);
    Area.Width = width;
    Area.Height = height;
    GL::drawTexturedRectangle(Area, 0, 0, width / (float)texWidth, height / (float)texHeight);
    GL::endDraw();
    glFlush();
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
