#define IMPORT_INITIALIZER
#define IMPORT_DECLARATIONS
#include "../header/DX8FX.hpp"

#pragma comment(lib,"d3d8.lib")
#pragma comment(lib,"d3dx8.lib")

DX8FXGlobal* Global;

int __cdecl DllMain(int hModule, int ul_reason_for_call, void* lpReserved)
{
	return 1;
}

Export int DX8Flip(Device* Device) {
  if (Device == Null) return Failure;
  Device->flip();
  return Success;
}

Export int DX8Init(HWND Window) {
  Global = new DX8FXGlobal();
	SoftFX::Load();
  Global->D3D = Direct3DCreate8(D3D_SDK_VERSION);
  Global->Window = Window;
	InstallOverrides();
	return (Global->D3D != 0);
}

Export int DX8Shutdown() {
  if (Global == Null) return Failure;
	UninstallOverrides();
	SoftFX::Unload();
	delete Global;
  Global = Null;
  return Success;
}
