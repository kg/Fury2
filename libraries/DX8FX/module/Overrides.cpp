#include "../header/DX8FX.hpp"

using namespace SoftFX;

defOverride(Allocate_Device) {
	readParam(int, Image, 0);
	readParam(int, Width, 1);
	readParam(int, Height, 2);
  Device* device = new Device(Width, Height);
  setNamedTag(Image, Device, reinterpret_cast<int>(device));
	return Success;
}

defOverride(Allocate_RenderTarget) {
	readParam(int, Image, 0);
	readParam(int, Width, 1);
	readParam(int, Height, 2);
  setNamedTag(Image, RenderTarget, Null);
	return Success;
}

defOverride(FilterSimple_Fill) {
  readParam(int, Image, 0);
  readParam(FX::Rectangle*, Area, 1);
  readParam(Pixel, Color, 2);
  SoftFX::BypassOverrides(1);
  SoftFX::FilterSimple_Fill(Image, Area, Pixel(255, 0, 0, 255));
  SoftFX::BypassOverrides(-1);
  return Success;
}

defOverride(Deallocate) {
	readParam(int, Image, 0);
	if (checkNamedTag(Image, Device)) {
		return Success;
	} else if (checkNamedTag(Image, RenderTarget)) {
		return Success;
	}
	return Failure;
}

void InstallOverrides() {
	addOverride(Deallocate);
  addOverride(FilterSimple_Fill);
}

void UninstallOverrides() {
	removeOverride(Deallocate);
  removeOverride(FilterSimple_Fill);
}
