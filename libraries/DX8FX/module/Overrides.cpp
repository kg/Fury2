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
}

void UninstallOverrides() {
	removeOverride(Deallocate);
}
