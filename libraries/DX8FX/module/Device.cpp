#include "../header/DX8FX.hpp"

Device::Device(int Width, int Height) {
  _device = 0;
  init(Width, Height);
  Global->Devices.push_back(this);
}

Device::~Device() {
  DeviceVector::iterator iter = std::find(Global->Devices.begin(), Global->Devices.end(), this);
  if (iter != Global->Devices.end()) {
    Global->Devices.erase(iter);
  }
  uninit();
}

bool Device::init(int Width, int Height) {
	D3DPRESENT_PARAMETERS ppPresent;
	LPDIRECT3DDEVICE8 devDevice = 0;
	ZeroMemory(&ppPresent, sizeof(ppPresent));

	ppPresent.Windowed = Global->Windowed;
	ppPresent.SwapEffect = D3DSWAPEFFECT_DISCARD;
	ppPresent.BackBufferFormat = D3DFMT_A8R8G8B8;
	ppPresent.BackBufferWidth = Width;
	ppPresent.BackBufferHeight = Height;
	ppPresent.BackBufferCount = 1;

	Global->D3D->CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, Global->Window, D3DCREATE_SOFTWARE_VERTEXPROCESSING, &ppPresent, &devDevice);

	if (devDevice) {
		_width = Width;
		_height = Height;
		_windowed = Global->Windowed;
		_device = devDevice;
		return true;
	}

	return false;
}

void Device::uninit() {
	if (!_textures.empty()) {
		// destroy textures
	}
	if (_device) {
		COMDestroy(_device);
	}
}

void Device::setTexture(Texture *newTexture, int stage) {
	return;
}

Texture* Device::getTexture(int stage) {
	return Null;
};

void Device::setRenderTarget(Texture *newTarget) {
	return;
}

Texture* Device::getDefaultRenderTarget() {
	return Null;
}

Texture* Device::getRenderTarget() {
	return Null;
};

bool Device::ready() {
	return (_device != Null);
}

template <class Type> void Device::setVertexFormat(Type& Vertex) {
	if (ready()) {
		_device->SetVertexShader(getVertexShader<Type>());
	}
	return;
}

template <class Type> void drawTrangles(Type* Vertexes, int Count) {
	if (ready()) {
		if (Count >= 1) {
			_device->DrawPrimitiveUP(D3DPT_TRIANGLELIST, Count, (void*)Vertexes, sizeof(Type));
		}
	}
	return;
}

template <class Type> void drawLines(Type* Vertexes, int Count) {
	if (ready()) {
		if (Count >= 1) {
			_device->DrawPrimitiveUP(D3DPT_LINELIST, Count, (void*)Vertexes, sizeof(Type));
		}
	}
	return;
}