typedef vector<Device*> DeviceVector;

class DX8FXGlobal {
public:
  zeroinit<HWND> Window;
  zeroinit<bool> Windowed;
  LPDIRECT3D8 D3D;
  DeviceVector Devices;

  DX8FXGlobal() {
    D3D = Null;
    
  }

  ~DX8FXGlobal() {
    if (D3D != Null) {
      Device* device;
      while (Devices.size() > 0) {
        device = Devices[0];
        Devices[0] = Null;
        delete device;
      }
      COMDestroy(D3D);
      D3D = Null;
    }
  }
};