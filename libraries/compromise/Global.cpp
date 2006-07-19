/*
Compromise (COM virtual registration library)
Copyright (C) 2006 Kevin Gadd

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "global.hpp"

static const RegistryRoot RegistryRoots[] = {
  {HKEY_CLASSES_ROOT, L"hkcr"},
  {HKEY_CURRENT_USER, L"hkcu"},
  {HKEY_LOCAL_MACHINE, L"hklm"}
};
static const unsigned RegistryRootCount = 3;

unsigned GetRegistryRootCount() {
  return RegistryRootCount;
}

const RegistryRoot& GetRegistryRoot(unsigned index) {
  return RegistryRoots[index];
}

void InstallTrampolines ();

void VRegInitialize () {
  for (unsigned r = 0; r < RegistryRootCount; r++) {
    pRegistry->Keys[RegistryRoots[r].Name] = VirtualRegKey(RegistryRoots[r].Name);
    RegKeyDefine(unsigned(RegistryRoots[r].ID), get(pRegistry->Keys, RegistryRoots[r].Name));
  }
}

BOOL WINAPI DllMain(
  HINSTANCE hinstDLL,
  DWORD fdwReason,
  LPVOID lpvReserved
  );

class _Global {
public:
  bool initialized;
  VirtualRegistry Registry;
  RegKeyHandles RegistryHandles;
  std::vector<wstring> RegisteredDLLs;

  _Global() {
    pRegistry = &Registry;
    pRegistryHandles = &RegistryHandles;
    initialized = true;
  }
};

static _Global Global;
VirtualRegistry* pRegistry;
RegKeyHandles* pRegistryHandles;
bool TrampolinesInstalled = false;
bool VirtualizeWrites = false;
bool EnableOverrides = false;
HKEY LastEnumKey = 0;
unsigned LastEnumIndex = 0xFFFFFFFF;
unsigned EnumOffset = 0;
unsigned VirtualHandleOffset = VirtualRegKeyBase;

bool GetDLLRegistered(const wstring& Filename) {
  std::vector<wstring>::iterator iter = std::find(Global.RegisteredDLLs.begin(), Global.RegisteredDLLs.end(), Filename);
  if (iter != Global.RegisteredDLLs.end()) {
    return true;
  }
  return false;
}

void SetDLLRegistered(const wstring& Filename, bool State) {
  if (State) {
    SetDLLRegistered(Filename, false);
    Global.RegisteredDLLs.push_back(Filename);
  } else {
    std::vector<wstring>::iterator iter = std::find(Global.RegisteredDLLs.begin(), Global.RegisteredDLLs.end(), Filename);
    if (iter != Global.RegisteredDLLs.end()) {
      Global.RegisteredDLLs.erase(iter);
    }
  }
}

Export inline int IsSupported() {
  OSVERSIONINFO info;
  memset(&info, 0, sizeof(OSVERSIONINFO));
  info.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx(&info);
  return (info.dwPlatformId >= 2);
}

Export int Initialize() {
  if (!EnableOverrides) {
    if (IsSupported()) {
      InstallTrampolines();
      VRegInitialize();
      EnableOverrides = true;
      OutputDebugString("Initialize()=1\n");
      return Success;
    }
    OutputDebugString("Initialize()=0\n");
    return Failure;
  } else {
    return Failure;
  }
}

Export int Uninitialize() {
  if (EnableOverrides) {
    OutputDebugString("Uninitialize()\n");
    EnableOverrides = false;
    return Success;
  } else {
    return Failure;
  }
}