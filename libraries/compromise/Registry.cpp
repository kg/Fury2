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

extern unsigned GetRegistryRootCount();
extern const RegistryRoot& GetRegistryRoot(unsigned index);

extern LONG WINAPI Real_RegOpenKeyExW(HKEY a0, LPCWSTR a1,
  DWORD a2, REGSAM a3, PHKEY a4);

unsigned RegKeyDefine(VirtualRegKey *Key) {
  unsigned id = VirtualHandleOffset;
  VirtualHandleOffset++;
  RegKeyHandle handle(id, Key, Key->Name);
  (*pRegistryHandles)[id] = handle;
  return id;
}

void RegKeyDefine(unsigned newID, unsigned oldID) {
  RegKeyHandles::iterator iter = pRegistryHandles->find(oldID);
  if (iter != pRegistryHandles->end()) {
    (*pRegistryHandles)[newID] = iter->second;
    (*pRegistryHandles)[newID].ID = newID;
  }
}

void RegKeyDefine(unsigned ID, VirtualRegKey *Key) {
  RegKeyHandle handle(ID, Key, Key->Name);
  (*pRegistryHandles)[ID] = handle;
}

void RegKeyNameDefine(unsigned ID, wstring Name) {
  RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
  if (iter != pRegistryHandles->end()) {
    // Already defined
  } else {
    RegKeyHandle handle(ID, Name);
    (*pRegistryHandles)[ID] = handle;
  }
  return;
}

const wstring& RegKeyNameResolve(unsigned ID) {
  wstringstream *buffer;
  wstring *str;
  RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
  if (iter != pRegistryHandles->end()) {
    return iter->second.Name;
  } else {
    // this leaks objects.
    buffer = new wstringstream();
    *buffer << "0x";
    *buffer << (void*)ID;
    str = new wstring(buffer->str());
    delete buffer;
    return *str;
  }
}

VirtualRegKey* RegKeyGetPtr(unsigned ID) {
  RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
  VirtualRegKey* ptr = Null;
  if (iter != pRegistryHandles->end()) {
    ptr = iter->second.Key;
    return ptr;
  } else {
    for (unsigned r = 0; r < GetRegistryRootCount(); r++) {
      if (ID == (unsigned)(GetRegistryRoot(r).ID)) {
        return VRegKeyResolve(GetRegistryRoot(r).Name);
      }
    }
    return Null;
  }
}

bool RegKeyIsDeleted(unsigned ID) {
  RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
  VirtualRegKey* ptr = Null;
  if (iter != pRegistryHandles->end()) {
    ptr = iter->second.Key;
    if (ptr)
      return ptr->Deleted;
    return false;
  } else {
    return false;
  }
}

void RegKeysInvalidate(VirtualRegKey *Key) {
  RegKeyHandles::iterator iter = pRegistryHandles->begin();
  while (iter != pRegistryHandles->end()) {
    if (iter->second.Key == Key) {
      iter->second.Key = Null;
    }
    ++iter;
  }
}

int RegKeyUndefine(unsigned ID) {
  RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
  if ((ID >= 0x80000000) && (ID < 0x90000000))
    return 0;
  if (iter != pRegistryHandles->end()) {
    pRegistryHandles->erase(iter);
    return 0;
  } else {
    return ERROR_INVALID_HANDLE;
  }
}

bool RegKeyGetVirtual(unsigned ID) {
  if (ID >= VirtualRegKeyBase) {
    return true;
  } else {
    RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
    if (iter != pRegistryHandles->end()) {
      return (iter->second.Key != Null);
    } else {
      return false;
    }
  }
}

bool RegKeyGetVirtual(HKEY Key) {
  return RegKeyGetVirtual(unsigned(Key));
}

//bool VRegGetVirtualized(HKEY Root) {
//  std::vector<HKEY>::iterator iter = std::find(pRegistry->Roots.begin(), pRegistry->Roots.end(), Root);
//  if (iter != pRegistry->Roots.end())
//    return true;
//  return false;
//}
//
//void VRegSetVirtualized(HKEY Root, bool state) {
//  if (state) {
//    VRegSetVirtualized(Root, false);
//    pRegistry->Roots.push_back(Root);
//  } else {
//    std::vector<HKEY>::iterator iter = std::find(pRegistry->Roots.begin(), pRegistry->Roots.end(), Root);
//    if (iter != pRegistry->Roots.end())
//      pRegistry->Roots.erase(iter);
//  }
//}

VirtualRegKey* VRegKeyCreate(const wstring& Path) {
  VirtualRegKey* key = Null;//VRegKeyResolve(Path);
  //if (key) {
  //  return key;
  //} else {
    wstringstream buffer;
    vector<wstring> *parts = split<wstring>(Path, L"\\");
    vector<wstring>::iterator iter;
    int i = 0;
    for (iter = parts->begin(); iter != parts->end(); ++iter) {
      VirtualRegKey* ikey = Null;
      if (i)
        buffer << L"\\";
      buffer << *iter;
      wstring iterLow = toLower(*iter);
      if (!key) {
        ikey = get(pRegistry->Keys, iterLow);
      } else {
        ikey = get(key->Children, iterLow);
      }
      if (!ikey) {
        assert(key);
        key->Deleted = false;
        key->Children[iterLow] = VirtualRegKey(buffer.str());
        ikey = key = get(key->Children, iterLow);
      } else {
        if (ikey && ikey->Deleted)
          ikey->Deleted = false;
        key = ikey;
      }
      i++;
    }
    delete parts;
    return key;
  //}
}

bool VRegKeyIsDeleted(const wstring& Path) {
  vector<wstring> *parts = split<wstring>(toLower(Path), L"\\");
  vector<wstring>::iterator iter;
  VirtualRegKey* key = Null;
  VirtualRegKey* lastKey = Null;
  wstring* part = Null;
  for (iter = parts->begin(); iter != parts->end(); ++iter) {
    VirtualRegKey* ikey = Null;
    if (!key) {
      ikey = get(pRegistry->Keys, toLower(*iter));
    } else {
      ikey = get(key->Children, toLower(*iter));
    }
    if (!ikey) {
      delete parts;
      return false;
    } else {
      if (ikey->Deleted) {
        delete parts;
        return true;
      }
    }
    key = ikey;
  }
  delete parts;
  return false;
}

int VRegKeyDelete(const wstring& Path) {
  vector<wstring> *parts = split<wstring>(toLower(Path), L"\\");
  vector<wstring>::iterator iter;
  VirtualRegKey* key = Null;
  VirtualRegKey* lastKey = Null;
  wstring* part = Null;
  for (iter = parts->begin(); iter != parts->end(); ++iter) {
    VirtualRegKey* ikey = Null;
    if (!key) {
      ikey = get(pRegistry->Keys, toLower(*iter));
    } else {
      ikey = get(key->Children, toLower(*iter));
    }
    if (!ikey) {
      delete parts;
      return 0;
    }
    lastKey = key;
    key = ikey;
    part = &(*iter);
  }
  if ((lastKey) && (key)) {
    key->Deleted = true;
    delete parts;
    return 1;
  }
  delete parts;
  return 0;
}

VirtualRegKey* VRegKeyResolve (const wstring& Path) {
  if (Path.find_first_of(L"\\")) {
    vector<wstring> *parts = split<wstring>(toLower(Path), L"\\");
    vector<wstring>::iterator iter;
    VirtualRegKey* key = Null;
    VirtualRegKey* result = Null;
    for (iter = parts->begin(); iter != parts->end(); ++iter) {
      if (!key) {
        result = key = get(pRegistry->Keys, *iter);
        if (!key)
          break;
      } else {
        result = key = get(key->Children, *iter);
        if (!key)
          break;
      }
    }
    delete parts;
    return result;
  } else {
    // Root name
    return &(pRegistry->Keys[Path]);
  }
  return Null;
}

VirtualRegKey* VRegKeyResolve (VirtualRegKey* Parent, const wstring& Path) {
  assert(Parent);
  vector<wstring> *parts = split<wstring>(toLower(Path), L"\\");
  vector<wstring>::iterator iter;
  VirtualRegKey* key = Null;
  VirtualRegKey* result = Null;
  for (iter = parts->begin(); iter != parts->end(); ++iter) {
    if (!key) {
      result = key = get(Parent->Children, *iter);
      if (!key)
        break;
    } else {
      result = key = get(key->Children, *iter);
      if (!key)
        break;
    }
  }
  delete parts;
  return result;
}

VirtualRegValue* VRegValueGet (VirtualRegKey* Key, const wstring& Name) {
  assert(Key);
  if (Name.size() == 0) {
    VirtualRegValue* ptr = &(Key->Default);
    return ptr;
  } else {
    VirtualRegValue* ptr = get(Key->Values, toLower(Name));
    if (!ptr) {
      VirtualRegKey* child = get(Key->Children, toLower(Name));
      if (child)
        ptr = &(child->Default);
    }
    return ptr;
  }
}

VirtualRegValue* VRegValueSet (VirtualRegKey* Key, const VirtualRegValue& Value) {
  assert(Key);
  if (Value.Name.size() == 0) {
    Key->Default = Value;
    return &(Key->Default);
  } else {
    VirtualRegValue* ptr = get(Key->Values, toLower(Value.Name));
    if (ptr) {
      *ptr = Value;
    } else {
      Key->Values[toLower(Value.Name)] = Value;
      ptr = get(Key->Values, toLower(Value.Name));
    }
    return ptr;
  }
}

LONG Virtual_RegOpenKeyEx(HKEY a0, wstring a1, DWORD a2, REGSAM a3, PHKEY a4) {
  unsigned hKey = unsigned(a0);
  VirtualRegKey* key = RegKeyGetPtr(hKey);
  if (key) {
    if (key->Deleted) {
      *a4 = Null;
      return ERROR_ACCESS_DENIED;
    }
    key = VRegKeyResolve(key, a1);
    if (key) {
      if (key->Deleted) {
        *a4 = Null;
        return ERROR_ACCESS_DENIED;
      } else {
        *a4 = (HKEY)RegKeyDefine(key);
        return ERROR_SUCCESS;
      }
    } else {
      *a4 = Null;
      return ERROR_FILE_NOT_FOUND;
    }
  } else {
    *a4 = Null;
    return ERROR_INVALID_HANDLE;
  }
}

LONG Virtual_RegCreateKeyEx(HKEY a0, wstring a1, DWORD a2, DWORD a3, DWORD a4, REGSAM a5, LPSECURITY_ATTRIBUTES a6, PHKEY a7, LPDWORD a8) {
  wstringstream buffer;
  buffer << RegKeyNameResolve(unsigned(a0));
  buffer << L"\\";
  buffer << a1;
  VirtualRegKey* ptr = VRegKeyCreate(buffer.str());
  if (ptr) {
    *a7 = (HKEY)RegKeyDefine(ptr);
    return ERROR_SUCCESS;
  } else {
    *a7 = Null;
    return ERROR_INVALID_HANDLE;
  }
}

LONG Virtual_RegSetValueEx(HKEY a0, wstring a1, DWORD a2, DWORD a3, BYTE* a4, DWORD a5, bool wide) {
  unsigned hKey = unsigned(a0);
  VirtualRegKey* key = RegKeyGetPtr(hKey);
  if (!key) {
    wstringstream buffer;
    buffer << RegKeyNameResolve(unsigned(a0));
    key = VRegKeyCreate(buffer.str());
  }
  if (key) {
    VirtualRegValue value(a1, getRegValueType(a3), a4, a5, wide);
    VRegValueSet(key, value);
    return ERROR_SUCCESS;
  } else {
    return ERROR_INVALID_HANDLE;
  }
}

LONG Virtual_RegQueryValueEx(HKEY a0, wstring a1, LPDWORD a2, LPDWORD a3, LPBYTE a4, LPDWORD a5, bool wide) {
  unsigned hKey = unsigned(a0);
  VirtualRegKey* key = RegKeyGetPtr(hKey);
  if (key) {
    VirtualRegValue* value = VRegValueGet(key, a1);
    if (value) {
      if (a3)
        *a3 = convertRegValueType(value->Type);
      switch (value->Type) {
        case RegString: {
          if (wide) {
            const wchar_t* src = value->Text.c_str();
            DWORD sz = 4096;
            if (a5)
              sz = *a5;
            sz = min(sz, (value->Text.size()+1) * sizeof(wchar_t));
            if (a5)
              *a5 = sz;
            if (a4) {
              memcpy(a4, src, sz);
              memset(a4 + (value->Text.size() * sizeof(wchar_t)), 0, sizeof(wchar_t));
            }
          } else {
            string temp(value->Text.begin(), value->Text.end());
            const char* src = temp.c_str();
            DWORD sz = 4096;
            if (a5)
              sz = *a5;
            sz = min(sz, temp.size()+1);
            if (a5)
              *a5 = sz;
            if (a4) {
              memcpy(a4, src, sz);
              memset(a4 + temp.size(), 0, 1);
            }
          }
          break;
                        }
        case RegDWord: {
          DWORD temp = value->DWord;
          if (*a5 >= sizeof(DWORD)) {
            if (a4)
              memcpy(a4, &temp, sizeof(DWORD));
            *a5 = sizeof(DWORD);
          }
          break;
                       }
      }
      return ERROR_SUCCESS;
    } else {
      return ERROR_FILE_NOT_FOUND;
    }
  } else {
    return ERROR_INVALID_HANDLE;
  }
}