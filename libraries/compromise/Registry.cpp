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
extern unsigned GetRegKeyHandle();

extern LONG WINAPI Real_RegOpenKeyExW(HKEY a0, LPCWSTR a1,
  DWORD a2, REGSAM a3, PHKEY a4);

unsigned RegKeyDefine(VirtualRegKey *Key) {
  unsigned id = GetRegKeyHandle();
  (*pRegistryHandles)[id] = new RegKeyHandle(id, Key, WString(Key->Name));
  return id;
}

void RegKeyDefine(unsigned newID, unsigned oldID) {
  if (newID == oldID)
    return;
  assert(!((newID >= 0x80000000) && (newID < 0x90000000)));
  //RegKeyHandles::iterator iter = pRegistryHandles->find(newID);
  //if (iter != pRegistryHandles->end())
  //  assert(false);
  RegKeyHandles::iterator iter = pRegistryHandles->find(oldID);
  if (iter == pRegistryHandles->end()) {
  } else {
    (*pRegistryHandles)[newID] = new RegKeyHandle(*(iter->second));
    (*pRegistryHandles)[newID]->ID = newID;
  }
}

void RegKeyDefine(unsigned ID, VirtualRegKey *Key) {
  assert(!((ID >= 0x80000000) && (ID < 0x90000000)));
  (*pRegistryHandles)[ID] = new RegKeyHandle(ID, Key, Key->Name);
}

void RegKeyNameDefine(unsigned ID, const WString& Name) {
  assert(!((ID >= 0x80000000) && (ID < 0x90000000)));
  RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
  if (iter == pRegistryHandles->end()) {
    assert(Name.Length < 256);
    (*pRegistryHandles)[ID] = new RegKeyHandle(ID, Name);
  } else {
    // Already defined
    (*pRegistryHandles)[ID]->Name = Name;
  }
  return;
}

void RegKeyNameResolve(unsigned ID, WString** Out) {
  *Out = 0;
  RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
  if (iter == pRegistryHandles->end()) {
    return;
  } else {
    *Out = &(iter->second->Name);
    return;
  }
}

VirtualRegKey* RegKeyGetPtr(unsigned ID) {
  RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
  VirtualRegKey* ptr = Null;
  if (iter == pRegistryHandles->end()) {
    for (unsigned r = 0; r < GetRegistryRootCount(); r++) {
      if (ID == (unsigned)(GetRegistryRoot(r).ID)) {
        return VRegKeyResolve(GetRegistryRoot(r).Name);
      }
    }
    return Null;
  } else {
    ptr = iter->second->Key;
    //if (ptr)
    //  ptr->Name = iter->second->Name;
    return ptr;
  }
}

bool RegKeyIsDeleted(unsigned ID) {
  RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
  VirtualRegKey* ptr = Null;
  if (iter == pRegistryHandles->end()) {
    return false;
  } else {
    ptr = iter->second->Key;
    if (ptr)
      return ptr->Deleted;
    return false;
  }
}

void RegKeysInvalidate(VirtualRegKey *Key) {
  assert(false);
  RegKeyHandles::iterator iter = pRegistryHandles->begin();
  while (!(iter == pRegistryHandles->end())) {
    if (iter->second->Key == Key) {
      iter->second->Key = Null;
    }
    ++iter;
  }
}

int RegKeyUndefine(unsigned ID) {
  if (ID >= 0x80000000 && ID < 0x90000000) 
    return 0;
  RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
  if (iter == pRegistryHandles->end()) {
    return ERROR_INVALID_HANDLE;
  } else {
    if (iter->second->ID == ID) {
      assert(!((iter->second->ID >= 0x80000000) && (iter->second->ID < 0x90000000)));
      RegKeyHandle* handle = iter->second;
      iter->second = 0;
      pRegistryHandles->erase(iter);
      handle->ID = 0;
      handle->Key = 0;
      handle->Name.Erase();
      delete handle;
      return 0;
    } else {
      return ERROR_INVALID_HANDLE;
    }
  }
}

bool RegKeyGetVirtual(unsigned ID) {
  if (ID >= VirtualRegKeyBase) {
    return true;
  } else {
    RegKeyHandles::iterator iter = pRegistryHandles->find(ID);
    if (iter == pRegistryHandles->end()) {
      return false;
    } else {
      return (iter->second->Key != Null);
    }
  }
}

bool RegKeyGetVirtual(HKEY Key) {
  return RegKeyGetVirtual(unsigned(Key));
}

VirtualRegKey* VRegKeyCreate(const WString& Path) {
  VirtualRegKey* key = Null;//VRegKeyResolve(Path);
  //if (key) {
  //  return key;
  //} else {
    wstringstream buffer;
    vector<WString> *parts = Path.Split(L'\\');
    vector<WString>::iterator iter;
    int i = 0;
    for (iter = parts->begin(); iter != parts->end(); ++iter) {
      if (iter->Length) {
        VirtualRegKey* ikey = Null;
        if (i)
          buffer << L"\\";
        buffer << *iter;
        WString newName(buffer.str().c_str());
        WString iterLow(iter->ToLower());
        ikey = Null;
        if (!key) {
          ikey = get(pRegistry->Keys, iterLow);
          if (ikey)
            assert(ikey->Name.EndsWithI(iterLow));
        } else {
          ikey = get(key->Children, iterLow);
          if (ikey)
            assert(ikey->Name.EndsWithI(iterLow));
        }
        if (!ikey) {
          if (key == Null) {
            DebugOut_("key==Null at " << *iter << " in " << Path << "\n");
            assert(false);
          }
          key->Deleted = false;
          VirtualRegKey newKey = VirtualRegKey(newName);
          newKey.Default.Text = WString();
          key->Children[iterLow] = newKey;
          ikey = key = get(key->Children, iterLow);
        } else {
          if (ikey->Name.EqualsI(newName)) {
            if (ikey && ikey->Deleted)
              ikey->Deleted = false;
            key = ikey;
          } else {
            VRegDump(*key);
            assert(false);
          }
        }
        i++;
      }
    }
    delete parts;
    if (key == 0)
      assert(false);
    return key;
  //}
}

bool VRegKeyIsDeleted(const WString& Path) {
  vector<WString> *parts = Path.ToLower().Split(L'\\');
  vector<WString>::iterator iter;
  VirtualRegKey* key = Null;
  VirtualRegKey* lastKey = Null;
  WString* part = Null;
  for (iter = parts->begin(); iter != parts->end(); ++iter) {
    VirtualRegKey* ikey = Null;
    if (!key) {
      ikey = get(pRegistry->Keys, iter->ToLower());
    } else {
      ikey = get(key->Children, iter->ToLower());
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

int VRegKeyDelete(const WString& Path) {
  vector<WString> *parts = Path.ToLower().Split(L'\\');
  vector<WString>::iterator iter;
  VirtualRegKey* key = Null;
  VirtualRegKey* lastKey = Null;
  WString* part = Null;
  for (iter = parts->begin(); iter != parts->end(); ++iter) {
    VirtualRegKey* ikey = Null;
    if (!key) {
      ikey = get(pRegistry->Keys, iter->ToLower());
    } else {
      ikey = get(key->Children, iter->ToLower());
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

VirtualRegKey* VRegKeyResolve (const WString& Path) {
  if (Path.Find(L'\\') >= 0) {
    vector<WString> *parts = Path.ToLower().Split(L'\\');
    vector<WString>::iterator iter;
    VirtualRegKey* key = Null;
    VirtualRegKey* result = Null;
    unsigned i = 0;
    unsigned c = parts->size();
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
      i++;
    }
    delete parts;
    if (i < c)
      return Null;
    return result;
  } else {
    // Root name
    return &(pRegistry->Keys[Path]);
  }
  return Null;
}

VirtualRegKey* VRegKeyResolve (VirtualRegKey* Parent, const WString& Path) {
  assert(Parent);
  vector<WString> *parts = Path.ToLower().Split(L'\\');
  vector<WString>::iterator iter;
  VirtualRegKey* key = Null;
  VirtualRegKey* result = Null;
  unsigned i = 0;
  unsigned c = parts->size();
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
    i++;
  }
  delete parts;
  if (i < c)
    return Null;
  return result;
}

VirtualRegValue* VRegValueGet (VirtualRegKey* Key, const WString& Name) {
  assert(Key);
  if (Name.size() == 0) {
    VirtualRegValue* ptr = &(Key->Default);
    return ptr;
  } else {
    VirtualRegKey* child = get(Key->Children, Name.ToLower());
    VirtualRegValue* ptr = get(Key->Values, Name.ToLower());
    if (child)
        ptr = &(child->Default);
    return ptr;
  }
}

VirtualRegValue* VRegValueSet (VirtualRegKey* Key, const VirtualRegValue& Value) {
  assert(Key);
  if (Value.Name.size() == 0) {
    Key->Default = Value;
    return &(Key->Default);
  } else {
    WString keyString = Value.Name.ToLower();
    VirtualRegValue* ptr = get(Key->Values, keyString);
    if (ptr) {
      *ptr = Value;
    } else {
      Key->Values[keyString] = Value;
      ptr = get(Key->Values, keyString);
    }
    return ptr;
  }
}

LONG Virtual_RegOpenKeyEx(HKEY a0, WString a1, DWORD a2, REGSAM a3, PHKEY a4) {
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

LONG Virtual_RegCreateKeyEx(HKEY a0, WString a1, DWORD a2, DWORD a3, DWORD a4, REGSAM a5, LPSECURITY_ATTRIBUTES a6, PHKEY a7, LPDWORD a8) {
  wstringstream buffer;
  WString *keyname = 0;
  RegKeyNameResolve(unsigned(a0), &keyname);
  if (keyname)
    assert(keyname->Length > 0);
  else
    return ERROR_INVALID_HANDLE;
  buffer << keyname;
  buffer << L"\\";
  buffer << a1;
  WString sKey = WString(buffer.str().c_str());
  VirtualRegKey* ptr = VRegKeyCreate(sKey);
  if (ptr) {
    unsigned key = RegKeyDefine(ptr);
    //RegKeyNameDefine(key, sKey);
    *a7 = reinterpret_cast<HKEY>(key);
    DebugOut_("Created " << sKey << "\n");
    return ERROR_SUCCESS;
  } else {
    *a7 = Null;
    return ERROR_INVALID_HANDLE;
  }
}

LONG Virtual_RegSetValueEx(HKEY a0, WString a1, DWORD a2, DWORD a3, BYTE* a4, DWORD a5, bool wide) {
  unsigned hKey = unsigned(a0);
  VirtualRegKey* key = RegKeyGetPtr(hKey);
  if (!key) {
    wstringstream buffer;
    WString *keyname;
    RegKeyNameResolve(unsigned(a0), &keyname);
    buffer << keyname;
    key = VRegKeyCreate(WString(buffer.str().c_str()));
  }
  if (key) {
    VRegValueSet(key, VirtualRegValue(a1, getRegValueType(a3), a4, a5, wide));
    return ERROR_SUCCESS;
  } else {
    return ERROR_INVALID_HANDLE;
  }
}

LONG Virtual_RegQueryValueEx(HKEY a0, WString a1, LPDWORD a2, LPDWORD a3, LPBYTE a4, LPDWORD a5, bool wide) {
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
            const wchar_t* src = value->Text.pointer();
            DWORD sz = 4096;
            if (a5) {
              sz = *a5;
              if (a4)
                memset(a4, 0, sz);
            }
            sz = min(sz, (value->Text.size()+1) * sizeof(wchar_t));
            if (a5)
              *a5 = sz;
            if (a4) {
              memcpy(a4, src, sz);
            }
          } else {
            AString temp(value->Text);
            const char* src = temp.pointer();
            DWORD sz = 4096;
            if (a5) {
              sz = *a5;
              if (a4)
                memset(a4, 0, sz);
            }
            sz = min(sz, temp.size()+1);
            if (a5)
              *a5 = sz;
            if (a4) {
              memcpy(a4, src, sz);
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
      if (a4 && a5)
        memset(a4, 0, *a5);
      return ERROR_FILE_NOT_FOUND;
    }
  } else {
    if (a4 && a5)
      memset(a4, 0, *a5);
    return ERROR_INVALID_HANDLE;
  }
}

void VRegDump(const VirtualRegKey& key) {
  DebugOut_(key.Name << "\\\n");
  {
    DebugOut_("Default=" << key.Default.Text << "\n");
    VirtualRegValues::const_iterator iter = key.Values.begin();
    while (iter != key.Values.end()) {
      DebugOut_(iter->second.Name << "=" << iter->second.Text << "\n");
      ++iter;
    }
  }
  {
    VirtualRegKeys::const_iterator iter = key.Children.begin();
    while (iter != key.Children.end()) {
      VRegDump(iter->second);
      ++iter;
    }
  }
}

void VRegDump() {
  VirtualRegKeys::const_iterator iter = pRegistry->Keys.begin();
  while (iter != pRegistry->Keys.end()) {
    VRegDump(iter->second);
    ++iter;
  }
}

void VRegFree() {
  pRegistry->Keys.clear();
}
