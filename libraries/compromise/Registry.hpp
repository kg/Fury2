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

static const unsigned VirtualRegKeyBase = 0x90000000;

// Definitions

enum RegValueTypes {
  RegString = REG_SZ,
  RegDWord = REG_DWORD,
  RegQWord = REG_QWORD
};

inline RegValueTypes getRegValueType(DWORD type) {
  switch (type) {
    case REG_SZ:
    case REG_EXPAND_SZ:
    case REG_BINARY:
      return RegString;
    case REG_DWORD:
    case REG_DWORD_BIG_ENDIAN:
      return RegDWord;
    case REG_QWORD:
      return RegQWord;
  }
  return RegDWord;
}

inline DWORD convertRegValueType(RegValueTypes type) {
  switch (type) {
    case RegString:
      return REG_SZ;
    case RegDWord:
      return REG_DWORD;
    case RegQWord:
      return REG_QWORD;
  }
  return 0;
}

class VirtualRegValue {
public:
  WString Name;
  RegValueTypes Type;
  WString Text;
  unsigned DWord;

  VirtualRegValue()
    :Name(NullString),
     Type(RegDWord),
     Text(NullString),
     DWord(0)
  {
  }

  VirtualRegValue(const WString& name, RegValueTypes type, void* data, unsigned dataLength, bool wide)
    :Name(name),
     Type(type),
     Text(NullString),
     DWord(0)
  {
    switch (type) {
      case RegString: {
        if (wide) {
          Text = WString((wchar_t*)data, (dataLength/2));
          DebugOut_("Value(" << Text << ")\n");
        } else {
          Text = WString((const char*)data, dataLength);;
          DebugOut_("Value(" << Text << ")\n");
        }
        break;
                      }
      case RegDWord:
        memcpy(&DWord, data, 4);
        break;
    }
  }

  VirtualRegValue(WString name, WString data)
    :Name(name),
     Type(RegString),
     Text(data),
     DWord(0)
  {
  }

  VirtualRegValue(WString name, unsigned data)
    :Name(name),
     Type(RegDWord),
     Text(NullString),
     DWord(data)
  {
  }

  VirtualRegValue(WString name, RegValueTypes type)
    :Name(name),
     Type(type),
     Text(NullString),
     DWord(0)
  {
  }

  VirtualRegValue(RegValueTypes type, void* data)
    :Name(NullString),
     Type(type),
     Text(NullString),
     DWord(0)
  {
  }

  VirtualRegValue(RegValueTypes type)
    :Name(NullString),
     Type(type),
     Text(NullString),
     DWord(0)
  {
  }

  VirtualRegValue(WString data)
    :Name(NullString),
     Type(RegString),
     Text(data),
     DWord(0)
  {
  }

  VirtualRegValue(unsigned data)
    :Name(NullString),
     Type(RegDWord),
     Text(NullString),
     DWord(data)
  {
  }

  ~VirtualRegValue() {
  }
//private:
  VirtualRegValue(const VirtualRegValue& other)
    :Name(other.Name),
     Type(other.Type),
     Text(other.Text),
     DWord(other.DWord)
  {
  }

  inline const VirtualRegValue& operator=(const VirtualRegValue& rhs) {
    if ((&rhs) == (this))
      return *this;
    Name = rhs.Name;
    Text = rhs.Text;
    Type = rhs.Type;
    DWord = rhs.DWord;
    return *this;
  }
};

typedef std::map<WString, VirtualRegKey, ltwstring> VirtualRegKeys;
typedef std::map<WString, VirtualRegValue, ltwstring> VirtualRegValues;

class VirtualRegKey {
public:
  WString Name;
  VirtualRegKeys Children;
  VirtualRegValue Default;
  VirtualRegValues Values;
  bool Deleted;

  VirtualRegKey(WString name) 
    :Name(name),
     Default(RegString),
     Deleted(false)
  {
  }

  VirtualRegKey() 
    :Name(NullString),
     Default(RegString),
     Deleted(false)
  {
  }
//private:
  VirtualRegKey(const VirtualRegKey& other)
    :Name(other.Name),
     Default(other.Default),
     Deleted(other.Deleted),
     Children(other.Children),
     Values(other.Values)
  {
  }

  inline const VirtualRegKey& operator=(const VirtualRegKey& rhs) {
    if ((&rhs) == (this))
      return *this;
    Name = rhs.Name;
    Default = rhs.Default;
    Children = rhs.Children;
    Values = rhs.Values;
    Deleted = rhs.Deleted;
    return *this;
  }
};

class VirtualRegistry {
public:
  VirtualRegKeys Keys;

  VirtualRegistry()
  {
  }
private:
  VirtualRegistry(const VirtualRegistry& other)
    :Keys(other.Keys)
  {
  }

  inline const VirtualRegistry& operator=(const VirtualRegistry& rhs) {
    if ((&rhs) == (this))
      return *this;
    Keys = rhs.Keys;
  }
};

class RegKeyHandle {
public:
  unsigned ID;
  VirtualRegKey* Key;
  WString Name;
  HKEY RealKey;

  RegKeyHandle()
    :ID(0),
     Key(Null),
     Name(),
     RealKey(Null)
  {
  }

  RegKeyHandle(unsigned id, const WString& name) 
    :ID(id),
     Key(Null),
     Name(name),
     RealKey(Null)
  {
  }

  RegKeyHandle(unsigned id, VirtualRegKey* key, const WString& name) 
    :ID(id),
     Key(key),
     Name(name),
     RealKey(Null)
  {
  }

  RegKeyHandle(const RegKeyHandle& other)
    :ID(other.ID),
     Key(other.Key),
     Name(other.Name),
     RealKey(other.RealKey)
  {
  }

  inline const RegKeyHandle& operator=(const RegKeyHandle& rhs) {
    if ((&rhs) == (this))
      return *this;
    ID = rhs.ID;
    Key = rhs.Key;
    Name = rhs.Name;
    RealKey = rhs.RealKey;
  }
};

typedef std::map<unsigned, RegKeyHandle*> RegKeyHandles;

unsigned RegKeyDefine (VirtualRegKey* Key);
void RegKeyDefine (unsigned newID, unsigned oldID);
void RegKeyDefine (unsigned ID, VirtualRegKey* Key);
void RegKeyNameDefine (unsigned ID, const WString& Name);
void RegKeyNameResolve (unsigned ID, WString** Out);
bool RegKeyGetVirtual (unsigned ID);
bool RegKeyGetVirtual (HKEY Key);
VirtualRegKey* RegKeyGetPtr (unsigned ID);
bool RegKeyIsDeleted (unsigned ID);
void RegKeysInvalidate (VirtualRegKey* Key);
int RegKeyUndefine (unsigned ID);

//bool VRegGetVirtualized (HKEY Root);
//void VRegSetVirtualized (HKEY Root, bool state);

VirtualRegKey* VRegKeyCreate (const WString& Path);
int VRegKeyDelete (const WString& Path);
bool VRegKeyIsDeleted (const WString& Path);
VirtualRegKey* VRegKeyResolve (const WString& Path);
VirtualRegKey* VRegKeyResolve (VirtualRegKey* Parent, const WString& Path);

VirtualRegValue* VRegValueGet (VirtualRegKey* Key, const WString& Name);
VirtualRegValue* VRegValueSet (VirtualRegKey* Key, const VirtualRegValue& Value);

void VRegDump ();
void VRegDump (const VirtualRegKey& key);
void VRegFree ();