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

// Predecl

class VirtualRegistry;
class VirtualRegKey;
class VirtualRegValue;
class RegKeyHandle;

enum RegValueTypes;

// Namespaces

using namespace std;

// Decl

typedef DWORD procQueryKey(HKEY KeyHandle,
    DWORD KeyInformationClass,
    PVOID KeyInformation,
    ULONG Length,
    PULONG ResultLength);

struct KEY_BASIC_INFORMATION {
  LARGE_INTEGER LastWriteTime;
  ULONG  TitleIndex;
  ULONG  NameLength;
  WCHAR  Name[1];  //  Variable-length string
};

struct RegistryRoot {
  HKEY ID;
  const wchar_t Name[5];
};
