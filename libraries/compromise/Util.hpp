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

bool VirtualizeWrites();

WString _wstr (const LPCWSTR ptr, bool escape);
WString _wstr (const LPCWSTR ptr);

WString _str (const LPCSTR ptr, bool escape);
WString _str (const LPCSTR ptr);

//
//template <class T> std::vector<T>* split(const T& Text, const T& Separator) {
//  std::vector<T> *results = new std::vector<T>();
//  int position = 0;
//  int next_position = Text.Find(Separator, position);
//  if (next_position != AString::npos) {
//    while (next_position != AString::npos) {
//      if ((next_position - position) <= 0) {
//        results->push_back(T(L""));
//      } else {
//        results->push_back(T(Text.begin() + position, Text.begin() + next_position));
//      }
//      position = next_position + Separator.size();
//      next_position = Text.find(Separator, position);
//    }
//    results->push_back(T(Text.begin() + position, Text.end()));
//  } else {
//    results->push_back(Text);
//  }
//  return results;
//}

template<class T> T toLower(const T& Text) {
  T output(Text);
  output.resize(Text.size());
  std::transform(Text.begin(), Text.end(), output.begin(), tolower);
  return output;
}

template<class T> T toUpper(const T& Text) {
  T output;
  output.resize(Text.size());
  std::transform(Text.begin(), Text.end(), output.begin(), toupper);
  return output;
}

template<class K, class T, class C, class A> T* get(std::map<K, T, C, A>& Container, const K& Key) {
  std::map<K, T, C, A>::iterator iter = Container.find(Key);
  if (iter == Container.end()) {
    return Null;
  } else {
    //const K& key = iter->first;
    //assert(key == Key);
    return &(iter->second);
  }
}

template<class K, class T, class C, class A> int remove(std::map<K, T, C, A>& Container, const K& Key) {
  std::map<K, T, C, A>::iterator iter = Container.find(Key);
  if (iter == Container.end()) {
    return 0;
  } else {
    Container.erase(iter);
    return 1;
  }
}

inline WString* getNameOfKey(HKEY key) {
  WString* str;
  HMODULE hLib = LoadLibraryW(L"ntdll.dll");
  procQueryKey* queryKey = (procQueryKey*)GetProcAddress(hLib, "NtQueryKey");
  void* pInfo = malloc(2048);
  ULONG infoLength = 0;
  DWORD result = queryKey(key, 0, pInfo, 2000, &infoLength);
  if (result == 0) {
    KEY_BASIC_INFORMATION* info = (KEY_BASIC_INFORMATION*)pInfo;
    wchar_t* nameStart = (wchar_t*)(unsigned(pInfo) + sizeof(LARGE_INTEGER) + (sizeof(ULONG)*2));
    str = new WString(nameStart, info->NameLength/2);
    FreeLibrary(hLib);
    free(pInfo);
    return str;
  } else {
    FreeLibrary(hLib);
    free(pInfo);
    return Null;
  }
}

struct ltwstring
{
  bool operator()(const WString& s1, const WString& s2) const
  {
    return s1 < s2;
//    return wcscmp(s1.Data, s2.Data) < 0;
  }
};