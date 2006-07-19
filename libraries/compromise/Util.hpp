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

inline std::wstring _wstr(const LPCWSTR ptr, bool escape) {
  if (ptr) {
    std::wstringstream buffer;
    if (escape) {
      buffer << L"\"" << std::wstring((const wchar_t*)ptr) << L"\"";
    } else {
      buffer << std::wstring((const wchar_t*)ptr);
    }
    return buffer.str();
  } else {
    if (escape) {
      return std::wstring(L"<null>");
    } else {
      return std::wstring(L"");
    }
  }
}

inline std::wstring _wstr(const LPCWSTR ptr) {
  return _wstr(ptr, true);
}

inline std::wstring _str(const LPCSTR ptr, bool escape) {
  if (ptr) {
    std::wstringstream buffer;
    std::string ascii = std::string((const char*)ptr);
    if (escape) {
      buffer << L"\"" << std::wstring(ascii.begin(), ascii.end()) << L"\"";
    } else {
      buffer << std::wstring(ascii.begin(), ascii.end());
    }
    return buffer.str();
  } else {
    if (escape) {
      return std::wstring(L"<null>");
    } else {
      return std::wstring(L"");
    }
  }
}

inline std::wstring _str(const LPCSTR ptr) {
  return _str(ptr, true);
}

template <class T> std::vector<T>* split(const T& Text, const T& Separator) {
  std::vector<T> *results = new std::vector<T>();
  int position = 0;
  int next_position = Text.find(Separator, position);
  if (next_position != std::string::npos) {
    while (next_position != std::string::npos) {
      if ((next_position - position) <= 0) {
        results->push_back(T(L""));
      } else {
        results->push_back(T(Text.begin() + position, Text.begin() + next_position));
      }
      position = next_position + Separator.size();
      next_position = Text.find(Separator, position);
    }
    results->push_back(T(Text.begin() + position, Text.end()));
  } else {
    results->push_back(Text);
  }
  return results;
}

template<class T> T toLower(const T& Text) {
  T output;
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

template<class K, class T> T* get(std::map<K, T>& Container, const K& Key) {
  std::map<K, T>::iterator iter = Container.find(Key);
  if (iter != Container.end()) {
    return &(iter->second);
  } else {
    return Null;
  }
}

template<class K, class T> int remove(std::map<K, T>& Container, const K& Key) {
  std::map<K, T>::iterator iter = Container.find(Key);
  if (iter != Container.end()) {
    Container.erase(iter);
    return 1;
  } else {
    return 0;
  }
}

inline wstring* getNameOfKey(HKEY key) {
  wstring* str;
  HMODULE hLib = LoadLibraryW(L"ntdll.dll");
  procQueryKey* queryKey = (procQueryKey*)GetProcAddress(hLib, "NtQueryKey");
  void* pInfo = malloc(2048);
  ULONG infoLength = 0;
  DWORD result = queryKey(key, 0, pInfo, 2000, &infoLength);
  if (result == 0) {
    KEY_BASIC_INFORMATION* info = (KEY_BASIC_INFORMATION*)pInfo;
    wchar_t* nameStart = (wchar_t*)(unsigned(pInfo) + sizeof(LARGE_INTEGER) + (sizeof(ULONG)*2));
    wchar_t* nameEnd = nameStart + info->NameLength/2;
    str = new wstring(nameStart, nameEnd);
    FreeLibrary(hLib);
    free(pInfo);
    return str;
  } else {
    FreeLibrary(hLib);
    free(pInfo);
    return Null;
  }
}