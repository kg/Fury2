/*
SoftFX (Software graphics manipulation library)
Copyright (C) 2003 Kevin Gadd

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

#include "../header/Profiler_Configuration.hpp"

#include <string>
#include <map>

#define Export extern "C" __declspec(dllexport)

#ifdef _DEBUG
namespace win32 {
  #include <windows.h>
};
#undef RGB
#undef RGBA
inline void _DebugTrace(char *text) {
  win32::OutputDebugStringA((win32::LPCSTR)text);
  return;
}
#else
inline void _DebugTrace(char *text) {
  return;
}
#endif

using namespace std;

struct ProfileStruct {
  double start;
  double elapsed;

  ProfileStruct() {
    this->start = 0;
    this->elapsed = 0;
  }
};

extern int LookupSpaceFree();

typedef void ProfilerResetCallback();
typedef void ProfilerPrintCallback(const char *Name);
typedef void ProfilerQueryCallback(double *Output);

ProfilerResetCallback *_ResetCallback = NULL;
ProfilerPrintCallback *_PrintCallback = NULL;
ProfilerQueryCallback *_QueryCallback = NULL;

map<string, ProfileStruct> ProfileData;

Export void ProfileStop(const char *Name) {
#ifdef ENABLE_PROFILER
  if (_QueryCallback == NULL) return;
  double StartTime = 0;
  _QueryCallback(&StartTime);
  ProfileStruct data;
  std::string DataName = Name;
  data = ProfileData[DataName];
  if (data.start == 0) return;
  data.elapsed += (StartTime - data.start);
  data.start = 0;
  ProfileData[DataName] = data;
#endif
}

Export void ProfileStart(const char *Name) {
#ifdef ENABLE_PROFILER
  if (_QueryCallback == NULL) return;
  std::string DataName = Name;
  if (ProfileData[DataName].start != 0) {
    ProfileStop(Name);    
  }
  _QueryCallback(&(ProfileData[DataName].start));
#endif
}

Export void ProfileReset() {
#ifdef ENABLE_PROFILER
  map<string, ProfileStruct>::iterator iter = ProfileData.begin();
  while(iter != ProfileData.end()) {
    iter->second.start = 0;
    iter->second.elapsed = 0;
    ++iter;
  }
#endif
}

Export void ProfileUpdate() {
#ifdef ENABLE_PROFILER
  char out_text[256];
  map<string, ProfileStruct>::iterator iter = ProfileData.begin();
  if (_ResetCallback) _ResetCallback();
  if (_PrintCallback) _PrintCallback("Profile Update:\n");
  _DebugTrace("Profile Update:\n");
  while(iter != ProfileData.end()) {
    sprintf(out_text, "%1.7f S - %s\n", iter->second.elapsed, iter->first.c_str());
    if (_PrintCallback) _PrintCallback(out_text);
    _DebugTrace(out_text);
    ++iter;
  }
  if (_PrintCallback) _PrintCallback("----------------------------\n");
  _DebugTrace("----------------------------\n");
#endif
}

Export void SetProfilerCallbacks(ProfilerResetCallback ResetCallback, ProfilerPrintCallback PrintCallback, ProfilerQueryCallback QueryCallback) {
  _ResetCallback = ResetCallback;
  _PrintCallback = PrintCallback;
  _QueryCallback = QueryCallback;
}
