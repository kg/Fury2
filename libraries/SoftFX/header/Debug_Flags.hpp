#define defFlags \
  defStart \
/* FLAGS GO HERE */ \
  defItem(RenderAmbientLight) \
  defItem(RenderLightCoronas) \
  defItem(RenderLightShadows) \
  defItem(RenderLightSurfaces) \
  defItem(RenderLightSprites) \
  defItem(RenderLightSourceSingle) \
/* FLAGS GO HERE */ \
  defEnd


/* CONSTANT LIST */
#ifndef DEBUG_FLAG_ENUM
#define DEBUG_FLAG_ENUM
#define defStart \
  const enum FlagIndex { \
    none,
#define defItem(n) \
    n,
#define defEnd \
    count \
  };

namespace DebugFlags {
  defFlags
};
#endif

#undef defStart
#undef defItem
#undef defEnd

#ifdef TRANSLATION_TABLE

/* TRANSLATION TABLE INITIALIZER */
#define defStart \
  template <class t1, class t2> inline void InitDebugFlagTranslationTable(t1& stiTable, t2* itsTable) {
#define defItem(n) \
    stiTable[#n] = DebugFlags::n; \
    itsTable[DebugFlags::n] = std::string(#n);
#define defEnd \
  }

defFlags

#endif

#undef defStart
#undef defItem
#undef defEnd

#undef defFlags

namespace DebugFlags {
  typedef std::map<std::string, int> SToFTable;
  extern int Flags[DebugFlags::count];
  extern std::string FlagIToSTable[DebugFlags::count];
  extern SToFTable FlagSToITable;
};