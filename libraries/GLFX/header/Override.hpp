namespace Override {

	struct OverrideParameters {
		int count;
		const char* key;
		int index;
		unsigned int p[16];
	};

	typedef int (Override)(OverrideParameters *Parameters);

};

namespace Tags {
	namespace Context {
		static const unsigned int KeyIndex = 7;
		static const unsigned int Key = 0x4F474C;
		static const unsigned int ValueIndex = 6;
	};
  namespace Texture {
    static const unsigned int ValueIndex = 5;
  };
  namespace DC {
    static const unsigned int ValueIndex = 4;
  };
};

#define checkNamedTag(img, name) (SoftFX::GetImageTag(img, Tags::name::KeyIndex) == Tags::name::Key)
#define getNamedTag(img, name) SoftFX::GetImageTag(img, Tags::name::ValueIndex)
#define setNamedTag(img, name, value) SoftFX::SetImageTag(img, Tags::name::ValueIndex, value)
#define setNamedTagAndKey(img, name, value) SoftFX::SetImageTag(img, Tags::name::KeyIndex, Tags::name::Key); \
  SoftFX::SetImageTag(img, Tags::name::ValueIndex, value)

#define readParam(type, name, idx) type name = (*((type*)&Parameters->p[idx]))
#define readParamD(type, name, idx, defaultValue) type name; \
  { \
    type* temp = ((type*)Parameters->p[idx]); \
    if (temp) { \
      name = *temp; \
    } else { \
      name = defaultValue; \
    } \
  }
#define readParamRect(name, idx, image) FX::Rectangle name; \
  { \
    FX::Rectangle* temp = ((FX::Rectangle*)Parameters->p[idx]); \
    if (temp) { \
      name = *temp; \
    } else { \
      GetImageRectangle(image, &name);\
    } \
  }
#define getParam(type, idx) (*((type*)&Parameters->p[idx]))

#define defOverride(name) Export int _##name (Override::OverrideParameters *Parameters)
#define passOverride(name, passTo) Export int _##name (Override::OverrideParameters *Parameters) { \
    return _##passTo(Parameters); \
  }
#define addOverride(name) SoftFX::AddOverride(#name, _##name)
#define addNamedOverride(name, ptr) SoftFX::AddOverride(#name, _##ptr)
#define removeOverride(name) SoftFX::RemoveOverride(#name, _##name)
#define removeNamedOverride(name, ptr) SoftFX::RemoveOverride(#name, _##ptr)

extern void InstallOverrides();
extern void UninstallOverrides();