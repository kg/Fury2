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

	namespace Device {
		static const unsigned int KeyIndex = 7;
		static const unsigned int Key = 0x44334438;
		static const unsigned int ValueIndex = 6;    
	};

	namespace RenderTarget {
		static const unsigned int KeyIndex = 7;
		static const unsigned int Key = 0x44334437;
		static const unsigned int ValueIndex = 6;    
	};

};

#define checkNamedTag(img, name) (GetImageTag(img, Tags::name::KeyIndex) == Tags::name::Key)
#define getNamedTag(img, name) GetImageTag(img, Tags::name::ValueIndex)
#define setNamedTag(img, name, value) SetImageTag(img, Tags::name::KeyIndex, Tags::name::Key); \
  SetImageTag(img, Tags::name::ValueIndex, value)

#define readParam(type, name, idx) type name = (*((type*)&Parameters->p[idx]))
#define getParam(type, idx) (*((type*)&Parameters->p[idx]))

#define defOverride(name) Export int _##name (Override::OverrideParameters *Parameters)
#define addOverride(name) SoftFX::AddOverride(#name, _##name)
#define addNamedOverride(name, ptr) SoftFX::AddOverride(#name, _##ptr)
#define removeOverride(name) SoftFX::RemoveOverride(#name, _##name)
#define removeNamedOverride(name, ptr) SoftFX::RemoveOverride(#name, _##ptr)

extern void InstallOverrides();
extern void UninstallOverrides();