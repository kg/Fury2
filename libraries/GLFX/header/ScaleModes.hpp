namespace ScaleModes {
  #define _SM_BEGIN(Name) \
    struct Name : ScaleMode { \
      static const int id = __COUNTER__; \
      static inline void Set() {
  #define _SM_END \
      } \
    };

  struct ScaleMode {
    static const int id = 0;
    static inline void Set();
  };

  _SM_BEGIN(Linear)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  _SM_END

  _SM_BEGIN(Bilinear)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  _SM_END

}