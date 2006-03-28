namespace BlendModes {
  #define _BM_BEGIN(Name) \
    struct Name : BlendMode { \
      static const int id = __COUNTER__; \
      static inline void Set() {
  #define _BM_END \
      } \
    };

  struct BlendMode {
    static const int id = 0;
    static inline void Set();
  };

  _BM_BEGIN(Normal)
    if (GLEW_EXT_blend_minmax) {
      glBlendEquationEXT(GL_FUNC_ADD_EXT);
    }
    if (GLEW_ARB_texture_env_combine) {
      if (GLEW_EXT_blend_color) {
        glBlendFunc(GL_CONSTANT_ALPHA_EXT, GL_ONE_MINUS_CONSTANT_ALPHA_EXT);
      } else {
        glBlendFunc(GL_ONE, GL_ZERO);
      }
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_ARB, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_ARB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_ARB, GL_SRC_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_ARB, GL_SRC_ALPHA);
      checkGLErrors();
    }
  _BM_END

  _BM_BEGIN(Additive)
    if (GLEW_EXT_blend_minmax) {
      glBlendEquationEXT(GL_FUNC_ADD_EXT);
    }
    if (GLEW_EXT_blend_color) {
      glBlendFunc(GL_CONSTANT_ALPHA_EXT, GL_ONE);
    } else {
      glBlendFunc(GL_ONE, GL_ONE);
    }
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    checkGLErrors();
  _BM_END

  _BM_BEGIN(Subtractive)
    if (GLEW_EXT_blend_subtract) {
      glBlendEquationEXT(GL_FUNC_REVERSE_SUBTRACT_EXT);
      if (GLEW_EXT_blend_color) {
        glBlendFunc(GL_CONSTANT_ALPHA_EXT, GL_ONE);
      } else {
        glBlendFunc(GL_ONE, GL_ONE);
      }
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      checkGLErrors();
    }
  _BM_END

  _BM_BEGIN(Additive_SourceAlpha)
    if (GLEW_EXT_blend_minmax) {
      glBlendEquationEXT(GL_FUNC_ADD_EXT);
    }
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    checkGLErrors();
  _BM_END

  _BM_BEGIN(Subtractive_SourceAlpha)
    if (GLEW_EXT_blend_subtract) {
      glBlendEquationEXT(GL_FUNC_REVERSE_SUBTRACT_EXT);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      checkGLErrors();
    }
  _BM_END

  _BM_BEGIN(SourceAlpha)
    if (GLEW_EXT_blend_minmax) {
      glBlendEquationEXT(GL_FUNC_ADD_EXT);
    }
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    checkGLErrors();
  _BM_END

  _BM_BEGIN(Font_SourceAlpha)
    if (GLEW_EXT_blend_minmax) {
      glBlendEquationEXT(GL_FUNC_ADD_EXT);
    }
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    checkGLErrors();
  _BM_END

  _BM_BEGIN(Multiply)
    if (GLEW_EXT_blend_minmax) {
      glBlendEquationEXT(GL_FUNC_ADD_EXT);
    }
    glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_COLOR);
    if (GLEW_ARB_texture_env_combine) {
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_MODULATE);
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_ARB, GL_MODULATE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_ARB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_CONSTANT_ARB);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_ALPHA_ARB, GL_CONSTANT_ARB);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_ARB, GL_ONE_MINUS_SRC_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_ARB, GL_ONE_MINUS_SRC_ALPHA);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_ARB, GL_SRC_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_ALPHA_ARB, GL_SRC_ALPHA);
      checkGLErrors();
    }
  _BM_END

  _BM_BEGIN(Lightmap)
    if (GLEW_EXT_blend_minmax) {
      glBlendEquationEXT(GL_FUNC_ADD_EXT);
    }
    glBlendFunc(GL_CONSTANT_ALPHA_EXT, GL_ONE_MINUS_CONSTANT_ALPHA_EXT);
    if (GLEW_ARB_multitexture) glActiveTextureARB(GL_TEXTURE0_ARB); 
    if (GLEW_ARB_texture_env_combine) {
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_ARB, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_ARB, GL_CONSTANT_ARB);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_ARB, GL_SRC_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_ARB, GL_SRC_ALPHA);
      checkGLErrors();
    }
    if (GLEW_ARB_multitexture) {
      glActiveTextureARB(GL_TEXTURE1_ARB); 
      if (GLEW_ARB_texture_env_combine) {
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
        glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_MODULATE);
        glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_ARB, GL_MODULATE);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_PREVIOUS_ARB);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_ARB, GL_PREVIOUS_ARB);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_TEXTURE);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_ALPHA_ARB, GL_TEXTURE);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_ARB, GL_SRC_COLOR);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_ARB, GL_SRC_ALPHA);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_ARB, GL_SRC_COLOR);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_ALPHA_ARB, GL_SRC_ALPHA);
        glTexEnvf(GL_TEXTURE_ENV, GL_RGB_SCALE_ARB, 2.0);
        checkGLErrors();
      }
    }
    if (GLEW_ARB_multitexture) glActiveTextureARB(GL_TEXTURE0_ARB); 
  _BM_END

  _BM_BEGIN(Mask_Normal)
    if (GLEW_EXT_blend_minmax) {
      glBlendEquationEXT(GL_FUNC_ADD_EXT);
    }
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    if (GLEW_ARB_multitexture) glActiveTextureARB(GL_TEXTURE0_ARB); 
    if (GLEW_ARB_texture_env_combine) {
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_ARB, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_ARB, GL_CONSTANT_ARB);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_ARB, GL_SRC_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_ARB, GL_SRC_ALPHA);
      checkGLErrors();
    }
    if (GLEW_ARB_multitexture) {
      glActiveTextureARB(GL_TEXTURE1_ARB);
      if (GLEW_ARB_texture_env_combine) {
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
        glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_REPLACE);
        glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_ARB, GL_REPLACE);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_PREVIOUS_ARB);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_ARB, GL_TEXTURE);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_TEXTURE);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_ARB, GL_SRC_COLOR);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_ARB, GL_SRC_ALPHA);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_ARB, GL_SRC_COLOR);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_ALPHA_ARB, GL_SRC_ALPHA);
        glTexEnvf(GL_TEXTURE_ENV, GL_RGB_SCALE_ARB, 1.0);
        checkGLErrors();
      }
    }
    if (GLEW_ARB_multitexture) glActiveTextureARB(GL_TEXTURE0_ARB); 
  _BM_END

  _BM_BEGIN(Mask_SourceAlpha)
    if (GLEW_EXT_blend_minmax) {
      glBlendEquationEXT(GL_FUNC_ADD_EXT);
    }
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    if (GLEW_ARB_multitexture) glActiveTextureARB(GL_TEXTURE0_ARB); 
    if (GLEW_ARB_texture_env_combine) {
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_ARB, GL_MODULATE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_ARB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_ALPHA_ARB, GL_CONSTANT_ARB);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_ARB, GL_SRC_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_ARB, GL_SRC_ALPHA);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_ALPHA_ARB, GL_SRC_ALPHA);
      checkGLErrors();
    }
    if (GLEW_ARB_multitexture) {
      glActiveTextureARB(GL_TEXTURE1_ARB); 
      if (GLEW_ARB_texture_env_combine) {
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
        glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_REPLACE);
        glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_ARB, GL_MODULATE);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_PREVIOUS_ARB);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_ARB, GL_PREVIOUS_ARB);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_PREVIOUS_ARB);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_ALPHA_ARB, GL_TEXTURE);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_ARB, GL_SRC_COLOR);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_ARB, GL_SRC_ALPHA);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_ARB, GL_SRC_COLOR);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_ALPHA_ARB, GL_SRC_ALPHA);
        glTexEnvf(GL_TEXTURE_ENV, GL_RGB_SCALE_ARB, 1.0);
        checkGLErrors();
      }
    }
    if (GLEW_ARB_multitexture) glActiveTextureARB(GL_TEXTURE0_ARB); 
  _BM_END

  _BM_BEGIN(SourceAlpha_Premultiplied)
    if (GLEW_EXT_blend_minmax) {
      glBlendEquationEXT(GL_FUNC_ADD_EXT);
    }
    glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    checkGLErrors();
  _BM_END

}