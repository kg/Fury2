#include "../header/GLFX.hpp"

namespace GLSL {
  bool isSupported() {
    return GLEW_ARB_fragment_shader != 0;
  }

  void useProgram(Program& program) {
    if (GLSL::isSupported()) {
      GL::endDraw();
      if (program.isValid()) {
        if (program.isLinked()) {
          glUseProgramObjectARB(program.Handle);
          Global->checkError();
        }
      }
    }
  }

  void disableProgram() {
    if (GLSL::isSupported()) {
      GL::endDraw();
      glUseProgramObjectARB(0);
      Global->checkError();
    }
  }

  bool Variable::isValid() {
    return (this->Handle != -1);
  }

  void Variable::set(Mat4 *values, int count) {
    if (this->isValid()) {
      glUniformMatrix4fvARB(this->Handle, count, false, reinterpret_cast<const GLfloat*>(values));
      Global->checkError();
    }
  }

  void Variable::set(Mat3 *values, int count) {
    if (this->isValid()) {
      glUniformMatrix3fvARB(this->Handle, count, false, reinterpret_cast<const GLfloat*>(values));
      Global->checkError();
    }
  }

  void Variable::set(Mat2 *values, int count) {
    if (this->isValid()) {
      glUniformMatrix2fvARB(this->Handle, count, false, reinterpret_cast<const GLfloat*>(values));
      Global->checkError();
    }
  }

  void Variable::set(Vec4 *values, int count) {
    if (this->isValid()) {
      glUniform4fvARB(this->Handle, count, reinterpret_cast<const GLfloat*>(values));
      Global->checkError();
    }
  }

  void Variable::set(Vec3 *values, int count) {
    if (this->isValid()) {
      glUniform3fvARB(this->Handle, count, reinterpret_cast<const GLfloat*>(values));
      Global->checkError();
    }
  }

  void Variable::set(Vec2 *values, int count) {
    if (this->isValid()) {
      glUniform2fvARB(this->Handle, count, reinterpret_cast<const GLfloat*>(values));
      Global->checkError();
    }
  }

  void Variable::set(float *values, int count) {
    if (this->isValid()) {
      glUniform1fvARB(this->Handle, count, values);
      Global->checkError();
    }
  }

  void Variable::set(int *values, int count) {
    if (this->isValid()) {
      glUniform1ivARB(this->Handle, count, values);
      Global->checkError();
    }
  }

  void Variable::set(Pixel value) {
    if (this->isValid()) {
      Vec4 vec;
      vec.V[0] = value[::Red] / 255.0f;
      vec.V[1] = value[::Green] / 255.0f;
      vec.V[2] = value[::Blue] / 255.0f;
      vec.V[3] = value[::Alpha] / 255.0f;
      glUniform4fvARB(this->Handle, 1, &(vec.V[0]));
      Global->checkError();
    }
  }

  void Variable::set(Vec4 value) {
    if (this->isValid()) {
      glUniform4fvARB(this->Handle, 1, &(value.V[0]));
      Global->checkError();
    }
  }

  void Variable::set(Vec3 value) {
    if (this->isValid()) {
      glUniform3fvARB(this->Handle, 1, &(value.V[0]));
      Global->checkError();
    }
  }

  void Variable::set(Vec2 value) {
    if (this->isValid()) {
      glUniform2fvARB(this->Handle, 1, &(value.V[0]));
      Global->checkError();
    }
  }

  void Variable::set(double value) {
    if (this->isValid()) {
      glUniform1fARB(this->Handle, (float)value);
      Global->checkError();
    }
  }

  void Variable::set(float value) {
    if (this->isValid()) {
      glUniform1fARB(this->Handle, value);
      Global->checkError();
    }
  }

  float Variable::get() {
    float temp = -1;
    if (this->isValid()) {
      glGetUniformfvARB(this->ProgramHandle, this->Handle, &temp);
      Global->checkError();
    }
    return temp;
  }

  void Variable::set(int value) {
    if (this->isValid()) {
      glUniform1iARB(this->Handle, value);
      Global->checkError();
    }
  }

  void Variable::set(bool value) {
    if (this->isValid()) {
      glUniform1iARB(this->Handle, (int)value);
      Global->checkError();
    }
  }

  bool Program::isLinked() {
    if (isValid()) {
      int temp = 0;
      GL::endDraw();
      glGetObjectParameterivARB(this->Handle, GL_OBJECT_LINK_STATUS_ARB, &temp);
      Global->checkError();
      return (temp == 1);
    } else { 
      return false;
    }
  }

  Variable Program::getVariable(const char *name) {
    GL::endDraw();
    GLint temp = glGetUniformLocationARB(this->Handle, name);
    Global->checkError();
    return Variable(this->Handle, temp);
  }

  bool Shader::isCompiled() {
    if (isValid()) {
      int temp = 0;
      GL::endDraw();
      glGetObjectParameterivARB(this->Handle, GL_OBJECT_COMPILE_STATUS_ARB, &temp);
      Global->checkError();
      return (temp == 1);
    } else { 
      return false;
    }
  }

  void Shader::addSource(const char* text) {
    if (this->isValid()) {
      const char **strings = &text;
      glShaderSourceARB(this->Handle, 1, strings, 0);
      Global->checkError();
    }
  }

  void Shader::compile() {
    if (this->isValid()) {
      glCompileShaderARB(this->Handle);
      Global->checkError();
      _DebugTrace(this->getInfoLog().c_str());
    }
  }

  void Program::attach(Shader& shader) {
    if (this->isValid()) {
      if (shader.isValid()) {
        glAttachObjectARB(this->Handle, shader.Handle);
        Global->checkError();
      }
    }
  }

  void Program::detach(Shader& shader) {
    if (this->isValid()) {
      if (shader.isValid()) {
        glDetachObjectARB(this->Handle, shader.Handle);
        Global->checkError();
      }
    }
  }

  void Program::link() {
    if (this->isValid()) {
      glLinkProgramARB(this->Handle);
      Global->checkError();
      _DebugTrace(this->getInfoLog().c_str());
    }
  }

  bool Object::isValid() {
    return (this->Handle != 0);
  }

  void Object::free() {
    if (this->isValid()) {
      glDeleteObjectARB(this->Handle);
      Global->checkError();
      this->Handle = 0;
    }
  }

  int Object::getInfoLogLength() {
    if (this->isValid()) {
      int temp = 0;
      glGetObjectParameterivARB(this->Handle, GL_OBJECT_INFO_LOG_LENGTH_ARB, &temp);
      Global->checkError();
      return (temp);
    }
    return 0;
  }

  std::string Object::getInfoLog() {
    std::string log;
    if (this->isValid()) {
      int length = this->getInfoLogLength();
      char* buf = (char*)malloc(length + 1);
      memset(buf, 0, length + 1);
      glGetInfoLogARB(this->Handle, length, &length, buf);
      log = std::string(buf);
      ::free(buf);
    }
    return log;
  }
}