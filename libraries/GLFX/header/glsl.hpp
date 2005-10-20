namespace GLSL {
  bool isSupported();

  void useProgram(Program& program);
  void disableProgram();

  class Variable {
  public:
    GLhandleARB ProgramHandle;
    GLint Handle;

    Variable() {
      ProgramHandle = 0;
      Handle = 0;
    }

    Variable(GLhandleARB program, GLint handle) {
      ProgramHandle = program;
      Handle = handle;
    }

    bool isValid();

    void set(double value);
    void set(float value);
    void set(int value);
    void set(bool value);
    void set(Pixel value);
    void set(Vec4 value);
    void set(Vec3 value);
    void set(Vec2 value);
    void set(float *values, int count);
    void set(int *values, int count);
    void set(Vec4 *values, int count);
    void set(Vec3 *values, int count);
    void set(Vec2 *values, int count);
    void set(Mat4 *values, int count);
    void set(Mat3 *values, int count);
    void set(Mat2 *values, int count);

    float get();
  };

  class Object {
  public:
    GLhandleARB Handle;

    ~Object() {
      free();
    }

    bool isValid();

    int getInfoLogLength();
    std::string getInfoLog();

    void free();
  };

  class Program : public Object {
  public:
    Program() {
      GL::endDraw();
      this->Handle = glCreateProgramObjectARB();
    }

    bool isLinked();

    Variable getVariable(const char* name);

    void attach(Shader& shader);
    void detach(Shader& shader);
    void link();
  };
  
  class Shader : public Object {
  public:
    bool isCompiled();

    void addSource(const char* text);
    void compile();
  };

  class VertexShader : public Shader {
  public:
    VertexShader() {
      GL::endDraw();
      this->Handle = glCreateShaderObjectARB(GL_VERTEX_SHADER_ARB);
    }
  };

  class FragmentShader : public Shader {
  public:
    FragmentShader() {
      GL::endDraw();
      this->Handle = glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB);
    }
  };
}