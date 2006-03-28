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

#include "../header/Polygon.hpp"

const int ParticleListStartSize = 256;

// m^3 * kg^-1 * s^-2
// * m * m * m / s / s / kg
// 1 pixel = 1 meter
// 1 mass = 1 kg
// 1 elapsed = 1 second
const double DefaultGravitationalConstant = 6.673e-11;
//K = (6.6742) (10 ^ -11)
//G = K m^3 s^-2 kg^-1
//N = m s^-2 kg
//G = K N m^2 kg^-2
//F = G(m1 * m2) / (d^2)
//F = K N m^2 kg^-2 (m1 * m2) / (d^2)
//F = K * N * m^2 * m1 * m2 / (d^2)
//F = K * N * m^2 * C kg * 1 kg / 100 m^2
//F = G * C kg * 1 kg / 100 m^2
//C = F / G / 1 kg / 100 m^2
//C = 1 m/s^2 / G / 1 kg / 100 m^2
//C = F / G / m2 / d
//
//m1 = C kg
//m1 = 1 Xg
//m2 = 1 kg
//d = 100m
//N = 1
//F = 1 m/s^2
const double DefaultXgDistance = 1.0;

// minimum life value accepted as >0
const double ParticleMinimumL = 1.0f / 10000.0f;

// fudge collision math by N pixels to account for the fact that our particles are points and not objects with real size
const double CollisionEpsilon = 0.01;

// fudge collision between particles and line segments by N pixels to account for near misses
const double ParticleLineExtension = 0.25;

// predefine types
struct ParticleEngineState;
struct ParticleType;
struct ParticleCamera;
struct ParticleDieEvent;
struct ParticleCollideEvent;
class Particle;
class ParticleModifier;
class ParticleGenerator;
class ParticleEngine;

// predefine function pointer types
typedef void (ParticleDieCallback)(ParticleDieEvent* evt);
typedef void (ParticleCollideCallback)(ParticleCollideEvent* evt);

// container type aliases
typedef std::vector<Particle> ParticleList;
typedef std::vector<unsigned int> ParticleCountList;
typedef std::vector<ParticleList> ParticleListList;
typedef std::vector<ParticleType*> ParticleTypeList;
typedef std::vector<ParticleModifier*> ParticleModifierList;
typedef std::vector<ParticleGenerator*> ParticleGeneratorList;

enum ParticleDecayModes {
  pdmNone,
  pdmSet,
  pdmAdd,
  pdmMultiply,
  pdmExponent,
  pdmRandomAdd,
  pdmRandomMultiply
};

enum ParticleLBehaviors {
  plbNone,
  plbRemove
};

enum ParticleLColorModes {
  plcNone,
  plcFade,
  plcInterpolate
};

enum ParticleAModes {
  pamFade,
  pamScale,
  pamFadeAndScale,
  pamRotate
};

enum ParticleRenderTypes {
  prtPixel,
  prtAntiAliasPixel,
  prtLine,
  prtAntiAliasLine,
  prtGradientLine,
  prtAntiAliasGradientLine,
  prtStroke,
  prtGraphic
};

struct ParticleGraphicParam {
  int FrameCount;
  Image **pFrames;
  float XCenter, YCenter;
  Pixel MatteColor;
  int Alpha;
  float FrameIncrement;
  Byte LoopMode;
};

struct ParticleType {
  float XVDecay, YVDecay, AVDecay, LVDecay;
  float Thickness;
  float Softness;
  float CollisionResponse;
  ParticleGraphicParam *Graphic;
  Pixel Color1, Color2;
  ParticleDieCallback* DieCallback;
  ParticleCollideCallback* CollideCallback;
  DoubleWord UserData;
  Byte XVDecayMode, YVDecayMode, AVDecayMode, LVDecayMode;
  Byte LColorMode;
  Byte LBehavior;
  Byte AMode;
  Byte RenderType;
  Byte RenderMode;
  Byte RenderTarget;
  Byte EnableCollision;
};

class Particle {
  public:
    float X, Y, A, L;
    float XV, YV, AV, LV;
    float Frame;
    int Type;

    bool tick(const ParticleEngineState& state);
    void render(ParticleEngineState& state);
    inline bool inside (const FRect& rect) const {
      return !((this->X < rect.X1) | (this->X > rect.X2) | (this->Y < rect.Y1) | (this->Y > rect.Y2));
    }
};

class ParticleModifier {
  public:
    float X, Y;
    float Range, RangeScale;
    float Mass;
    float Attraction;
    float XVDecay, YVDecay, AVDecay, LVDecay;
    Byte XVDecayMode, YVDecayMode, AVDecayMode, LVDecayMode;
    int ExcludeType, RequireType;
    FRect Area;

    void tick(const ParticleEngineState& state);
};

class ParticleGenerator {
  public:
    int Type;
    int Life;
    float GenerateRate;
    float GenerateDelay;
    float NewX, NewY, NewL, NewA, NewR;
    float NewXV, NewYV, NewLV, NewAV, NewRV;
    float RandomX, RandomY, RandomL, RandomA, RandomR;
    float RandomXV, RandomYV, RandomLV, RandomAV, RandomRV;
    float GenerateRotation;
    float RandomGenerateRotation;
    float CurrentDelay, CurrentRotation;

    void tick(const ParticleEngineState& state);
};

struct ParticleCamera {
  Image **pRenderTargets;
  int RenderTargetCount;
  Rectangle Rectangle;
  float Alpha;
  float ViewportX, ViewportY;
};

class ParticleEngine {
  public:
    ParticleListList Particles;
    ParticleCountList ParticleCounts;
    ParticleTypeList Types;
    ParticleModifierList Modifiers;
    ParticleGeneratorList Generators;
    SpriteParam **Sprites;
    CollisionMatrix *Surfaces;
    FRect Bounds;
    MTRand RNG;
    // Gravitational constant
    double G;
    // Gravitational unit (kg replacement)
    double Xg;

    inline ParticleEngine() {
      Particles = ParticleListList();
      Types = ParticleTypeList();
      Modifiers = ParticleModifierList();
      Generators = ParticleGeneratorList();
      Surfaces = 0;
      Sprites = 0;
      RNG = MTRand();
      G = DefaultGravitationalConstant;
      setXg(DefaultXgDistance);
	    // set default active zone to a ridiculously large rectangle
      Bounds = FRect(-999999, -999999, 999999, 999999);
    }

    inline void setXg(double Distance) {
      Xg = 1.0 / G / 1.0 / Distance;
    }

    inline float gravity (float m_a_xg, float m_b_kg, float d2) const {
      return G * (m_a_xg * Xg) * (m_b_kg) / d2;
    }

    void spawn(const Particle& particle, ParticleList& list);
    void spawn(const Particle& particle);
    void prespawn(ParticleList& list, int count);
    void tick(float elapsed);
    void render(ParticleCamera& camera);
};

struct ParticleEngineState {
  ParticleEngine* Engine;
  ParticleCamera* Camera;
  ParticleType* Type;
  ParticleList* TypeList;
  Particle* Particle;
  float Elapsed, Theta, Distance;
  Image* RenderTarget;
  RenderFunction* Renderer;
  TexturedPolygon Poly;

  inline void clear() {
    Engine = 0;
    Camera = 0;
    Type = 0;
    Particle = 0;
    Elapsed = 0;
    RenderTarget = 0;
    Renderer = 0;
    Theta = 0;
    Distance = 0;
  }

  ParticleEngineState() {
    clear();
  }

  ParticleEngineState(ParticleEngine &Engine, ParticleCamera &Camera) {
    clear();
    this->Engine = &Engine;
    this->Camera = &Camera;
  }

  ParticleEngineState(ParticleEngine &Engine, float Elapsed) {
    clear();
    this->Engine = &Engine;
    this->Elapsed = Elapsed;
  }
};

struct ParticleDieEvent {
  const ParticleEngine* Engine;
  const ParticleType* Type;
  const Particle* Particle;
  DoubleWord UserData;
};

struct ParticleCollideEvent {
  const ParticleEngine* Engine;
  const ParticleType* Type;
  const Particle* Particle;
  DoubleWord UserData;
  const SpriteParam* Sprite;
  const FPoint* Vector;
};
