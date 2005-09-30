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

const double ParticleMinimumL = 1.0f / 10000.0f;

const double CollisionEpsilon = 0.01;

const double ParticleLineExtension = 0.5;

struct ParticleEngineState;
struct ParticleType;
struct Particle;
struct ParticleModifier;
struct ParticleGenerator;
struct ParticleCamera;
struct ParticleDieEvent;
struct ParticleCollideEvent;
class ParticleEngine;

typedef void (ParticleDieCallback)(ParticleDieEvent* evt);
typedef void (ParticleCollideCallback)(ParticleCollideEvent* evt);

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
    float XVDecay;
    float YVDecay;
    float AVDecay;
    float LVDecay;
    float Thickness;
    float Softness;
    float CollisionResponse;
    ParticleGraphicParam *Graphic;
    Pixel Color1;
    Pixel Color2;
    ParticleDieCallback* DieCallback;
    ParticleCollideCallback* CollideCallback;
    DoubleWord UserData;
    Byte XVDecayMode;
    Byte YVDecayMode;
    Byte AVDecayMode;
    Byte LVDecayMode;
    Byte LColorMode;
    Byte LBehavior;
    Byte AMode;
    Byte RenderType;
    Byte RenderMode;
    Byte RenderTarget;
    Byte EnableCollision;
};

struct Particle {
    float X, Y, A, L;
    float XV, YV, AV, LV;
    float Frame;
    int Type;
public:
    bool tick(ParticleEngineState& state);
    void render(ParticleEngineState& state);
    inline bool inside(FRect& rect) {
      return !((this->X < rect.X1) | (this->X > rect.X2) | (this->Y < rect.Y1) | (this->Y > rect.Y2));
    }
};

struct ParticleModifier {
    float X, Y;
    float Range, RangeScale;
    float Mass;
    float Attraction;
    float XVDecay;
    float YVDecay;
    float AVDecay;
    float LVDecay;
    Byte XVDecayMode;
    Byte YVDecayMode;
    Byte AVDecayMode;
    Byte LVDecayMode;
    int ExcludeType, RequireType;
    FRect Area;
public:
    void tick(ParticleEngineState& state);
};

struct ParticleGenerator {
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
public:
    void tick(ParticleEngineState& state);
};

struct ParticleCamera {
    Image **pRenderTargets;
    int RenderTargetCount;
    Rectangle Rectangle;
    float Alpha;
    float ViewportX;
    float ViewportY;
};

typedef std::vector<Particle> ParticleList;
typedef std::vector<int> ParticleCountList;
typedef std::vector<ParticleList> ParticleListList;
typedef std::vector<ParticleType*> ParticleTypeList;
typedef std::vector<ParticleModifier*> ParticleModifierList;
typedef std::vector<ParticleGenerator*> ParticleGeneratorList;

class ParticleEngine {
public:
    ParticleListList Particles;
    ParticleCountList ParticleCounts;
    ParticleTypeList Types;
    ParticleModifierList Modifiers;
    ParticleGeneratorList Generators;
    SpriteParam **Sprites;
    CollisionMatrix *Surfaces;
    FRect Size;
    MTRand RNG;
    double G;
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
      Size.X1 = -99999;
      Size.Y1 = -99999;
      Size.X2 = 99999;
      Size.Y2 = 99999;
    }

    inline void setXg(double Distance) {
      Xg = 1.0 / G / 1.0 / Distance;
    }

    inline float gravity(float m_a_xg, float m_b_kg, float d2) {
      return G * (m_a_xg * Xg) * (m_b_kg) / d2;
    }

    void spawn(Particle& particle, ParticleList& list);
    void spawn(Particle& particle);
    void prespawn(ParticleList& list, int count);
    void tick(float elapsed);
    void render(ParticleCamera& camera);
};

struct ParticleEngineState {
    ParticleEngine* engine;
    ParticleCamera* camera;
    ParticleType* type;
    ParticleList* typeList;
    Particle* particle;
    float elapsed, theta, distance;
    Image* renderTarget;
    RenderFunction* renderer;
    TexturedPolygon poly;

    inline void clear() {
      engine = 0;
      camera = 0;
      type = 0;
      particle = 0;
      elapsed = 0;
      renderTarget = 0;
      renderer = 0;
      theta = 0;
      distance = 0;
    }

    ParticleEngineState() {
      clear();
    }

    ParticleEngineState(ParticleEngine &Engine, ParticleCamera &Camera) {
      clear();
      engine = &Engine;
      camera = &Camera;
    }

    ParticleEngineState(ParticleEngine &Engine, float Elapsed) {
      clear();
      engine = &Engine;
      elapsed = Elapsed;
    }
};

struct ParticleDieEvent {
  ParticleEngine* engine;
  ParticleType* type;
  Particle* particle;
  DoubleWord UserData;
};

struct ParticleCollideEvent {
  ParticleEngine* engine;
  ParticleType* type;
  Particle* particle;
  DoubleWord UserData;
  SpriteParam* sprite;
  FPoint* vector;
};
