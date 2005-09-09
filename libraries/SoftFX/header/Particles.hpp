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

const int ParticleListStartSize = 256;

const float ParticleMinimumL = 1.0f / 10000.0f;

struct ParticleType;
struct Particle;
struct ParticleModifier;
struct ParticleGenerator;
struct ParticleCamera;
class ParticleEngine;

enum ParticleDecayModes {
    pdmNone,
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
    ParticleGraphicParam *Graphic;
    Pixel Color1;
    Pixel Color2;
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
};

struct Particle {
    float X, Y, A, L;
    float XV, YV, AV, LV;
    float Frame;
    int Type;
public:
    void tick(ParticleEngine& engine, ParticleType& type);
    void render(ParticleEngine& engine, ParticleType& type, ParticleCamera& camera);
};

struct ParticleModifier {
    float X, Y;
    float Range;
    float RangeScale;
    float Attraction;
    float MaxAttraction;
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
    void tick(ParticleEngine& engine, Particle& particle);
};

struct ParticleGenerator {
    int Type;
    int GenerateRate;
    float GenerateDelay;
    float CurrentDelay;
    float NewX, NewY, NewL, NewA, NewR;
    float NewXV, NewYV, NewLV, NewAV, NewRV;
    float RandomX, RandomY, RandomL, RandomA;
    float RandomXV, RandomYV, RandomLV, RandomAV;
    float Rotation, GenerateRotation;
public:
    void tick(ParticleEngine& engine);
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
    MTRand RNG;

    inline ParticleEngine() {
      Particles = ParticleListList();
      Types = ParticleTypeList();
      Modifiers = ParticleModifierList();
      Generators = ParticleGeneratorList();
      RNG = MTRand();
    }

    void spawn(Particle& particle, ParticleList& list);
    void spawn(Particle& particle);
    void tick();
    void render(ParticleCamera& camera);
};