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

enum ParticleDecayModes {
    pdmNone,
    pdmAdd,
    pdmMultiply,
    pdmExponent
};

enum ParticleLBehaviors {
    plbNone,
    plbRemove
};

enum ParticleRenderModes {
    prmPixel,
    prmAntiAliasPixel,
    prmLine,
    prmAntiAliasLine,
    prmGradientLine,
    prmAntiAliasGradientLine,
    prmGraphic
};

struct ParticleGraphicParam {
    int FrameCount;
    Image **pFrames;
    float XCenter, YCenter;
    DoubleWord MatteColor;
    int Alpha;
    int Frame;
};

struct ParticleType {
    float XVDecay;
    float YVDecay;
    float AVDecay;
    float LVDecay;
    ParticleGraphicParam *Graphic;
    FLine Line;
    DoubleWord Color1;
    DoubleWord Color2;
    Byte XVDecayMode;
    Byte YVDecayMode;
    Byte AVDecayMode;
    Byte LVDecayMode;
    Byte LBehavior;
    Byte RenderMode;
    Byte BlitMode;
};

struct Particle {
    float X, Y, A, L;
    float XV, YV, AV, LV;
    int Type;
public:
    void tick();
};

struct ParticleModifier {
    float X, Y;
    float Range;
    float Decay;
    float XVDecay;
    float YVDecay;
    float AVDecay;
    float LVDecay;
    Byte DecayMode;
    Byte XVDecayMode;
    Byte YVDecayMode;
    Byte AVDecayMode;
    Byte LVDecayMode;
public:
    void tick(Particle& particle);
};

struct ParticleGenerator {
    int Type;
    int GenerateRate;
    int GenerateDelay;
    int CurrentDelay;
    float NewX, NewY, NewL, NewA;
    float NewXV, NewYV, NewLV, NewAV;
    float RandomX, RandomY, RandomL, RandomA;
public:
    void tick();
};

typedef std::vector<Particle> ParticleList;
typedef std::vector<ParticleType*> ParticleTypeList;
typedef std::vector<ParticleModifier*> ParticleModifierList;
typedef std::vector<ParticleGenerator*> ParticleGeneratorList;

class ParticleEngine {

public:
    int ActiveParticles;
    ParticleList Particles;
    ParticleTypeList Types;
    ParticleModifierList Modifiers;
    ParticleGeneratorList Generators;

    void tick();
    void render(Image* surface, float xoffset, float yoffset);
};