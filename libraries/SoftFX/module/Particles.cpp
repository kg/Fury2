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

#ifdef _OPENMP
#include <omp.h>
#endif
#include "../header/SoftFX Main.hpp"
#include "../header/MersenneTwister.h"
#include "../header/Fury2.hpp"
#include "../header/Particles.hpp"
#include "../header/Filters.hpp"
#include "../header/Blitters.hpp"
#include "../header/Resample.hpp"

void decayUpdate(ParticleDecayModes mode, float& value, const float decay, const float elapsed, MTRand& rng) {
  switch (mode) {
    case pdmSet: 
      value = (decay * _One(elapsed)) + (value * (1 - _One(elapsed))); 
      break; 

    case pdmAdd: 
      value += decay * elapsed; 
      break; 

    case pdmMultiply: 
      value *= pow(decay, elapsed); 
      break; 

    case pdmExponent: 
      value = pow(value, pow(decay, elapsed)); 
      break; 

    case pdmRandomAdd: 
      value += decay * rng.rand() * elapsed; 
      break; 

    case pdmRandomMultiply: 
      if (decay >= 0.0f)
        value *= pow((float)pow(decay, (float)rng.rand()), elapsed); 
      else
        value *= pow((float)(1.0f + (abs(decay) * rng.rand())), elapsed); 
      break; 
  }
}

inline void decayUpdate(Byte mode, float& value, const float decay, const float elapsed, MTRand& rng) {
  decayUpdate((ParticleDecayModes)mode, value, decay, elapsed, rng);
}

RenderFunction* getRenderer(int id) {
  switch (id) {
    case 1:
      return RenderFunction_SourceAlpha;
    case 2:
      return RenderFunction_Additive;
    case 3:
      return RenderFunction_Subtractive;
    case 4:
      return RenderFunction_Merge;
    case 5:
      return RenderFunction_Shadow;
    case 6:
      return RenderFunction_Screen;
    case 7:
      return RenderFunction_Additive_SourceAlpha;
    case 8:
      return RenderFunction_Subtractive_SourceAlpha;
    case 9:
      return RenderFunction_Font_SourceAlpha;
    default:
      return 0;
  }
}

void ParticleEngine::spawn(const Particle& particle, ParticleList& list) {
  if (!particle.inside(this->Bounds)) return;
  unsigned int c = this->ParticleCounts[particle.Type];
  if (c >= list.size())
    list.push_back(particle);
  else
    list[c] = particle;
  this->ParticleCounts[particle.Type] = ++c;
}

void ParticleEngine::prespawn(ParticleList& list, int newcount) {
  if (list.capacity() < newcount)
    list.reserve(newcount);
}

inline void ParticleEngine::spawn(const Particle& particle) {
  if (particle.Type < 0) return;
  if (particle.Type >= this->Particles.size()) return;
  this->spawn(particle, this->Particles[particle.Type]);
}

void ParticleEngine::render(ParticleCamera& camera) {
  ParticleEngineState state = ParticleEngineState(*this, camera);
  ParticleList::iterator iter;
  ParticleListList::iterator listiter;
  ParticleTypeList::iterator typeiter;

  // set up the clipping rectangles of all the render targets
  for (int r = 0; r < camera.RenderTargetCount; r++) {
    Rectangle clip;
    clip = camera.Rectangle;
    if (camera.pRenderTargets[r]) {
      camera.pRenderTargets[r]->clipRectangle(&clip);
      camera.pRenderTargets[r]->setClipRectangle(&clip);
    }
  }

  unsigned int t = 0;
  typeiter = this->Types.begin();
  listiter = this->Particles.begin();
  while (typeiter != this->Types.end()) {
    unsigned int i = this->ParticleCounts[t];
    if (i > listiter->size()) i = listiter->size();
    iter = listiter->begin();

    // preallocate a polygon structure for any polygon rendering operations
    state.Poly.Allocate(4);

    state.Type = (*typeiter);
    state.RenderTarget = camera.pRenderTargets[ClipValue(state.Type->RenderTarget, camera.RenderTargetCount)];

    if (state.RenderTarget) {
      state.Theta = 0;
      state.Distance = 0;

      // predetermine the renderer for this particle type
      state.Renderer = getRenderer(state.Type->RenderMode);

      // precalculate theta and distance for this particle type
      if (state.Type->RenderType == prtGraphic) {
        if (state.Type->Graphic) {
          Image* f = state.Type->Graphic->pFrames[0];
          float w = f->Width / 2, h = f->Height / 2;
          state.Theta = atan2(h, w);
          state.Distance = sqrt((w * w) + (h * h));
        }
      }

      while (i--) {
        state.Particle = &(*iter);
        iter->render(state);
        ++iter;
      }

    }

    ++listiter;
    ++typeiter;
    t++;
  }

  // reset the clipping rectangles of all the render targets
  for (int r = 0; r < camera.RenderTargetCount; r++) {
    if (camera.pRenderTargets[r]) {
      camera.pRenderTargets[r]->setClipRectangle(camera.pRenderTargets[r]->getRectangle());
    }
  }
}

void ParticleEngine::tick(float elapsed) {
  ParticleEngineState state = ParticleEngineState(*this, elapsed);
  ParticleList::iterator iter;
  ParticleListList::iterator listiter;
  ParticleCountList::iterator countiter;
  ParticleTypeList::iterator typeiter;
  ParticleModifierList::iterator modifier;
  ParticleGeneratorList::iterator generator;
  std::vector<int> killList;
  ParticleDieEvent evt;
  evt.Engine = this;
  bool kill = false;

  generator = this->Generators.begin();
  while (generator != this->Generators.end()) {
    (*generator)->tick(state);
    ++generator;
  }

  unsigned int t = 0;
  typeiter = this->Types.begin();
  listiter = this->Particles.begin();
  countiter = this->ParticleCounts.begin();
  while (typeiter != this->Types.end()) {
    state.Type = (*typeiter);
    evt.Type = state.Type;
    evt.UserData = evt.Type->UserData;
    state.TypeList = &(*listiter);
    unsigned int i, c;

    iter = listiter->begin();
    modifier = this->Modifiers.begin();
    while (modifier != this->Modifiers.end()) {
	    // check type requirement
      bool r = ((t == (*modifier)->RequireType) || ((*modifier)->RequireType == -1));
	    // check type exclusion
      bool e = ((t != (*modifier)->ExcludeType));
      if (e && r) {
        iter = listiter->begin();
        i = *countiter;
        while (i--) {
          state.Particle = &(*iter);
          (*modifier)->tick(state);
          ++iter;
        }
      }
      ++modifier;
    }

    iter = listiter->begin();
    i = 0;
    c = *countiter;
#ifdef _OPENMP
    bool* killList = StaticAllocate<bool>(ListBuffer, c);
    #pragma omp parallel    
    {
      #pragma omp for
      for (int i = 0; i < c; i++) {
        state.Particle = &(listiter->at(i));
        // particle::tick returns true if the particle has expired and needs to be removed
        kill = state.Particle->tick(state);
        killList[i] = kill;
      }
    }
    int kc = c;
    #pragma omp parallel    
    {
      #pragma omp for
      for (int i = 0; i < kc; i++) {
        if (killList[i]) {
          state.Particle = &(listiter->at(i));
	        assert(c > 0);
          if (state.Type->DieCallback) 
          {
            evt.Particle = state.Particle;
            state.Type->DieCallback(&evt);
          }
          // swap the dead particle with the last live particle in the particle list
          Particle temp = (*listiter)[c - 1];
          (*listiter)[c - 1] = (*listiter)[i];
          (*listiter)[i] = temp;
          // decrease the size of the particle list (the dead particle is now outside the list)
          c--;
        }
      }
    }
#else
    bool* killList = StaticAllocate<bool>(ListBuffer, c);
    while (i < c) {
      state.Particle = &(*iter);
      // particle::tick returns true if the particle has expired and needs to be removed
      kill = iter->tick(state);
      killList[i] = kill;
      //if (kill) {
		    //assert(c > 0);
      //  if (state.Type->DieCallback) 
      //  {
      //    evt.Particle = state.Particle;
      //    state.Type->DieCallback(&evt);
      //  }
      //  // swap the dead particle with the last live particle in the particle list
      //  Particle temp = (*listiter)[c - 1];
      //  (*listiter)[c - 1] = *iter;
      //  *iter = temp;
      //  // decrease the size of the particle list (the dead particle is now outside the list)
      //  c--;
      //} else {
        ++iter;
        i++;
      //}
    }
    int kc = c;
    for (int i = 0; i < kc; i++) {
      if (killList[i]) {
        state.Particle = &(listiter->at(i));
	      assert(c > 0);
        if (state.Type->DieCallback) 
        {
          evt.Particle = state.Particle;
          state.Type->DieCallback(&evt);
        }
        // swap the dead particle with the last live particle in the particle list
        Particle temp = (*listiter)[c - 1];
        (*listiter)[c - 1] = (*listiter)[i];
        (*listiter)[i] = temp;
        // decrease the size of the particle list (the dead particle is now outside the list)
        c--;
      }
    }
#endif

    // update the stored list count to reflect any dead particles from this update
    *countiter = c;

    ++countiter;
    ++listiter;
    ++typeiter;
    t++;
  }
}

#define generateProperty(id) \
  p.id = this->New ## id + (this->Random ## id * (engine.RNG.rand(2.0f) - 1.0f))
#define generatePropertyP(id) \
  p.id = this->New ## id + (this->Random ## id * (engine.RNG.rand()))

void ParticleGenerator::tick(const ParticleEngineState& state) {
  if (this->Life == 0) return;
  ParticleEngine& engine = *(state.Engine);
  float delay = this->GenerateDelay;
  if (delay == 0) 
	  delay = 1;
  else
	  delay = 1 / delay;
  if (this->CurrentDelay < 0) this->CurrentDelay = 0;
  this->CurrentDelay += state.Elapsed;
  while (this->CurrentDelay > delay) {
    this->CurrentDelay -= delay;
    if (this->Life > 0) {
      this->Life -= 1;
      if (this->Life < 0) 
        this->Life = 0;
    }

    Particle p;
    p.Type = this->Type;
    p.Frame = 0;

    int t = ClipValue(this->Type, engine.Types.size() - 1);
    ParticleListList::iterator list = engine.Particles.begin();
    while (t--)
      ++list;

    int i = this->GenerateRate;

    // prepare room for any new particles that will be added to the list this iteration
    engine.prespawn(*list, i + engine.ParticleCounts[ClipValue(this->Type, engine.Types.size() - 1)]);

    while (i--) {
      float r = (this->CurrentRotation * Radian);
      generateProperty(X);
      generateProperty(Y);
      generatePropertyP(A);
      generatePropertyP(L);
      generateProperty(XV);
      generateProperty(YV);
      generatePropertyP(AV);
      generatePropertyP(LV);
      float xd = sin(r), yd = -cos(r);
      float weight = this->NewR + (this->RandomR * engine.RNG.rand());
      p.X += (xd * weight);
      p.Y += (yd * weight);
      weight = this->NewRV + (this->RandomRV * engine.RNG.rand());
      p.XV += (xd * weight);
      p.YV += (yd * weight);
      engine.spawn(p, *list);
      this->CurrentRotation += (this->GenerateRotation + (this->RandomGenerateRotation * engine.RNG.rand()));
    }

    this->CurrentRotation = NormalizeAngle(this->CurrentRotation);
  }
}

#undef generateProperty
#undef generatePropertyP

void ParticleModifier::tick(const ParticleEngineState& state) {
  ParticleEngine& engine = *(state.Engine);
  Particle& particle = *(state.Particle);
  if (particle.X < this->Area.X1) return;
  if (particle.Y < this->Area.Y1) return;
  if (particle.X > this->Area.X2) return;
  if (particle.Y > this->Area.Y2) return;
  float xd = particle.X - this->X;
  float yd = particle.Y - this->Y;
  if (this->Range > 0) {
    if (xd > this->Range) return;
    if (yd > this->Range) return;
  }
  float distance2 = (xd * xd) + (yd * yd);
  float distance = sqrt(distance2);
  float weight = state.Elapsed;

  // calculate effect weight
  if (this->Range > 0) {
    weight = (this->Range - distance) / this->Range * (this->RangeScale);
    if (weight > 1) 
      weight = 1;
    else if (weight < 0) 
      weight = 0;
    weight *= state.Elapsed;
  }

  // apply decay
  decayUpdate(state.Type->XVDecayMode, particle.X, state.Type->XVDecay, weight, engine.RNG);
  decayUpdate(state.Type->YVDecayMode, particle.Y, state.Type->YVDecay, weight, engine.RNG);
  decayUpdate(state.Type->AVDecayMode, particle.A, state.Type->AVDecay, weight, engine.RNG);
  decayUpdate(state.Type->LVDecayMode, particle.L, state.Type->LVDecay, weight, engine.RNG);

  // apply gravity
  if ((weight != 0) && (this->Attraction != 0) && (distance != 0) && (this->Mass != 0)) {
    weight = engine.gravity(this->Mass, 1.0f, distance2) * weight * this->Attraction;
    particle.XV -= weight * (particle.X - this->X);
    particle.YV -= weight * (particle.Y - this->Y);
  }
}

FPoint generateReflectionVector(FPoint N, FPoint I) {
  // R= 2*(-I dot N)*N + I 
  I.normalize();
  N.normalize();
  float dot = N.dot(-I);
  FPoint R = N;
  R *= 2.0f * dot;
  R += I;
  R /= R.length();
  return R;
}

FPoint selectNormal(FPoint N, const FPoint& iP, const FPoint& pP) {
  // any given line segment has two perpendicular normals
  // this function selects the one facing in the direction we want
  // by determining which of the two is closest to the incoming point
  FPoint t = iP; t += N;
  float distance1 = (t.distance(pP));
  t = iP; t -= N;
  float distance2 = (t.distance(pP));
  if (distance2 > distance1)
    return -N;
  else
    return N;
}

bool Particle::tick(const ParticleEngineState& state) {
  ParticleEngine& engine = *(state.Engine);
  const ParticleType& type = *(state.Type);
  ParticleCollideEvent evt;
  evt.Engine = &engine;
  evt.Type = &type;
  evt.Particle = this;
  evt.UserData = type.UserData;

  // apply decay
  decayUpdate(type.XVDecayMode, X, type.XVDecay, state.Elapsed, engine.RNG);
  decayUpdate(type.YVDecayMode, Y, type.YVDecay, state.Elapsed, engine.RNG);
  decayUpdate(type.AVDecayMode, A, type.AVDecay, state.Elapsed, engine.RNG);
  decayUpdate(type.LVDecayMode, L, type.LVDecay, state.Elapsed, engine.RNG);

  // process movement
  FPoint newLocation;
  newLocation.X = this->X + this->XV * state.Elapsed;
  newLocation.Y = this->Y + this->YV * state.Elapsed;

  // process collision
  if (type.EnableCollision != 0) {
    FLine path;
    FPoint I;
    FPoint intersectPoint;
    FPoint intersectVector;
    float time = state.Elapsed;

    // generate the particle's projected path
    path.Start.X = this->X;
    path.Start.Y = this->Y;
    path.End = newLocation;
    FRect bounds = path.bounds();
    I = path.vector();

    if (engine.Surfaces != 0) {
      
      CollisionMatrix* matrix;
      matrix = engine.Surfaces;
      int sx1 = floor(bounds.X1 / matrix->SectorWidth) - 1, sy1 = floor(bounds.Y1 / matrix->SectorHeight) - 1, sx2 = ceil(bounds.X2 / matrix->SectorWidth) + 1, sy2 = ceil(bounds.Y2 / matrix->SectorHeight) + 1;
      
      // iterate through all the cells in the collision matrix that the path passes through
      for (int sy = sy1; sy <= sy2; sy++) {
        for (int sx = sx1; sx <= sx2; sx++) {

          CollisionSector* sector;
          sector = engine.Surfaces->getSector(sx, sy);
          if ((sector != 0)) {
            std::vector<FLine>::iterator iter = sector->Lines.begin();
            std::vector<FLine>::iterator enditer = sector->Lines.end();
            while (iter != enditer) {
              FLine test = *iter;
              // fudge line endpoints
              test.extend(ParticleLineExtension);
              if (test.intersect(path, intersectPoint)) {
                if (type.CollisionResponse != 0.0f) {
                  // process collision response
                  // select the perpendicular normal for the collision surface
                  FPoint N = selectNormal(iter->normal(), intersectPoint, path.Start);
                  // generate the reflection vector from the normal and incoming vector
                  FPoint R = generateReflectionVector(N, I);
                  // dampen the reflection vector by the response strength
                  R *= sqrt((this->XV * this->XV) + (this->YV * this->YV)) * type.CollisionResponse;
                  intersectVector = intersectPoint;
                  intersectVector -= path.Start;
                  intersectVector /= I;
                  // relocate the particle to something resembling the last point it occupied before collision
                  this->X = intersectPoint.X - (this->XV * CollisionEpsilon);
                  this->Y = intersectPoint.Y - (this->YV * CollisionEpsilon);
                  if (state.Type->CollideCallback)
                  {
                    evt.Vector = &R;
                    evt.Sprite = 0;
                    state.Type->CollideCallback(&evt);
                  }
                  // replace the particle's velocity with the new reflection velocity
                  this->XV = R.X;
                  this->YV = R.Y;
                  // reduce the remaining time to process for the particle
                  time -= intersectVector.length() * state.Elapsed;
                  // generate the next path for the particle
                  newLocation.X = this->X + (this->XV * time);
                  newLocation.Y = this->Y + (this->YV * time);
                  path.Start.X = this->X;
                  path.Start.Y = this->Y;
                  if (time <= 0) {
                    // no time remains for the particle to move so we can stop checking for collisions
                    newLocation = path.Start;
                    path.End = path.Start;
                    iter = enditer;
                    break;
                  } else {
                    // the particle has not moved for the elapsed time so continue checking for collisions
                    // until a clear path is found or all time is elapsed
                    path.End = newLocation;
                    I = path.vector();
                  }
                } else {
                  // relocate the particle to something resembling the last point it occupied before collision
                  this->X = intersectPoint.X - (this->XV * CollisionEpsilon);
                  this->Y = intersectPoint.Y - (this->YV * CollisionEpsilon);
                  // collision response is disabled, so we're done
                  newLocation.X = this->X;
                  newLocation.Y = this->Y;
                  this->XV = 0;
                  this->YV = 0;
                }
              }

              if (iter != enditer)
                ++iter;
            }
          }
          if (time <= 0)
            // particle's time has elapsed so we can stop iterating cells
            break;

        }

        if (time <= 0)
          // particle's time has elapsed so we can stop iterating cells
          break;
      }

    }

    if (engine.Sprites != 0) {
      SpriteParam** list = engine.Sprites;
      SpriteParam* guardian = reinterpret_cast<SpriteParam*>((void*)-1);
      if (*list == guardian) list = 0;
      while (list) {
        SpriteParam* current = *list;
        while (current) {
          FRect spriteRect = current->getRect();
          for (int e = 0; e < 4; e++) {
            FLine test = spriteRect.edge(e);
            test.extend(ParticleLineExtension);
            if (test.intersect(path, intersectPoint)) {
              if (type.CollisionResponse != 0.0f) {
                FPoint N = selectNormal(test.normal(), intersectPoint, path.Start);
                FPoint R = generateReflectionVector(N, I);
                R *= sqrt((this->XV * this->XV) + (this->YV * this->YV)) * type.CollisionResponse;
                intersectVector = intersectPoint;
                intersectVector -= path.Start;
                intersectVector /= I;
                this->X = intersectPoint.X - (this->XV * CollisionEpsilon);
                this->Y = intersectPoint.Y - (this->YV * CollisionEpsilon);
                if (state.Type->CollideCallback)
                {
                  evt.Vector = &R;
                  evt.Sprite = current;
                  state.Type->CollideCallback(&evt);
                }
                this->XV = R.X;
                this->YV = R.Y;
                time -= intersectVector.length() * state.Elapsed;
                newLocation.X = this->X + (this->XV * time);
                newLocation.Y = this->Y + (this->YV * time);
                path.Start.X = this->X;
                path.Start.Y = this->Y;
                if (time <= 0) {
                  newLocation = path.Start;
                  path.End = path.Start;
                  current = 0;
                } else {
                  path.End = newLocation;
                  I = path.vector();
                }
              } else {
                this->X = intersectPoint.X - (this->XV * CollisionEpsilon);
                this->Y = intersectPoint.Y - (this->YV * CollisionEpsilon);
                newLocation.X = this->X;
                newLocation.Y = this->Y;
                this->XV = 0;
                this->YV = 0;
              }
            }
          }
          if (current) 
            current = current->pNext;
        }
        list++;
        if (*list == guardian) list = 0;
      }
    }

  }

  // apply movement
  this->X = newLocation.X;
  this->Y = newLocation.Y;
  this->A += this->AV * state.Elapsed;
  this->L += this->LV * state.Elapsed;

  // process trivial culling (we return true to indicate the particle has 'died' and should be removed)
  if ((type.LBehavior == plbRemove) && (this->L <= ParticleMinimumL)) return true;
  if (!this->inside(engine.Bounds)) return true;

  // process animation
  if (type.Graphic) {
    this->Frame += type.Graphic->FrameIncrement * state.Elapsed;
    switch (type.Graphic->LoopMode) {
      case 0:
        // play once
        if (this->Frame < 0) this->Frame = 0;
        if (this->Frame > type.Graphic->FrameCount - 1) this->Frame = type.Graphic->FrameCount - 1;
        break;
      case 1:
        // loop
        if (this->Frame < 0) this->Frame += type.Graphic->FrameCount - 1;
        if (this->Frame > type.Graphic->FrameCount - 1) this->Frame -= type.Graphic->FrameCount - 1;
        break;
    }
  } else {
    this->Frame = 0;
  }

  return false;
}

void Particle::render(ParticleEngineState& state) {
  const ParticleType& type = *(state.Type);
  const ParticleCamera& camera = *(state.Camera);
  float x = this->X - camera.ViewportX + camera.Rectangle.Left, y = this->Y - camera.ViewportY + camera.Rectangle.Top;
  Pixel c = type.Color1, c2 = type.Color2;
  float s, r;
  Image *renderTarget = state.RenderTarget;
  Rectangle lineRect, destRect;
  Stroke stroke;
  StrokePoint strokePoints[2];
  RenderFunction *renderer;
  Image *texture;

  // if the particle's color is based on its life value, calculate the color
  if (type.LColorMode != plcNone) {
    switch (type.LColorMode) {
      case plcFade:
        c = ScaleAlpha(c, ClipByte(this->L * 255.0f));
        break;
      case plcInterpolate:
        float l = this->L;
        if (l > 1) l = 1;
        if (l < 0) l = 0;
        c = Pixel(type.Color1, type.Color2, 1 - l);
        break;
    }
  }

  switch (type.RenderType) {
    case prtPixel:
      renderTarget->setPixel(floor(x), floor(y), c);
      break;

    case prtAntiAliasPixel:
      switch (type.AMode) {
        case pamFadeAndScale:
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          break;
      }
      renderTarget->setPixelAA(x, y, c);
      break;

    case prtLine:
      switch (type.AMode) {
        case pamFadeAndScale:
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          break;
      }
      lineRect.setValuesAbsolute(floor(x), floor(y), floor(x + this->XV), floor(y + this->YV));
      FilterSimple_Line(renderTarget, &lineRect, c);
      break;

    case prtAntiAliasLine:
      switch (type.AMode) {
        case pamFadeAndScale:
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          break;
      }
      FilterSimple_Line_AA(renderTarget, x, y, x + this->XV, y + this->YV, c);
      break;

    case prtGradientLine:
      switch (type.AMode) {
        case pamFadeAndScale:
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          c2 = ScaleAlpha(type.Color2, ClipByte(_One(this->A + this->AV) * camera.Alpha * 255.0f));
          break;
      }
      lineRect.setValuesAbsolute(floor(x), floor(y), floor(x + this->XV), floor(y + this->YV));
      FilterSimple_Line_Gradient(renderTarget, &lineRect, c, c2);
      break;

    case prtAntiAliasGradientLine:
      switch (type.AMode) {
        case pamFadeAndScale:
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          c2 = ScaleAlpha(type.Color2, ClipByte(_One(this->A + this->AV) * camera.Alpha * 255.0f));
          break;
      }
      FilterSimple_Line_Gradient_AA(renderTarget, x, y, x + this->XV, y + this->YV, c, c2);
      break;

    case prtStroke:
      s = 1.0f;
      switch (type.AMode) {
        case pamScale:
          s *= _One(this->A);
          break;
        case pamFadeAndScale:
          s *= _One(this->A);
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          break;
      }
      renderer = state.Renderer;
      stroke.PointCount = 2;
      stroke.Loop = 0;
      stroke.Softness = type.Softness;
      stroke.Points = strokePoints;
      strokePoints[0].X = x;
      strokePoints[0].Y = y;
      strokePoints[0].Color = c.V;
      strokePoints[0].Thickness = type.Thickness * s;
      strokePoints[1].X = x + this->XV;
      strokePoints[1].Y = y + this->YV;
      strokePoints[1].Color = c.V;
      strokePoints[1].Thickness = type.Thickness * _One(this->A + this->AV);
      FilterSimple_RenderStroke(renderTarget, &stroke, renderer, 0);
      break;

    case prtGraphic:
      if (type.Graphic) {
        int a = type.Graphic->Alpha;
        s = 1.0f;
        r = 0.0f;
        switch (type.AMode) {
          case pamFadeAndScale:
            s *= _One(this->A);
            a = ClipByte(a * camera.Alpha * this->A);
            c = ScaleAlpha(c, a);
            break;
          case pamFade:
            a = ClipByte(a * camera.Alpha * this->A);
            c = ScaleAlpha(c, a);
            break;
          case pamRotate:
            r = this->A * 360.0f;
            break;
        }
        texture = type.Graphic->pFrames[(int)floor(this->Frame)];
        if (texture) {
          if ((s == 1.0f) && (r == 0.0f)) {
            // this particle is not scaled or rotated - do a trivial rectangular blit
            destRect.Left = ceil(x - type.Graphic->XCenter);
            destRect.Top = ceil(y - type.Graphic->YCenter);
            destRect.Width = texture->Width;
            destRect.Height = texture->Height;
            switch (type.RenderMode) {
              default:
                BlitSimple_Normal_Tint_Opacity(renderTarget, texture, &destRect, 0, 0, c, a);
                break;
              case 1:
                BlitSimple_SourceAlpha_Tint_Opacity(renderTarget, texture, &destRect, 0, 0, c, a);
                break;
              case 2:
                BlitSimple_Additive_Opacity(renderTarget, texture, &destRect, 0, 0, a);
                break;
              case 3:
                BlitSimple_Subtractive_Opacity(renderTarget, texture, &destRect, 0, 0, a);
                break;
              case 4:
                BlitSimple_Merge_Opacity(renderTarget, texture, &destRect, 0, 0, a);
                break;
              case 5:
                break;
              case 6:
                BlitSimple_Screen_Opacity(renderTarget, texture, &destRect, 0, 0, a);
                break;
              case 7:
                BlitSimple_Additive_SourceAlpha_Opacity(renderTarget, texture, &destRect, 0, 0, a);
                break;
              case 8:
                BlitSimple_Subtractive_SourceAlpha_Opacity(renderTarget, texture, &destRect, 0, 0, a);
                break;
              case 9:
                BlitSimple_Font_SourceAlpha_RGB_Opacity(renderTarget, texture, &destRect, 0, 0, c, 255);
                break;
            }
          } else {
            // this particle is scaled and/or rotated, so draw it using a quad
            renderer = state.Renderer;
            float px[4], py[4];
            float w = texture->Width, h = texture->Height;
            TexturedPolygon& poly = state.Poly;
            poly.Empty();
            r *= Radian;
            s /= 2;
            w *= s; h *= s;
            if (!((state.Camera->Rectangle.Left > x) || (state.Camera->Rectangle.Top > y) || (state.Camera->Rectangle.right() < x) || (state.Camera->Rectangle.bottom() < y))) {
              if (state.Theta != 0)
                Rotate4Points(w, h, r, px, py, state.Theta);
              else
                Rotate4Points(w, h, r, px, py);
              poly.Append(TexturedVertex(px[0] + x, py[0] + y, 0, 0));
              poly.Append(TexturedVertex(px[1] + x, py[1] + y, texture->Width - 1, 0));
              poly.Append(TexturedVertex(px[2] + x, py[2] + y, texture->Width - 1, texture->Height - 1));
              poly.Append(TexturedVertex(px[3] + x, py[3] + y, 0, texture->Height - 1));
              FilterSimple_ConvexPolygon_Textured(renderTarget, texture, &poly, DefaultSampleFunction, renderer, c.V);
            }
          }
        }
      }
      break;
  }
}

Export ParticleEngine* CreateParticleEngine() {
  return new ParticleEngine();
}

Export int DestroyParticleEngine(ParticleEngine *Engine) {
  if (Engine) {
    delete Engine;
    return Success;
  }
  return Failure;
}

Export int RemoveParticleModifier(ParticleEngine *Engine, ParticleModifier *Modifier) {
  if (!Engine) return Failure;
  if (!Modifier) return Failure;
  ParticleModifierList::iterator iter = Engine->Modifiers.begin();
  while (iter != Engine->Modifiers.end()) {
    if (*iter == Modifier) {
      Engine->Modifiers.erase(iter);
      return Success;
    }
    ++iter;
  }
  return Trivial_Success;
}

Export int AddParticleModifier(ParticleEngine *Engine, ParticleModifier *NewModifier) {
  if (!Engine) return Failure;
  if (!NewModifier) return Failure;
  RemoveParticleModifier(Engine, NewModifier);
  Engine->Modifiers.push_back(NewModifier);
  return Success;
}

Export int RemoveParticleGenerator(ParticleEngine *Engine, ParticleGenerator *Generator) {
  if (!Engine) return Failure;
  if (!Generator) return Failure;
  ParticleGeneratorList::iterator iter = Engine->Generators.begin();
  while (iter != Engine->Generators.end()) {
    if (*iter == Generator) {
      Engine->Generators.erase(iter);
      return Success;
    }
    ++iter;
  }
  return Trivial_Success;
}

Export int AddParticleGenerator(ParticleEngine *Engine, ParticleGenerator *NewGenerator) {
  if (!Engine) return Failure;
  if (!NewGenerator) return Failure;
  RemoveParticleGenerator(Engine, NewGenerator);
  Engine->Generators.push_back(NewGenerator);
  return Success;
}

Export int RemoveParticleType(ParticleEngine *Engine, ParticleType *Type) {
  if (!Engine) return Failure;
  if (!Type) return Failure;
  ParticleTypeList::iterator iter = Engine->Types.begin();
  ParticleListList::iterator liter = Engine->Particles.begin();
  ParticleCountList::iterator citer = Engine->ParticleCounts.begin();
  while (iter != Engine->Types.end()) {
    if (*iter == Type) {
      Engine->Types.erase(iter);
      Engine->Particles.erase(liter);
      Engine->ParticleCounts.erase(citer);
      return Success;
    }
    ++iter;
    ++liter;
    ++citer;
  }
  return Trivial_Success;
}

Export int AddParticleType(ParticleEngine *Engine, ParticleType *NewType) {
  if (!Engine) return Failure;
  if (!NewType) return Failure;
  RemoveParticleType(Engine, NewType);
  Engine->Types.push_back(NewType);
  Engine->Particles.push_back(ParticleList());
  Engine->ParticleCounts.push_back(0);
  return Success;
}

Export int SpawnParticle(ParticleEngine *Engine, Particle *NewParticle) {
  if (!Engine) return Failure;
  if (!NewParticle) return Failure;
  Engine->spawn(*NewParticle);
  return Success;
}

Export int TickGenerator(ParticleEngine *Engine, ParticleGenerator *Generator, float elapsed) {
  if (!Engine) return Failure;
  if (!Generator) return Failure;
  ParticleEngineState state = ParticleEngineState(*Engine, elapsed);
  Generator->tick(state);
  return Success;
}

Export int EmptyParticleEngine(ParticleEngine *Engine) {
  if (!Engine) return Failure;
  ParticleListList::iterator liter = Engine->Particles.begin();
  ParticleCountList::iterator citer = Engine->ParticleCounts.begin();
  while (liter != Engine->Particles.end()) {
    liter->clear();
    *citer = 0;
    ++liter;
    ++citer;
  }
  return Success;
}

Export int SetParticleEngineSize(ParticleEngine *Engine, FRect *Size) {
  if (!Engine) return Failure;
  if (!Size) return Failure;
  Engine->Bounds = *Size;
  return Success;
}

Export int SetParticleEngineSprites(ParticleEngine *Engine, SpriteParam** FirstSprite) {
  if (!Engine) return Failure;
  Engine->Sprites = FirstSprite;
  return Success;
}

Export int SetParticleEngineCollisionMatrix(ParticleEngine *Engine, CollisionMatrix *Matrix) {
  if (!Engine) return Failure;
  Engine->Surfaces = Matrix;
  return Success;
}

Export int UpdateParticleEngine(ParticleEngine *Engine, float ElapsedTime) {
  if (!Engine) return Failure;
  Engine->tick(ElapsedTime);
  return Success;
}

Export int RenderParticleEngine(ParticleEngine *Engine, ParticleCamera *Camera) {
  if (!Engine) return Failure;
  if (!Camera) return Failure;
  Engine->render(*Camera);
  return Success;
}

Export int GetParticleCount(ParticleEngine *Engine, int Type) {
  if (!Engine) return Failure;
  ParticleCountList::iterator citer = Engine->ParticleCounts.begin();
  int i = 0, c = 0;
  while (citer != Engine->ParticleCounts.end()) {
    // if Type is omitted (<= 0) we count particles of any type
    if ((i == Type) || (Type < 0))
      c += *citer;
    ++citer;
  }
  return c;
}