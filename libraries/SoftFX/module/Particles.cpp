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

#include "../header/SoftFX Main.hpp"
#include "../header/Particles.hpp"

#define decayUpdate(p, t, id) \
  if (t->id##VDecayMode != pdmNone) { \
    switch(t->id##VDecayMode) { \
      case pdmAdd: \
        p->id##V += t->id##VDecay; \
        break; \
      case pdmMultiply: \
        p->id##V *= t->id##VDecay; \
        break; \
      case pdmExponent: \
        p->id##V = pow(p->id##V, t->id##VDecay); \
        break; \
    } \
  }

void ParticleEngine::tick() {
  ParticleList::iterator iter = this->Particles.begin();
  ParticleModifierList::iterator modifier;
  ParticleType *type = Null;
  int type_id = -1;
  bool kill = false;
  while (iter != this->Particles.end()) {
    kill = false;
    if (iter->Type != type_id) {
      type_id = iter->Type;
      type = this->Types[type_id];
    }
    decayUpdate(iter, type, X);
    decayUpdate(iter, type, Y);
    decayUpdate(iter, type, A);
    decayUpdate(iter, type, L);
    modifier = this->Modifiers.begin();
    while (modifier != this->Modifiers.end()) {
      (*modifier)->tick(*iter);
      modifier++;
    }
    iter->tick();
    kill = ((type->LBehavior == plbRemove) && (iter->L <= 0));
    if (kill) {
      iter = this->Particles.erase(iter);
    } else {
      ++iter;
    }
  }
}

#undef decayUpdate

void ParticleModifier::tick(Particle& particle) {
  bool in_range = false;
  if (this->Range > 0) {
    float xd = abs(particle.X - this->X);
    float yd = abs(particle.Y - this->Y);
  } else {
    in_range = true;
  }
}

void Particle::tick() {
  this->X += this->XV;
  this->Y += this->YV;
  this->A += this->AV;
  this->L += this->LV;
}

Export ParticleEngine* CreateParticleEngine() {
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

Export int UpdateParticleEngine(ParticleEngine *Engine) {
  if (!Engine) return Failure;
  Engine->tick();
  return Success;
}

Export int RenderParticleEngine(Image *Surface, ParticleEngine *Engine, float XOffset, float YOffset) {
  if (!Engine) return Failure;
  if (!Surface) return Failure;
  Engine->render(Surface, XOffset, YOffset);
  return Success;
}