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
#ifndef _POLYGON_HPP_
#define _POLYGON_HPP_

inline Pixel GradientVertex::color() {
  return *(Pixel*)(&(this->Color));
}
inline void GradientVertex::setColor(Pixel V) {
  this->Color = V.V;
}

template <class T> inline FInterval CalculateInterval(FPoint axis, T& polygon) {
  int vertices = polygon.Count();
  if (vertices < 1) return FInterval(0, 0);
  float d=axis.dot(polygon.GetVertexPoint(0));
  FInterval value(d, d);
  for (int v = 0; v < vertices; v++) {
    d=polygon.GetVertexPoint(v).dot(axis);
    if (d < value.Min)
      value.Min = d;
    else
      if (d > value.Max)
        value.Max = d;
  }
  return value;
}

template <class T> inline bool Intersects(T& a, FLine& line) {
  if (a.Count() < 2) return false;
  FLine edge;
  FPoint temp = FPoint();
  int edges = a.GetEdgeCount();
  for (int e = 0; e < edges; e++) {
    edge = a.GetEdgeLine(e);
    if (line.intersect(edge, temp)) {
      return true;
    }
  }
  return false;
}

template <class T> inline bool Intersects(T& a, T& b) {
  if (a.Count() < 2) return false;
  if (b.Count() < 2) return false;
  if ((a.Count() == 2) || (b.Count() == 2)) {
    if (a.Count() != b.Count()) {
      FLine l = (a.Count() == 2) ? a.GetEdgeLine(0) : b.GetEdgeLine(0);
      return Intersects((a.Count() == 2) ? b : a, l);
    } else {
      FLine lA = a.GetEdgeLine(0);
      FLine lB = b.GetEdgeLine(0);
      FPoint temp = FPoint();
      return lA.intersect(lB, temp);
    }
  }
  int aEdges = a.GetEdgeCount();
  int bEdges = b.GetEdgeCount();
  FPoint normal;
  for (int e = 0; e < aEdges; e++) {
    normal = a.GetEdgeLine(e).normal();
    FInterval aInterval = CalculateInterval(normal, a), bInterval = CalculateInterval(normal, b);
    if (!aInterval.intersects(bInterval))
      return false;
  }
  for (int e = 0; e < bEdges; e++) {
    normal = b.GetEdgeLine(e).normal();
    FInterval aInterval = CalculateInterval(normal, a), bInterval = CalculateInterval(normal, b);
    if (!aInterval.intersects(bInterval))
      return false;
  }
  //for (int e = 0; e < aEdges; e++) {
  //  normal = a.GetEdgeLine(e).vector();
  //  FInterval aInterval = CalculateInterval(normal, a), bInterval = CalculateInterval(normal, b);
  //  if (!aInterval.intersects(bInterval))
  //    return false;
  //}
  //for (int e = 0; e < bEdges; e++) {
  //  normal = b.GetEdgeLine(e).vector();
  //  FInterval aInterval = CalculateInterval(normal, a), bInterval = CalculateInterval(normal, b);
  //  if (!aInterval.intersects(bInterval))
  //    return false;
  //}
  return true;
}

template <class T> inline T Intersection(T& a, T& b, Rectangle *Area, int Edge) {
  T c;
  c.X = 0;
  c.Y = 0;
  switch(Edge) {
  default:
  case 0: // left
    c.X = Area->Left;
    c.Y = a.Y + ((Area->Left - a.X) * (b.Y - a.Y) / (b.X - a.X));
    break;
  case 1: // top
    c.X = a.X + ((b.X - a.X) * (Area->Top - a.Y) / (b.Y - a.Y));
    c.Y = Area->Top;
    break;
  case 2: // right
    c.X = Area->right();
    c.Y = a.Y + ((b.Y - a.Y) * (Area->right() - a.X) / (b.X - a.X));
    break;
  case 3: // bottom
    c.X = a.X + ((b.X - a.X) * (Area->bottom() - a.Y) / (b.Y - a.Y));
    c.Y = Area->bottom();
    break;
  }
  return c;
}

inline GradientVertex Intersection(GradientVertex& a, GradientVertex& b, Rectangle *Area, int Edge) {
  GradientVertex c;
  float d = 0;
  c.X = 0;
  c.Y = 0;
  switch(Edge) {
  default:
  case 0:
    d = (Area->Left - a.X) / (b.X - a.X);
    c.X = Area->Left;
    c.Y = a.Y + ((b.Y - a.Y) * d);
    c.setColor(Pixel(a.color(), b.color(), d));
    break;
  case 1:
    d = (Area->Top - a.Y) / (b.Y - a.Y);
    c.X = a.X + ((b.X - a.X) * d);
    c.Y = Area->Top;
    c.setColor(Pixel(a.color(), b.color(), d));
    break;
  case 2:
    d = (Area->right() - a.X) / (b.X - a.X);
    c.X = Area->right();
    c.Y = a.Y + ((b.Y - a.Y) * d);
    c.setColor(Pixel(a.color(), b.color(), d));
    break;
  case 3:
    d = (Area->bottom() - a.Y) / (b.Y - a.Y);
    c.X = a.X + ((b.X - a.X) * d);
    c.Y = Area->bottom();
    c.setColor(Pixel(a.color(), b.color(), d));
    break;
  }
  return c;
}

inline TexturedVertex Intersection(TexturedVertex& a, TexturedVertex& b, Rectangle *Area, int Edge) {
  TexturedVertex c;
  float d = 0;
  c.X = 0;
  c.Y = 0;
  switch(Edge) {
  default:
  case 0:
    d = (Area->Left - a.X) / (b.X - a.X);
    c.X = Area->Left;
    c.Y = a.Y + ((b.Y - a.Y) * d);
    c.U = a.U + ((b.U - a.U) * d);
    c.V = a.V + ((b.V - a.V) * d);
    break;
  case 1:
    d = (Area->Top - a.Y) / (b.Y - a.Y);
    c.X = a.X + ((b.X - a.X) * d);
    c.Y = Area->Top;
    c.U = a.U + ((b.U - a.U) * d);
    c.V = a.V + ((b.V - a.V) * d);
    break;
  case 2:
    d = (Area->right() - a.X) / (b.X - a.X);
    c.X = Area->right();
    c.Y = a.Y + ((b.Y - a.Y) * d);
    c.U = a.U + ((b.U - a.U) * d);
    c.V = a.V + ((b.V - a.V) * d);
    break;
  case 3:
    d = (Area->bottom() - a.Y) / (b.Y - a.Y);
    c.X = a.X + ((b.X - a.X) * d);
    c.Y = Area->bottom();
    c.U = a.U + ((b.U - a.U) * d);
    c.V = a.V + ((b.V - a.V) * d);
    break;
  }
  return c;
}

template <class Vertex> class PolygonEdge {
public:
  Vertex Start;
  Vertex End;

  PolygonEdge(Vertex start, Vertex end) {
    Start = start;
    End = end;
  }

  inline FLine GetLine() {
    return FLine(FPoint(Start.X, Start.Y), FPoint(End.X, End.Y));
  }
};

template <class Vertex> class Polygon {
public:
  Vertex *Vertexes;
  int VertexCount;
  int InactiveVertexes; // muahahahaha dirty hack
  struct {
	Byte Reserved : 7;
	bool IsStatic : 1;
  };

  Polygon() {
    Vertexes = Null;
    VertexCount = 0;
    InactiveVertexes = 0;
    Reserved = 0;
  }

  Polygon(const Polygon& other) {
    Vertexes = Null;
    VertexCount = 0;
    InactiveVertexes = 0;
    Reserved = 0;
    Copy(other);
  }

  ~Polygon() {
    this->Deallocate();
  }

  inline void Empty() {
    this->InactiveVertexes = this->VertexCount;
  }

  inline void Copy (const Polygon& Source) {
    if ((this->Reserved == 0) || (this->VertexCount < Source.VertexCount)) this->Allocate(Source.VertexCount - Source.InactiveVertexes);
    _Copy<Vertex>(this->Vertexes, Source.Vertexes, this->VertexCount);
    this->InactiveVertexes = this->VertexCount - Source.VertexCount;
    return;
  }

  inline void Copy (Polygon* Source) {
    Copy(*Source);
    return;
  }

  inline void SetCount (int Count) {
    this->InactiveVertexes = ClipValue(this->VertexCount - Count, this->VertexCount);
    return;
  }

  inline void InsertVertex (int Index, Vertex* Value) {
    return InsertVertex(Index, *Value);
  }

  inline void InsertVertex (int Index, Vertex& Value) {
  int usedCount = this->VertexCount - this->InactiveVertexes;
    if (this->InactiveVertexes == 0) return;
    if (Index > (usedCount - 1)) {
      // insert at end; just set
      this->Vertexes[Index] = Value;
      this->InactiveVertexes--;
    } else {
      for (int move = usedCount - 1; move >= Index; move--) {
        this->Vertexes[move + 1] = this->Vertexes[move];
      }
      this->Vertexes[Index] = Value;
      this->InactiveVertexes--;
    }
    return;
  }

  inline void RemoveVertex (int Index) {
  int usedCount = this->VertexCount - this->InactiveVertexes;
    if (this->InactiveVertexes >= this->VertexCount) return;
    if (Index > (usedCount - 1)) {
      // remove at end; just adjust count
      this->InactiveVertexes++;
    } else {
      for (int move = Index; move <= usedCount; move++) {
        this->Vertexes[move] = this->Vertexes[move + 1];
      }
      this->InactiveVertexes++;
    }
    return;
  }

  inline void SetVertex (int Index, Vertex& Value) {
    this->Vertexes[Index] = Value;
    return;
  }

  inline Vertex& GetVertex (int Index) {
    return this->Vertexes[Index];
  }

  inline FPoint GetVertexPoint (int Index) {
    return FPoint(this->Vertexes[Index].X, this->Vertexes[Index].Y);
  }

  inline PolygonEdge<Vertex> GetEdge (int Index) {
    return PolygonEdge<Vertex>(this->Vertexes[Index], this->Vertexes[WrapValue(Index + 1, 0, VertexCount - InactiveVertexes - 1)]);
  }

  inline FLine GetEdgeLine (int Index) {
    return FLine(GetVertexPoint(Index), GetVertexPoint(WrapValue(Index + 1, 0, VertexCount - InactiveVertexes - 1)));
  }

  inline int GetEdgeCount() {
    return VertexCount - InactiveVertexes;
  }

  inline void Translate(float x, float y) {
    Vertex *CurrentVertex = Vertexes;
    for (int i = 0; i < VertexCount - InactiveVertexes; i++) {
      CurrentVertex->X += x;
      CurrentVertex->Y += y;
      CurrentVertex++;
    }
  }

  inline float MinimumX() const {
    Vertex *CurrentVertex = Vertexes;
    float Result = 99999999;
      for (int i = 0; i < VertexCount - InactiveVertexes; i++) {
        if (CurrentVertex->X < Result) Result = CurrentVertex->X;
        CurrentVertex++;
      }
      return Result;
  }
  inline float MinimumY() const {
    Vertex *CurrentVertex = Vertexes;
    float Result = 99999999;
      for (int i = 0; i < VertexCount - InactiveVertexes; i++) {
        if (CurrentVertex->Y < Result) Result = CurrentVertex->Y;
        CurrentVertex++;
      }
      return Result;
  }
  inline float MaximumX() const {
    Vertex *CurrentVertex = Vertexes;
    float Result = -99999999;
      for (int i = 0; i < VertexCount - InactiveVertexes; i++) {
        if (CurrentVertex->X > Result) Result = CurrentVertex->X;
        CurrentVertex++;
      }
      return Result;
  }
  inline float MaximumY() const {
    Vertex *CurrentVertex = Vertexes;
    float Result = -99999999;
      for (int i = 0; i < VertexCount - InactiveVertexes; i++) {
        if (CurrentVertex->Y > Result) Result = CurrentVertex->Y;
        CurrentVertex++;
      }
      return Result;
  }
  inline void GetBounds(FRect &Out) const {
    Vertex *CurrentVertex = Vertexes;
    Out.X1 = 99999999;
    Out.Y1 = 99999999;
    Out.X2 = -99999999;
    Out.Y2 = -99999999;
    for (int i = 0; i < VertexCount - InactiveVertexes; i++) {
      if (CurrentVertex->X < Out.X1) Out.X1 = CurrentVertex->X;
      if (CurrentVertex->Y < Out.Y1) Out.Y1 = CurrentVertex->Y;
      if (CurrentVertex->X > Out.X2) Out.X2 = CurrentVertex->X;
      if (CurrentVertex->Y > Out.Y2) Out.Y2 = CurrentVertex->Y;
      CurrentVertex++;
    }
  }

  inline int Count() {
    return this->VertexCount - this->InactiveVertexes;
  }

  inline void Append(Vertex &NewVertex) {
    if (this->InactiveVertexes < 1) return;
    this->Vertexes[this->VertexCount - (this->InactiveVertexes)] = NewVertex;
    this->InactiveVertexes--;
  }

  inline void Append(Vertex *NewVertex) {
    this->Append(*NewVertex);
  }

  inline void Finalize() {
    this->VertexCount = this->VertexCount - this->InactiveVertexes;
    this->InactiveVertexes = 0;
  }

  inline void Allocate(int Count) {
    this->Deallocate();
    if (Count < 1) return;
//    this->Vertexes = AllocateArray(Vertex, Count);
    this->Vertexes = LookupAllocate<Vertex>(Count);
	  this->IsStatic = false;
    this->VertexCount = Count;
    this->InactiveVertexes = Count;
    this->Reserved = 1;
  }

  inline void Allocate(int Count, StaticTables Table) {
    this->Deallocate();
    if (Count < 1) return;
//    this->Vertexes = AllocateArray(Vertex, Count);
//    this->Vertexes = LookupAllocate<Vertex>(Count);
	  this->Vertexes = StaticAllocate<Vertex>(Table, Count);
	  this->IsStatic = true;
    this->VertexCount = Count;
    this->InactiveVertexes = Count;
    this->Reserved = 1;
  }

  inline void Deallocate() {
    if ((this->Vertexes == Null) || (this->VertexCount == 0)) return;
    if (this->Reserved == 0) return;
//    DeleteArray(this->Vertexes);
	if (this->IsStatic)
	{
		this->Vertexes = Null;
	} 
	else
	{
	    LookupDeallocate(this->Vertexes);
		this->Vertexes = Null;
	}
	this->IsStatic = false;
    this->VertexCount = 0;
    this->Reserved = 0;
    this->InactiveVertexes = 0;
  }
};

template <class T> Polygon<T>* ClipPolygon(Polygon<T> *Poly, Rectangle *ClipRegion) {
  Polygon<T> *CurrentPoly;
  Polygon<T> *SourcePoly;
  T *a = Null, *b = Null;
  if (Poly->VertexCount < 2) {
    return Null;
  }

  CurrentPoly = new Polygon<T>;
  SourcePoly = Poly;

  CurrentPoly->Allocate(Poly->VertexCount * 4, PolyClipBuffer1);

  for (int edge = 0; edge < 4; edge++) {
    int point = 0;
    while (point < SourcePoly->Count()) {
      if (point == 0) {
        a = &(SourcePoly->Vertexes[SourcePoly->Count() - 1]);
      } else {
        a = &(SourcePoly->Vertexes[point - 1]);
      }
      b = &(SourcePoly->Vertexes[point]);
      
      if (b->inside(ClipRegion, edge)) {
        if (a->inside(ClipRegion, edge)) {
          CurrentPoly->Append(*b);
        } else {
          CurrentPoly->Append(Intersection(*a, *b, ClipRegion, edge));
          CurrentPoly->Append(*b);
        }
      } else {
        if (a->inside(ClipRegion, edge)) {
          CurrentPoly->Append(Intersection(*a, *b, ClipRegion, edge));
        } else {
//          CurrentPoly->Append(Intersection(*a, *b, ClipRegion, edge));
        }
      }

      point++;
    }
    if (SourcePoly == Poly) {
      SourcePoly = CurrentPoly;
      CurrentPoly = new Polygon<T>;
      CurrentPoly->Allocate(Poly->VertexCount * 4, PolyClipBuffer2);
    } else {
      _Swap(CurrentPoly, SourcePoly);
      CurrentPoly->InactiveVertexes = CurrentPoly->VertexCount;
    }
  }

  CurrentPoly->Deallocate();
  delete CurrentPoly;
  SourcePoly->Finalize();

  return SourcePoly;
}

typedef Polygon<FPoint> SimplePolygon;
typedef Polygon<TexturedVertex> TexturedPolygon;
typedef Polygon<GradientVertex> GradientPolygon;
#endif