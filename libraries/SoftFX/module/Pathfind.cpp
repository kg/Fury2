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
#include "../header/Filters.hpp"
#include "../header/Pathfind.hpp"
#include <stack>
#include <vector>
#include <list>

const int max_tree_depth = 64;

class treeNode;
class buildState;

class buildState {
public:
  treeNode *node;
  FLine* obstruction;
  int leaf;

  buildState() {
    node = Null;
    obstruction = Null;
    leaf = 0;
  }

  buildState(treeNode *nod) {
    node = nod;
    obstruction = Null;
    leaf = 0;
  }

  buildState(treeNode *nod, FLine *obs) {
    node = nod;
    obstruction = obs;
    leaf = 0;
  }
};

class treeNode {
public:
  treeNode* up;
  treeNode* left;
  treeNode* right;
  FPoint point;
  int depth;

  treeNode() {
    up = Null;
    left = Null;
    right = Null;
    depth = 0;
  }

  treeNode(FPoint pt) {
    up = Null;
    left = Null;
    right = Null;
    point = pt;
    depth = 0;
  }

  ~treeNode() {
    up = Null;
    if (left) delete left;
    left = Null;
    if (right) delete right;
    right = Null;
  }

  void pushLeft(treeNode *node) {
    node->up = this;
    node->depth = this->depth + 1;
    left = node;
  }

  void pushRight(treeNode *node) {
    node->up = this;
    node->depth = this->depth + 1;
    right = node;
  }
};

typedef void (drawcbk)(void);

void drawTree(Image* img, treeNode* node) {
  img->setPixelAA(node->point.X, node->point.Y, Pixel(255,255,255,255));
  if (node->left) {
    FilterSimple_Line_AA(img, node->point.X, node->point.Y, node->left->point.X, node->left->point.Y, Pixel(255, 0, 0, 255));
    drawTree(img, node->left);
  }
  if (node->right) {
    FilterSimple_Line_AA(img, node->point.X, node->point.Y, node->right->point.X, node->right->point.Y, Pixel(0, 255, 0, 255));
    drawTree(img, node->right);
  }
  return;
}

typedef std::list<buildState> stateStack;

Export int PathFind_Vector(FPoint *start, FPoint *end, FLine *obstructions, int obstructioncount, FPoint *pathbuffer, int pathbuffersize, Image* drawbuffer, drawcbk* drawcallback) {
  if (!start) return Failure;
  if (!end) return Failure;
  if (!obstructions) return Failure;
  //if (!pathbuffer) return Failure;
  //if (pathbuffersize < 2) return Failure;
  if (obstructioncount < 1) {
    pathbuffer[0] = *start;
    pathbuffer[1] = *end;
    return Trivial_Success;
  }
  treeNode *root = new treeNode(*start);
  treeNode *tail = new treeNode(*end);
  treeNode *current, *newnode;
  root->pushLeft(tail);
  FLine currentLine;
  FPoint where, newpoint;
  FVector vector, newvector;
  float vector_length, vector_angle;
  int leaf = 0, closest_obstruction;
  float closest_obstruction_distance;
  stateStack stack;
  stack.push_front(buildState(root, Null));
  drawbuffer->clear();
  drawTree(drawbuffer, root);
  drawcallback();
  while (stack.size() > 0) {
    leaf = stack.front().leaf;
    switch (leaf) {
      case 0:
        current = stack.front().node->left;
        stack.front().leaf++;
        break;
      case 1:
        current = stack.front().node->right;
        stack.front().leaf++;
        break;
      case 2:
        stack.pop_front();
        continue;
        break;
    }
    if (current) {
      currentLine.Start = current->up->point;
      currentLine.End = current->point;
      closest_obstruction = -1;
      closest_obstruction_distance = 999999;
      for (int o = 0; o < obstructioncount; o++) {
        bool skip = false;
        if (currentLine.intersect(obstructions[o], where)) {
          for (stateStack::iterator iter = stack.begin(); iter != stack.end(); iter++) {
            if (iter->obstruction == &(obstructions[o])) {
              skip = true;
              break;
            }
          }
          if (!skip) {
            vector = FVector(current->up->point, where);
            if (vector.length() < closest_obstruction_distance) {
              closest_obstruction = o;
              closest_obstruction_distance = vector.length();
            }
          }
        }
      }
      if (closest_obstruction > -1) {
        if (stack.front().node->depth < max_tree_depth) {
          vector = FVector(obstructions[closest_obstruction].Start, obstructions[closest_obstruction].End);
          vector_length = vector.length();
          vector_angle = AngleBetween(obstructions[closest_obstruction].Start, obstructions[closest_obstruction].End);
          vector.X = sin(vector_angle * Radian);
          vector.Y = -cos(vector_angle * Radian);
          newvector = vector;
          newvector.Multiply(-1);
          newpoint = obstructions[closest_obstruction].Start;
          newpoint.Translate(newvector.X, newvector.Y);
          newnode = new treeNode(newpoint);
          newnode->pushLeft(new treeNode(*end));
          current->up->pushLeft(newnode);
          newvector = vector;
          newvector.Multiply(vector_length + 1);
          newpoint = obstructions[closest_obstruction].Start;
          newpoint.Translate(newvector.X, newvector.Y);
          newnode = new treeNode(newpoint);
          newnode->pushLeft(new treeNode(*end));
          current->up->pushRight(newnode);
          if (leaf == 0) {
            if (current->up->left)
              stack.push_front(buildState(current->up->left, &(obstructions[closest_obstruction])));
          } else {
            if (current->up->right)
              stack.push_front(buildState(current->up->right, &(obstructions[closest_obstruction])));
          }
        }
      }
    }
    drawbuffer->clear();
    drawTree(drawbuffer, root);
    if (current)
      drawbuffer->setPixelAA(current->point.X, current->point.Y, Pixel(0,0,255,255));
    drawcallback();
  }
  return Success;
}