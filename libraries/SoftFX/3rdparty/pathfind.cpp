#include "stdafx.h"
#include "F2SoftFX.h"
#include <stdlib.h>
#include <math.h>

#define Path8 1

struct NODE
{
	int  x,    // X pos of the tile
		 y;	   // Y pos of the tile
	long f;    // the cost to move from the start to the node and from the node to the goal(the heuristic)
	int	Parent,// this identifies the parent from which the successors was created(NULL if it is the first node)
		ID;    // the id which the parent will refer to
};

class AStar
{
	void ConstructPath(NODE n);
	bool IsInOPEN(NODE n);
	bool IsInCLOSED(NODE n);
	void AddToOPEN(NODE n);
	void AddToCLOSED(NODE n);
	void RemoveFromOPEN(NODE n);
	void GenerateSuccs(NODE n);
	void GenerateSucc(int x,int y,int Parent);
	NODE GetBestNode();
	NODE GetNodeFromCLOSED(int ID);
	int nOpen,nClosed,cOpen,cClosed,cID;
	NODE nStart,nGoal;
	NODE* OPEN;
	NODE* CLOSED;
public:
	f2geblockbuffer blocking;
	int nPath;
	NODE* PATH;
	bool FindPath(POINT StartPos,POINT EndPos);	
}Path;

/* --| ASTAR FUNCTION |-----------------------------------------------------------------------*\
| Name: ConstructPath
| Desc: constructs a path trough the .Parent value of the node that found the goal
\* --| ASTAR FUNCTION |-----------------------------------------------------------------------*/
void AStar::ConstructPath(NODE n)
{
	PATH  =(NODE*) malloc(300 *sizeof(NODE));
	nPath=0;
	NODE nTemp=n;
	int a=0;
	NODE *AltPath;
	while (nTemp.ID != -1)
	{
		PATH[nPath++]=nTemp;	
		nTemp=GetNodeFromCLOSED(nTemp.Parent);
	}
	PATH[nPath++]=nTemp;	
	AltPath=(NODE*) malloc(nPath*sizeof(NODE));
	
	for (int i=nPath-1;i>-1;i--)
	{
		AltPath[a++]=PATH[i];
	}

	free(PATH);
	PATH=AltPath;
}
/* --| ASTAR FUNCTION |-----------------------------------------------------------------------*\
| Name: IsInOPEN
| Desc: Verifies if a node is in the OPEN list
\* --| ASTAR FUNCTION |-----------------------------------------------------------------------*/
bool AStar::IsInOPEN(NODE n)
{
	if (OPEN==NULL) return false;

	int Len=(sizeof(*OPEN)*cOpen)/sizeof(NODE);
	for (int i=0;i<Len;i++)
	{
		if (OPEN[i].x == n.x && OPEN[i].y == n.y)
			break;
	}
	if (i==Len) return false;
	return true;
}
/* --| ASTAR FUNCTION |-----------------------------------------------------------------------*\
| Name: IsInCLOSED
| Desc: Verifies if a node is in the CLOSED list
\* --| ASTAR FUNCTION |-----------------------------------------------------------------------*/
bool AStar::IsInCLOSED(NODE n)
{
	if (CLOSED==NULL) return false;

	int Len=(sizeof(*CLOSED)*cClosed)/sizeof(NODE);
	for (int i=0;i<Len;i++)
	{
		if (CLOSED[i].x == n.x && CLOSED[i].y == n.y)
			break;
	}
	if (i==Len) return false;
	return true;
}
/* --| ASTAR FUNCTION |-----------------------------------------------------------------------*\
| Name: AddToOPEN
| Desc: Adds a node to the OPEN list
\* --| ASTAR FUNCTION |-----------------------------------------------------------------------*/
void AStar::AddToOPEN(NODE n)
{
	if (cOpen>nOpen)
	{
		nOpen+=100;
		OPEN=(NODE*) realloc(OPEN,sizeof(NODE)*nOpen);
	}
	OPEN[cOpen++]=n;
}
/* --| ASTAR FUNCTION |-----------------------------------------------------------------------*\
| Name: AddToClosed
| Desc: Adds a node to the CLOSED list
\* --| ASTAR FUNCTION |-----------------------------------------------------------------------*/
void AStar::AddToCLOSED(NODE n)
{
	if (cClosed>nClosed)
	{
		nClosed+=100;
		CLOSED=(NODE*) realloc(CLOSED,sizeof(NODE)*nClosed);
	}
	CLOSED[cClosed++]=n;
}
/* --| ASTAR FUNCTION |-----------------------------------------------------------------------*\
| Name: RemoveFromOPEN
| Desc: Removes a node from the OPEN list
\* --| ASTAR FUNCTION |-----------------------------------------------------------------------*/
void AStar::RemoveFromOPEN(NODE n)
{
	if (OPEN==NULL) return;

	int Len=(sizeof(*OPEN)*cOpen)/sizeof(NODE);
	for (int i=0;i<Len;i++)
		if (OPEN[i].x == n.x && OPEN[i].y == n.y)
			break;
	for (int j=i;j<cOpen;j++)
		OPEN[j]=OPEN[j+1];
	cOpen--;
}
/* --| ASTAR FUNCTION |-----------------------------------------------------------------------*\
| Name: GenerateSucc
| Desc: Generates a node and adds it to OPEN
\* --| ASTAR FUNCTION |-----------------------------------------------------------------------*/
void AStar::GenerateSucc(int x,int y,int Parent)
{
	// First we check if the new successor is ot of the Map area or is a
	// forbidden tile.So if it is clear for movement,create the node and
	// if the node is not already created(is not in OPEN) and it is not 
	// in CLOSED then create it
	if (x<0) return; 
	if (y<0) return;
	if (x>=blocking.width) return;
	if (y>=blocking.height) return;
	if (blocking.values[(y * blocking.width) + (x)] != 0) return;
	
	NODE n;
	
	n.x=x; n.y=y; n.ID=cID++; n.Parent=Parent;
	n.f=(abs(nGoal.x - n.x)+1) * (abs(nGoal.y - n.y)+1);
	if (!IsInOPEN(n) && !IsInCLOSED(n)) 
		AddToOPEN(n);
}
/* --| ASTAR FUNCTION |-----------------------------------------------------------------------*\
| Name: GenerateSuccs
| Desc: Generate successors for the node
\* --| ASTAR FUNCTION |-----------------------------------------------------------------------*/
void AStar::GenerateSuccs(NODE n)
{
#if (Path8)
	GenerateSucc(n.x-1,n.y-1,n.ID); // Upper Left
	GenerateSucc(n.x  ,n.y-1,n.ID); // Upper Center
	GenerateSucc(n.x+1,n.y-1,n.ID); // Upper Right
	GenerateSucc(n.x-1,n.y  ,n.ID); // Center Left
	GenerateSucc(n.x+1,n.y  ,n.ID); // Center Right
	GenerateSucc(n.x-1,n.y+1,n.ID); // Lower Left
	GenerateSucc(n.x  ,n.y+1,n.ID); // Lower Center
	GenerateSucc(n.x+1,n.y+1,n.ID); // Lower Right
#else
	GenerateSucc(n.x  ,n.y-1,n.ID); // Upper Center
	GenerateSucc(n.x-1,n.y  ,n.ID); // Center Left
	GenerateSucc(n.x+1,n.y  ,n.ID); // Center Right
	GenerateSucc(n.x  ,n.y+1,n.ID); // Lower Center
#endif

	RemoveFromOPEN(n);
	AddToCLOSED(n);
}
/* --| ASTAR FUNCTION |-----------------------------------------------------------------------*\
| Name: GetNodeFromCLOSED
| Desc: Retrieves the node from the CLOSED list (for path construction)
\* --| ASTAR FUNCTION |-----------------------------------------------------------------------*/
NODE AStar::GetNodeFromCLOSED(int ID)
{
	for (int i=0;i<cClosed;i++)
		if (CLOSED[i].ID == ID)
			return CLOSED[i];
	return CLOSED[0]; // to silence the warning
	
}
/* --| ASTAR FUNCTION |-----------------------------------------------------------------------*\
| Name: GetBestNode
| Desc: Retrieves the node that is the closest to the goal (defined by the heuristic)
\* --| ASTAR FUNCTION |-----------------------------------------------------------------------*/
NODE AStar::GetBestNode()
{
	long f=OPEN[0].f;
	int ArrPos=0;
	int Len=(sizeof(*OPEN)*cOpen)/sizeof(NODE);
	NODE tNode;
	
	for (int i=1;i<Len;i++)
	{
		if (OPEN[i].f<=f) 
		{
			f=OPEN[i].f;
			ArrPos=i;
		}
	}
	tNode.f		=OPEN[ArrPos].f;
	tNode.x		=OPEN[ArrPos].x;
	tNode.y		=OPEN[ArrPos].y;
	tNode.Parent=OPEN[ArrPos].Parent;
	tNode.ID	=OPEN[ArrPos].ID;
	return tNode;
}
/* --| ASTAR FUNCTION |-----------------------------------------------------------------------*\
| Name: FindPath
| Desc: the main function of AStar,this one finds the path,or not,and stores it in the PATH
\* --| ASTAR FUNCTION |-----------------------------------------------------------------------*/
bool AStar::FindPath(POINT StartPos,POINT EndPos)
{
	// -- Memory allocation -------------------
	if (nPath>0 && nPath<300) free(PATH);
	OPEN  =(NODE*) malloc(1000*sizeof(NODE));
	CLOSED=(NODE*) malloc(1000*sizeof(NODE));
	nOpen  =1000;
	nClosed=1000;
	cOpen  =1;
	cClosed=0;
	cID=0;
	// -- Search initialization ---------------
	NODE n;
	n.f=n.Parent=n.ID=-1;n.x=StartPos.x;n.y=StartPos.y;
	nStart=n;
	n.f=n.Parent=n.ID=0;n.x= EndPos.x;n.y= EndPos.y;
	nGoal=n;
	OPEN[0]=nStart;
	// -- Pathfinding -------------------------
	while (cOpen) // Do while OPEN is not empty,if OPEN is empty,it means that the path was not found
	{
 		n=GetBestNode();
		if (n.x==nGoal.x && n.y==nGoal.y)
			break;

		GenerateSuccs(n);
	}
	// -- Memory deallocation -----------------
	if (cOpen !=0) ConstructPath(n);
	free(OPEN);
	free(CLOSED);

	if (cOpen==0)
		return false;
	else
		return true;
}

F2FX int f2geAStarPathfind(f2gepathbuffer * pathbuffer, f2geblockbuffer * blocking, int startx, int starty, int endx, int endy) {
AStar * Pathfinder = new AStar;
POINT ptStart, ptEnd;
int nCopy = 0;
	ptStart.x = startx;
	ptStart.y = starty;
	ptEnd.x = endx;
	ptEnd.y = endy;
	Pathfinder->blocking = *blocking;
	if (Pathfinder->FindPath(ptStart, ptEnd)) {
		pathbuffer->path = (f2gepathpoint *)GlobalAlloc(GPTR, Pathfinder->nPath * 8);
		pathbuffer->size = Pathfinder->nPath;
		for (nCopy = 0; nCopy < pathbuffer->size; nCopy++) {
			pathbuffer->path[nCopy].x = Pathfinder->PATH[nCopy].x;
			pathbuffer->path[nCopy].y = Pathfinder->PATH[nCopy].y;
		}
		return true;
	} else {
		pathbuffer->path = (f2gepathpoint *)NULL;
		pathbuffer->size = 0;
		return false;
	}
}
