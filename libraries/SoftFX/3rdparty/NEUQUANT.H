/* NeuQuant Neural-Net Quantization Algorithm Interface
 * ----------------------------------------------------
*/

#include <stdio.h>

/* For 256 colours, fixed arrays need 8kb, plus space for the image
   ---------------------------------------------------------------- */

/* four primes near 500 - assume no image has a length so large */
/* that it is divisible by all four primes */
#define prime1		499
#define prime2		491
#define prime3		487
#define prime4		503

#define minpicturebytes	(3*prime4)		/* minimum size for input image */


/* Initialise network in range (0,0,0) to (255,255,255) and set parameters
   ----------------------------------------------------------------------- */
void initnet(unsigned char *thepic, int len, int sample);

/* Unbias network to give byte values 0..255 and record position i to prepare for sort
   ----------------------------------------------------------------------------------- */
void unbiasnet();	/* can edit this function to do output of colour map */

/* Output colour map
   ----------------- */
void writecolourmap(FILE *f);

/* Insertion sort of network and building of netindex[0..255] (to do after unbias)
   ------------------------------------------------------------------------------- */
void inxbuild();

/* Search for BGR values 0..255 (after net is unbiased) and return colour index
   ---------------------------------------------------------------------------- */
int inxsearch(register int b, register int g, register int r);

/* Main Learning Loop
   ------------------ */
void learn();

/* Program Skeleton
   ----------------
	[select samplefac in range 1..30]
	[read image from input file]
	pic = (unsigned char*) malloc(3*width*height);
	initnet(pic,3*width*height,samplefac);
	learn();
	unbiasnet();
	[write output image header, using writecolourmap(f)]
	inxbuild();
	write output image using inxsearch(b,g,r)		*/
