/* NeuQuant Neural-Net Quantization Algorithm
 * ------------------------------------------
 Code found on the Neuquant web page: http://acm.org/~dekker/NEUQUANT.HTML
 */

#define F2FX extern "C" __declspec(dllexport)

// number of colours used
#define NETSIZE		256

// 1 slower, 30 faster [1..30]
//#define SFACTOR         15 // 1 15 30

#include "neuquant.h"

int netsize = NETSIZE;

/* Network Definitions
   ------------------- */

#define maxnetpos	(netsize-1)
#define netbiasshift	4	/* bias for colour values */
#define ncycles		100	/* no. of learning cycles */

/* defs for freq and bias */
#define intbiasshift    16	/* bias for fractions */
#define intbias		(((int) 1)<<intbiasshift)
#define gammashift  	10	/* gamma = 1024 */
#define gamma   	(((int) 1)<<gammashift)
#define betashift  	10
#define beta		(intbias>>betashift)	/* beta = 1/1024 */
#define betagamma	(intbias<<(gammashift-betashift))

/* defs for decreasing radius factor */
#define INITRAD		(NETSIZE>>3)	/* for 256 cols, radius starts */
#define initrad		(netsize>>3)	/* for 256 cols, radius starts */
#define radiusbiasshift	6	/* at 32.0 biased by 6 bits */
#define radiusbias	(((int) 1)<<radiusbiasshift)
#define initradius	(initrad*radiusbias)	/* and decreases by a */
#define radiusdec	30	/* factor of 1/30 each cycle */

/* defs for decreasing alpha factor */
#define alphabiasshift	10	/* alpha starts at 1.0 */
#define initalpha	(((int) 1)<<alphabiasshift)
int             alphadec;	/* biased by 10 bits */

/* radbias and alpharadbias used for radpower calculation */
#define radbiasshift	8
#define radbias		(((int) 1)<<radbiasshift)
#define alpharadbshift  (alphabiasshift+radbiasshift)
#define alpharadbias    (((int) 1)<<alpharadbshift)


/* Types and Global Variables
   -------------------------- */

static unsigned char *thepicture;	/* the input image itself */
static int      lengthcount;	/* lengthcount = H*W*4 */
static int      samplefac;	/* sampling factor 1..30 */
static int      colorsreq;
static int      bpp;

typedef int     pixel[4];	/* BGRc */
static pixel    network[NETSIZE];	/* the network itself */

static int      netindex[256];	/* for network lookup - really 256 */

static int      bias[NETSIZE];	/* bias and freq arrays for learning */
static int      freq[NETSIZE];
static int      radpower[INITRAD];	/* radpower for precomputation */


/* Initialise network in range (0,0,0) to (255,255,255) and set parameters
   ----------------------------------------------------------------------- */

/*
   void initnet(thepic,len,sample,output_size)
   unsigned char *thepic;
   int len;
   int sample;
   int output_size;
   { */
static void initnet(unsigned char *thepic, int len,int sample)
{
     int    i, *p;

/*
   netsize = output_size;

   network = (pixel *)malloc(netsize*sizeof(pixel));
   bias = (int *)malloc(netsize*sizeof(int));
   freq = (int *)malloc(netsize*sizeof(int));
   radpower = (int *)malloc(initrad*sizeof(int));

   if(!network || !bias || !freq || !radpower)exit(1);
 */

    thepicture = thepic;
    lengthcount = len;
    samplefac = sample;

    for (i = 0; i < netsize; i++) {
	p = network[i];
	p[0] = p[1] = p[2] = (i << (netbiasshift + 8)) / netsize;
	freq[i] = intbias / netsize;	/* 1/netsize */
	bias[i] = 0;
    }
}


/* Unbias network to give byte values 0..255 and record position i to prepare for sort
   ----------------------------------------------------------------------------------- */

static void            unbiasnet()
{
    int             i, j;

    for (i = 0; i < netsize; i++) {
	for (j = 0; j < 3; j++)
	    network[i][j] >>= netbiasshift;
	network[i][3] = i;	/* record colour no */
    }
}


/* Output colour map
   -----------------
static void            writecolourmap(FILE *f)
{
    int             i, j;

    for (i = 2; i >= 0; i--)
	for (j = 0; j < netsize; j++)
	    putc(network[j][i], f);
}
*/


/* Insertion sort of network and building of netindex[0..255]
   (to do after unbias)
   ----------------------------------------------------------
   --------------------- */

static void            inxbuild()
{
     int    i, j, smallpos, smallval;
     int   *p, *q;
    int             previouscol, startpos;

    previouscol = 0;
    startpos = 0;
    for (i = 0; i < netsize; i++) {
	p = network[i];
	smallpos = i;
	smallval = p[1];	/* index on g */
	/* find smallest in i..netsize-1 */
	for (j = i + 1; j < netsize; j++) {
	    q = network[j];
	    if (q[1] < smallval) {	/* index on g */
		smallpos = j;
		smallval = q[1];	/* index on g */
	    }
	}
	q = network[smallpos];
	/* swap p (i) and q (smallpos) entries */
	if (i != smallpos) {
	    j = q[0];
	    q[0] = p[0];
	    p[0] = j;
	    j = q[1];
	    q[1] = p[1];
	    p[1] = j;
	    j = q[2];
	    q[2] = p[2];
	    p[2] = j;
	    j = q[3];
	    q[3] = p[3];
	    p[3] = j;
	}
	/* smallval entry is now in position i */
	if (smallval != previouscol) {
	    netindex[previouscol] = (startpos + i) >> 1;
	    for (j = previouscol + 1; j < smallval; j++)
		netindex[j] = i;
	    previouscol = smallval;
	    startpos = i;
	}
    }
    netindex[previouscol] = (startpos + maxnetpos) >> 1;
    for (j = previouscol + 1; j < 256; j++)
	netindex[j] = maxnetpos;	/* really 256 */
}


/* Search for BGR values 0..255 (after net is unbiased) and
   return colour index
   --------------------------------------------------------
   -------------------- */
static int             inxsearch(int    b, int g, int r)
{
    int    i, j, dist, a, bestd;
    int   *p;
    int             best;

    bestd = 1000;		/* biggest possible dist is 256*3 */
    best = -1;
    i = netindex[g];		/* index on g */
    j = i - 1;			/* start at netindex[g] and work outwards */

    while ((i < netsize) || (j >= 0)) {
	if (i < netsize) {
	    p = network[i];
	    dist = p[1] - g;	/* inx key */
	    if (dist >= bestd)
		i = netsize;	/* stop iter */
	    else {
		i++;
		if (dist < 0)
		    dist = -dist;
		a = p[0] - b;
		if (a < 0)
		    a = -a;
		dist += a;
		if (dist < bestd) {
		    a = p[2] - r;
		    if (a < 0)
			a = -a;
		    dist += a;
		    if (dist < bestd) {
			bestd = dist;
			best = p[3];
		    }
		}
	    }
	}
/*        if (j >= 0) */   // not needed (Weber)
        {
	    p = network[j];
	    dist = g - p[1];	/* inx key - reverse dif */
	    if (dist >= bestd)
		j = -1;		/* stop iter */
	    else {
		j--;
		if (dist < 0)
		    dist = -dist;
		a = p[0] - b;
		if (a < 0)
		    a = -a;
		dist += a;
		if (dist < bestd) {
		    a = p[2] - r;
		    if (a < 0)
			a = -a;
		    dist += a;
		    if (dist < bestd) {
			bestd = dist;
			best = p[3];
		    }
		}
	    }
	}
    }
    return (best);
}


/* Search for biased BGR values
   ---------------------------- */

static int             contest(int b, int g, int r)
{
    /* finds closest neuron (min dist) and updates freq */
    /* finds best neuron (min dist-bias) and returns position */
    /* for frequently chosen neurons, freq[i] is high and bias[i] is negative */
    /* bias[i] = gamma*((1/netsize)-freq[i]) */

    int    i, dist, a, biasdist, betafreq;
    int    bestpos, bestbiaspos, bestd, bestbiasd;
    int   *p, *f, *n;

    bestd = ~(((int) 1) << 31);
    bestbiasd = bestd;
    bestpos = -1;
    bestbiaspos = bestpos;
    p = bias;
    f = freq;

    for (i = 0; i < netsize; i++) {
	n = network[i];
	dist = n[0] - b;
	if (dist < 0)
	    dist = -dist;
	a = n[1] - g;
	if (a < 0)
	    a = -a;
	dist += a;
	a = n[2] - r;
	if (a < 0)
	    a = -a;
	dist += a;
	if (dist < bestd) {
	    bestd = dist;
	    bestpos = i;
	}
	biasdist = dist - ((*p) >> (intbiasshift - netbiasshift));
	if (biasdist < bestbiasd) {
	    bestbiasd = biasdist;
	    bestbiaspos = i;
	}
	betafreq = (*f >> betashift);
	*f++ -= betafreq;
	*p++ += (betafreq << gammashift);
    }
    freq[bestpos] += beta;
    bias[bestpos] -= betagamma;
    return (bestbiaspos);
}


/* Move neuron i towards biased (b,g,r) by factor alpha
   ---------------------------------------------------- */

static void            altersingle(int alpha, int i, int b, int g, int r)
{
     int   *n;

    n = network[i];		/* alter hit neuron */
    *n -= (alpha * (*n - b)) / initalpha;
    n++;
    *n -= (alpha * (*n - g)) / initalpha;
    n++;
    *n -= (alpha * (*n - r)) / initalpha;
}


/* Move adjacent neurons by precomputed alpha*(1-((i-j)^2/[r]^2))
   in radpower[|i-j|]
   --------------------------------------------------------------
   ------------------- */

static void            alterneigh(int rad, int i, int b, int g, int r)
{
     int    j, k, lo, hi, a;
     int   *p, *q;

    lo = i - rad;
    if (lo < -1)
	lo = -1;
    hi = i + rad;
    if (hi > netsize)
	hi = netsize;

    j = i + 1;
    k = i - 1;
    q = radpower;
    while ((j < hi) || (k > lo)) {
	a = (*(++q));
	if (j < hi) {
	    p = network[j];
	    *p -= (a * (*p - b)) / alpharadbias;
	    p++;
	    *p -= (a * (*p - g)) / alpharadbias;
	    p++;
	    *p -= (a * (*p - r)) / alpharadbias;
	    j++;
	}
	if (k > lo) {
	    p = network[k];
	    *p -= (a * (*p - b)) / alpharadbias;
	    p++;
	    *p -= (a * (*p - g)) / alpharadbias;
	    p++;
	    *p -= (a * (*p - r)) / alpharadbias;
	    k--;
	}
    }
}


/* Main Learning Loop
   ------------------ */
static void            learn()
{
    int    i, j, b, g, r;
    int    radius, rad, alpha, step, delta, samplepixels;
    unsigned char *p;
    unsigned char  *lim;

    alphadec = 30 + ((samplefac - 1) / bpp);
    p = thepicture;
    lim = thepicture + (lengthcount);
    samplepixels = lengthcount / (bpp * samplefac);
    delta = samplepixels / ncycles;
    alpha = initalpha;
    radius = initradius;

    rad = radius >> radiusbiasshift;
    if (rad <= 1)
	rad = 0;
    for (i = 0; i < rad; i++)
	radpower[i] = alpha * (((rad * rad - i * i) * radbias) / (rad * rad));

//      printf("beginning 1D learning: initial radius=%d\n", rad);

    if ((lengthcount % prime1) != 0)
	step = bpp * prime1;
    else {
	if ((lengthcount % prime2) != 0)
	    step = bpp * prime2;
	else {
	    if ((lengthcount % prime3) != 0)
		step = bpp * prime3;
	    else
		step = bpp * prime4;
	}
    }

    i = 0;
    while (i < samplepixels) {
	b = p[0] << netbiasshift;
	g = p[1] << netbiasshift;
	r = p[2] << netbiasshift;
	j = contest(b, g, r);

	altersingle(alpha, j, b, g, r);
	if (rad)
	    alterneigh(rad, j, b, g, r);	/* alter neighbours */

	p += step;
	if (p >= lim)
	    p -= (lengthcount);

	i++;
	if (i % delta == 0) {
	    alpha -= alpha / alphadec;
	    radius -= radius / radiusdec;
	    rad = radius >> radiusbiasshift;
	    if (rad <= 1)
		rad = 0;
	    for (j = 0; j < rad; j++)
		radpower[j] = alpha * (((rad * rad - j * j) * radbias) / (rad * rad));
	}
    }
//      printf("finished 1D learning: final alpha=%f !\n",((float)alpha)/initalpha);
}



static void            writemap(unsigned char *f)
{
    int             j, k = 0;

    // BGR
    for (j = 0; j < colorsreq; j++) {
	    f[k] = network[j][0];
	    k++;
	    f[k] = network[j][1];
	    k++;
	    f[k] = network[j][2];
	    k++;
	    f[k] = 255;
        k++;
	}
}

static void            readmap(unsigned char *f)
{
    int             j, k = 0;

    // BGR
    for (j = 0; j < colorsreq; j++) {
	    network[j][0] = f[k];
	    k++;
	    network[j][1] = f[k];
	    k++;
	    network[j][2] = f[k];
	    k++;
	    network[j][3] = j;
        k++;
	}
}

static void 		Quantize24Bit(unsigned char *pic_in,
	unsigned char *pic_out, unsigned char *pal_out,
	int length, int reduce_to, int sample_factor)
{
    int             i;

    bpp=3;
    netsize = reduce_to;
    initnet(pic_in, length, sample_factor);

    learn();
    unbiasnet();

    writemap(pal_out);
    inxbuild();

    for (i = 0; i < length; i += bpp) {
	    pic_out[i/4] = inxsearch(
	    pic_in[i],
	    pic_in[i + 1],
	    pic_in[i + 2]);
    }
}

static void 		Quantize32Bit(unsigned char *pic_in,
	unsigned char *pic_out, unsigned char *pal_out,
	int length, int reduce_to, int sample_factor)
{
    int             i;

    bpp=4;
    netsize = reduce_to;
    initnet(pic_in, length, sample_factor);

    learn();
    unbiasnet();

    writemap(pal_out);
    inxbuild();
    for (i = 0; i < length; i += bpp) {
	    pic_out[i/4] = inxsearch(
	    pic_in[i],
	    pic_in[i + 1],
	    pic_in[i + 2]);
    }
}

static void 		Reduce32Bit(unsigned char *pic_in,
	unsigned char *pic_out, unsigned char *palette,
	int length, int reduce_to)
{
    int             i;

    bpp=4;
    netsize = reduce_to;
    readmap(palette);
    inxbuild();
    for (i = 0; i < length; i += bpp) {
	    pic_out[i/4] = inxsearch(
	    pic_in[i],
	    pic_in[i + 1],
	    pic_in[i + 2]);
    }
}

F2FX int neuReduce(unsigned char *RGBpic, unsigned char *picout, unsigned char *palout, int numcolors, long picsize, int sfactor)
{
    colorsreq = numcolors;
    Quantize32Bit(RGBpic,
                  picout,
		  palout,
		  picsize*4,
		  numcolors,
		  sfactor
		  );
    return 0;
}

F2FX int paletteReduce(unsigned char *RGBpic, unsigned char *palette, unsigned char *picout, int numcolors, long picsize)
{
    colorsreq = numcolors;
    Reduce32Bit(RGBpic,
                  picout,
		  palette,
		  picsize*4,
		  numcolors
		  );
    return 0;
}

/* Program Skeleton
   ----------------
   [select samplefac in range 1..30]
   [read image from input file]
   pic_in = (unsigned char*) malloc(3*width*height);
   initnet(pic_in,3*width*height,samplefac);
   learn();
   unbiasnet();
   [write output image header, using writecolourmap(f)]
   inxbuild();
   write output image using inxsearch(b,g,r)
 */
/* Output:
        QuantizedPicture[picsize];
        QuantizedPalette[numcolors*3];
*/
