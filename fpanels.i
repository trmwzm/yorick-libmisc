
require, "style.i";
require, "yut.i";   // pltit & strplt

// pldefault, style="boxed.gs", dpi=75, color="fg" , height=500, width=500;
// pldefault, type="solid", width=1.0
// pldefault, marks=0, mcolor="fg", msize=1.0
// pldefault, mspace=0.16, mphase=0.14
// pldefault, rays=0, arrowl=1.0, arroww=1.0
// pldefault, rspace=0.13, rphase=0.11375
// pldefault, font="helvetica", height=12.0
// pldefault, justify="NN", opaque=0
// pldefault, hollow= 0, aspect=0.125
// pldefault, edges=0, ecolor="fg", ewidth=1.0

/*---------------------------------------------------------------------*/

func fpanels(nx,ny,file=,pwidth=,pheight=,
                   lmargin=,rmargin=,bmargin=,tmargin=,
                   xgap=,ygap=,landscape=,order=,
                   ptitle=,psubtitle=,toffset=,
                   charcolor=,charsize=,charthick=,charfont=,advance=)
/* DOCUMENT fpanels,nx,ny,file=,pwidth=,pheight=,
 *                        lmargin=,rmargin=,bmargin=,tmargin=,
 *                        xgap=,ygap=,landscape=0/1,order=,
 *                        ptitle=,psubtitle=,toffset=,
 *                        charcolor=,charsize=,chrathick=,charfont=,advance=0/1
 * Sets up multiple (NX*NY) panels on a page, with page width PWIDTH
 * (default 8.5in) page height PHEIGHT (default 11.0in), left/right/bottom/top
 * margins LMARGIN/RMARGIN/BMARGIN/TMARGIN (default 1.0in), and X/YGAP
 * between the panels (default 0.0in).
 *
 * If FILE is set to a filename, a new plot window is created, and
 * PostScript file of that name is used to dump hard copy, by calling HCPON.
 *
 * Calling FPANELS with no arguments turns off multiple panels and closes
 * any associated PostScript file.
 *
 * The panels are numbered 1,...,NX*NY as follows:
 * For ORDER > 0, X ordering varies fastest.
 * ORDER=1 => Left->right, top->bottom (default)
 * ORDER=2 => Left->right, bottom->top
 * ORDER=3 => Right->left, top->bottom
 * ORDER=4 => Right->left, bottom->top
 * For ORDER < 0, the X/Y ordering is interchanged, with Y varying fastest.
 * E.g.,
 * ORDER=-1 => Top->bottom, left->right, and so on.
 *
 * (Function FNEXT may be used to select a panel and/or advance to a new page.)
 *
 * LANDSCAPE option is for landscape orientation.
 *
 * PTITLE/PSUBTITLE specify title/subtitle for the whole page, using characters
 * of color CHARCOLOR, size CHARSIZE (default 1.0), thickness CHARTHICK
 * (default 1.0), and font CHARFONT (default "helvetica").
 *
 * TOFFSET=[title_offset, subtitle_offset] controls title positioning.
 *
 * Specifying ADVANCE==1 creates a new page for the next plot.
 * SEE ALSO: fnext, plsys
 */
{
  extern FPANELS_NX, FPANELS_NY, FPANELS_CUR;
  local landscape1, systems1, legends1, clegends1, winNum, type;

// window number
  if(is_void(winNum))winNum=window();

  if (!param_set(nx) || !param_set(ny)) {
//  Turn off multiple panels
    FPANELS_NX= 0;
    FPANELS_NY= 0;
    FPANELS_CUR= 0;
    fma;
    hcpoff;
    return;
  }

  FPANELS_NX= nx;
  FPANELS_NY= ny;
  FPANELS_CUR= 0;

// Page width/height (inches)
  if (is_void(pwidth))  pwidth= 8.5;
  if (is_void(pheight)) pheight= 11.0;

// Left, right, bottom, top margins (inches)
  if (is_void(lmargin)) lmargin= 1.0;
  if (is_void(rmargin)) rmargin= 1.0;
  if (is_void(bmargin)) bmargin= 1.0;
  if (is_void(tmargin)) tmargin= 1.0;

// X/Y gaps
  if (is_void(xgap)) xgap= 0.0;
  if (is_void(ygap)) ygap= 0.0;

  if (is_void(charsize))  charsize= 1.0;
  if (is_void(charthick)) charthick= 1.0;
  if (is_void(charfont))  charfont= "helvetica";

  if (is_void(toffset))  toffset= [0., 0.];

  order1= 1;
  if (param_set(order)) order1= order;

  in2ndc = 72.27 * .0013;  // convert from inches to NDC coordinate

// If hard copy is requested, explicitly create window of right dimensions
  if  (!is_void(file)) {

    if (strlen(get_env("DISPLAY")) == 0) {
      display= "";

    } else {
      fma;
      winkill, winNum;
      display= [];
    }

    if (param_set(landscape)) {
      //window,winNum,width=550,height=425,legends=0,hcp=file,dump=1,display=display;
      window,winNum,width=825,height=638,legends=0,hcp=file,dump=1,display=display;
    } else {
      //window,winNum,width=425,height=550,legends=0,hcp=file,dump=1,display=display;
      window,winNum,width=638,height=825,legends=0,hcp=file,dump=1,display=display;
    }
    hcpon;
  }

// Read style file
  read_style, "work2.gs", landscape1, systems1, legends1, clegends1;

// Landscape mode
  landscape1= int(param_set(landscape));

// Set up default style before copying and changing viewports
  legends1.nlines = 0;               // turn off legends ;
  clegends1.nlines = 0;              // turn off legends ;

// Inner dimensions of plot
  if (landscape1) {
     l= lmargin;
     t= tmargin;
     b= bmargin;
     r= rmargin;
     lmargin=b;
     tmargin=l;
     bmargin=r;
     rmargin=t;
//   Landscape orientation
     xinner= pheight - (bmargin+tmargin);
     yinner= pwidth - (lmargin+rmargin);
     xoffset= bmargin;
     yoffset= lmargin;

  } else {
//   Portrait orientation
     xinner= pwidth - (lmargin+rmargin);
     yinner= pheight - (bmargin+tmargin);
     xoffset= lmargin;
     yoffset= bmargin;
  }

// Panel spread
  xspread= (xinner+0.5*xgap) / nx;
  yspread= (yinner+0.5*ygap) / ny;

// Define viewports
  systems1= systems1(-:1:nx*ny);

  xfirst=     (order1 > 0);
  xrightward= (abs(order1) <= 2);
  yupward=    ((abs(order1) % 2) == 0);

  for (iy=1; iy <= ny; iy++) {
    for (ix=1; ix <= nx; ix++) {
       x0= xoffset + (ix-1)*xspread;
       x1= x0 + xspread - 0.5*xgap;
       y0= yoffset + (iy-1)*yspread;
       y1= y0 + yspread - 0.5*ygap;

       ix1= (2*xrightward-1)*(ix-1 - (1-xrightward)*(nx-1));
       iy1=    (2*yupward-1)*(iy-1 - (1-yupward)*(ny-1));

       ipanel= 1 + (1+xfirst*(nx-1))*iy1 + (1+(1-xfirst)*(ny-1))*ix1;
       systems1(ipanel).viewport= in2ndc*[x0, x1, y0, y1];
    }
  }

// Set style
  set_style, landscape1, systems1, legends1, clegends1;

// Create new page, if requested;
  if (param_set(advance)) fma;

// Display page titles
  plsys, 0;
  port= viewport();

  if (!is_null(ptitle) && (ptitle != ""))
    plt, ptitle, 0.4, 0.99+toffset(1),   //port(zcen:1:2)(1), port(4)+0.02+toffset(1),
         color=charcolor, justify="CB", height=charsize*18.0, font=charfont+"B";

  if (!is_null(psubtitle) && (psubtitle != ""))
    plt, psubtitle, 0.4, 0.05+toffset(2),  // port(zcen:1:2)(1), port(3)-0.050-toffset(2),
         color=charcolor, justify="CT", height=charsize*12.0, font=charfont;

  return;
}

/*---------------------------------------------------------------------*/

func fnext(ipanel,advance=)
/* DOCUMENT fnext, ipanel, advance=0/1
 * Move to plot panel IPANEL among panels defined by a call to FPANELS.
 * If none were defined, then simply move to next plot frame.
 * (If IPANEL > number_of_panels, then 1+(IPANEL-1) % number_panels is used.)
 * If IPANEL is omitted, the next panel is picked cyclically, and if the
 * previous panel was the last panel on the page, then a new page is created.
 * Specifying ADVANCE=1 advances to next page before moving to panel IPANEL,
 * and if IPANEL is not specified, moves to panel 1 on next page.
 * SEE ALSO: fpanels, plsys, flexp
 */
{
  extern FPANELS_NX, FPANELS_NY, FPANELS_CUR;

  if (!param_set(FPANELS_NX)) {
//  Panels not defined; advance frame and return
    fma;
    return;
  }

  if (!is_void(ipanel)) {
    FPANELS_CUR= (ipanel-1) % (FPANELS_NX*FPANELS_NY);
    if (param_set(advance)) fma;
    plsys, FPANELS_CUR+1;

  } else {
    if (param_set(advance)) {
      advflag= 1;
      FPANELS_CUR= 1;
    } else {
      advflag= (FPANELS_CUR == FPANELS_NX*FPANELS_NY);
      FPANELS_CUR= 1 + FPANELS_CUR % (FPANELS_NX*FPANELS_NY);
    }

    if (advflag) fma;
    plsys, FPANELS_CUR;
  }

  return;
}

/*---------------------------------------------------------------------------*/

func param_set(var)
/* DOCUMENT param_set(var)
   is true if VAR is not NULL, is not the scalar number zero,
   is not the null string, and is not a range.
   SEE ALSO: is_null, is_number, is_scalar
 */
{
 vartype= typeof(var);

 if ((vartype == "void") || (vartype == "range")) return 0;

 if (is_array(var) && !dimsof(var)(1)) {
//  Scalar
    if (vartype == "pointer") return (var != pointer(0) );

    if (vartype == "string") return (var != "");

    if (vartype == "struct_instance") return 1;

    return (var != 0);
  }
 return 1;
}


