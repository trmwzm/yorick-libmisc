//"r_fw0"; "r_dAz"; "r_drg"; "r_tauB"; "r_rgB"; "r_rgE"; "r_fr0"; "r_kr0"; "i_srgfft"; "r_rgdf";
//"r_azdf"; "i_sazfft"; "i_firstvalrg"; "r_sBac"; "r_sEac"; "avg_yaw_pitch_roll"; "r_spatchcen";
//"mcp.r_href(i_chan,mcp.r_hrefdot(i_chan"; "mcp.r_cref(i_chan,mcp.r_crefdot(i_chan";
//"mcp.r_hrefcom,mcp.r_hrefcomdot"; "mcp.r_crefcom,mcp.r_crefcomdot"; "r_sStartAp,r_sEndAp";
//"r_schm2"; "r_vm2m0"; "r_vm2m1"; "tvp(i_tvpmodcen.r_possch"; "r_rhocentercom"; "r_ur2m2";
//"r_vr2m2"; "r_schref2"; "r_vr2m1"; "r_unr2"; "r_atmdelay,r_datmdelaydr";

require, "yut.i";
require, "sartools.i";
require, "uav_tvp.i";
//require, "uav_modes_inc.i";
require, "dsp.i";
require, "vec2d.i";
require, "uav_dat.i";  //rcuav
require, "fpanels.i";
require, "read_corners.i";
require, "lmfit.i";
require, "yconv.i";
require, "prstruct.i";
require, "time.i";

// plugins
require, "offt.i";
include, "osincint.i";

//////////////////////////////////////////////////////////////////////////

DEGRA= 180/pi;
RADEG= pi/180.;
SOL= 299792458.0;

/*------------------------------------------------------------------*/

struct trackS
{
  string datnm;
  string auxnm;
  string tvpnm;
  string awgnm;
  string demnm;
  string dehnm;
  string crnnm;
  string wrkdir;
  string rctmpnm;
  string tvptmpnm;
  string tmpldir;
  string rctmpl;
  string rcxnm;
  string mc2nm;
  string azcnm;
  string pdbnm;
  string phtrknm;
  string toponm;
  string procdatnm;
}

/*-----------------------------------------------------------------------*/

func rtisarroi (tvpnm,k0,dr,nrsp,&ds,&llhaprt,&r1,nazap=,llh=,sch=,crnnm=,tvpdcm=,\
                r1p=,ir1p=,lrl=,verbose=,pad=,procall=)
{
  if (is_void(tvpdcm)) tvpdcm= 50;  // decimation for geoloc
  if (is_void(nazap)) nazap= 1.0;   // number of azimuth apertures
  if (is_void(pad)) pad= 500.0;

  //pad=  2000.0;       // FIXXXX !

  nrp= nrsp(1);
  nsp= nrsp(2);

  ntrck= numberof(tvpnm);

  //file handel list
  local peg;
  otvp= save(dat=save(),peg=save());
  for (it=1;it<=ntrck;it++) {
    f= read_uav_tvp(tvpnm(it),peg);
    save,otvp(dat),string(0),f.tvp;
    save,otvp(peg),string(0),peg;
  }

  //get squint from TVP & swath
  tvp= otvp(dat,1);
  peg= otvp(peg,1);

  re= peg.rad;
  ds= tvp.posSch(1,1:2)(dif)(1);         // OUTPUT
  hp= tvp.posSch(3,::tvpdcm)(avg);
  ypr= tvp.attitude(,::tvpdcm)(,avg);
  esa= tvp.esa(1,::tvpdcm)(avg);
  ht= tvp.refh(avg);
  dwp= tvp.dwp(1);

  // starting range on dr spacing from tvp.dwp(1)
  if (is_void(ir1p))
    if (!is_void(r1p))
      ir1p= long((r1p-dwp)/dr+0.5)+1;
    else
      ir1p= 1;

  if (is_void(r1p)) r1p= dwp+(ir1p-1)*dr;
  r1= r1p;                                // OUTPUT

  // mean squint
  r0p= r1p+(nrp-1)*dr;
  sinp= lkatesarg(ypr,esa,hp,ht,span(r1p,r0p,nrp)(-,),re,lrl=lrl)(1,avg,avg);
  cosp= sqrt(1-sinp^2);

  // compressed range swath aperture length, add to ROI to get aprt.
  sap= saprt([[r1p*cosp,0],[r0p*cosp,0.]],k0,dr,ds/nazap,sinp,re,hp,ht);
  sapm= min(sap);
  sapM= max(sap);

  // ss is the max s0 compressed interval [min,max]
  ss= [tvp.posSch(1,1)-sapm,tvp.posSch(1,0)-sapM]+pad*[1,-1]; //s0 start end end & pad
  //focusable swath
  swsch= transpose([ss,c_rg(r1p+[0,(nrp-1)]*dr,re,hp,ht,lrl=lrl)(-,),ht],2);  // N pattern

  if (procall==1) {
    crsch= swsch(,*);
    crsch_in= crsch;
  } else {
    if (crnnm&&strlen(crnnm)) {
      if (verbose==1) write,"Corner file to locate patch: ",crnnm;
      crsch= readcrfile(crnnm,peg);
    } else if (!is_void(llh)) {
      crsch= llh_to_sch(llh,peg);
      if (verbose==1) write,"LLH to locate patch";
    } else if (!is_void(sch)) {
      crsch= sch;
      if (verbose==1) write,"SCH to locate patch";
    } else {
      if (verbose==1) write,"picking track center...";
      hp= tvp.posSch(3,::tvpdcm)(avg);
      crsch= [0,c_rg((ir1p?r1p:dwp)+(nrp-1)*dr,re,hp,ht,lrl=lrl),ht];
    }
    if (dimsof(crsch)(1)==(1)) crsch= [crsch];
    crsch_in= crsch;

    //provide sufficient roi/swath length for 1 patch;  (.. not good if variable patch size)
    if (abs(crsch(1,ptp))<nsp*ds) crsch= _(crsch,\
      transpose([avg(crsch(1,))+[-1,1]*nsp*ds/2,c_rg((ir1p?r1p:dwp)+nrp*dr/2,re,hp,ht,lrl=lrl),ht],2));

    //points in roi_point to roi_point segment
    ncr= numberof(crsch(1,));
    crsch0= []
    if(ncr>1) {
      csnr= vec2dintsct(crsch([1,2],:-1),crsch([1,2],2:),swsch([1,2],1),swsch([1,2],2),dnr);
      csfr= vec2dintsct(crsch([1,2],:-1),crsch([1,2],2:),swsch([1,2],4),swsch([1,2],3),dfr);
      csnr= (csnr+csfr)/2;
      wsw= where(abs(dnr)<1&abs(dfr)<1);
      if ((nw=numberof(wsw))) {
        crsch0= transpose([csnr(1,wsw),csnr(2,wsw),ht],2);
        if (verbose==1) write,"numberof ROI edges intersecting swath: ",nw;
      }
    }
    // points inside swath
    q= crsch; q(2,..)= 0;
    csnr= vec2dintsct(q([1,2],..),crsch([1,2],..),swsch([1,2],1),swsch([1,2],2),dnr,ddnr);  //near rg
    csfr= vec2dintsct(q([1,2],..),crsch([1,2],..),swsch([1,2],4),swsch([1,2],3),dfr,ddfr);  //far rg
    wsw= where( dnr>0 & dnr<1 & ddnr>0 &ddnr<1 & abs(dfr)>1);  //
    if ((nw=numberof(wsw))) {
      if (nw==1)
        crsch= _(crsch0,crsch(,wsw(1)));
      else
        crsch= _(crsch0,crsch(,wsw));
      if (verbose==1) write,"numberof ROI vertices inside swath: ",nw;
    } else {
      if (verbose==1) write,"WARNING: no ROI vertices inside swath: ";
    }
  }
  //points in roi_point to roi_point segment
  ncr= numberof(crsch(1,));
  llhout= sch_to_llh(crsch,peg);
  llhout(1:2,..)*DEGRA;

  // intersection with remaining swaths, crop llhout
  sapmm= sapm;
  sapMM= sapM;
  for (it=2;it<=ntrck;it++) {
    tvp= otvp.dat(noop(it));
    peg= otvp.peg(noop(it));
    re= peg.rad;
    ds= tvp.posSch(1,1:2)(dif)(1);
    hp= tvp.posSch(3,::tvpdcm)(avg);
    ypr= tvp.attitude(,::tvpdcm)(,avg);
    esa= tvp.esa(1,::tvpdcm)(avg);
    ht= tvp.refh(avg);
    dwp= tvp.dwp(1)

    r0p= r1p+(nrp-1)*dr;
    sinp= lkatesarg(ypr,esa,hp,ht,span(r1p,r0p,nrp)(-,),re,lrl=lrl)(1,avg,avg);
    cosp= sqrt(1-sinp^2);

    // compressed range swath aperture length
    sap= saprt([[r1p*cosp,0],[r0p*cosp,0.]],k0,dr,ds/nazap,sinp,re,hp,ht);
    sapm= min(sap);
    sapM= max(sap);
    sapmm= min(sapm,sapmm);
    sapMM= max(sapM,sapMM);

    // ss is the max s0 compressed interval [min,max]
    ss= [tvp.posSch(1,1)-sapm,tvp.posSch(1,0)-sapM]+pad*[1,-1]; //s0 start end end
    swsch= transpose([ss,c_rg(r1p+[0,(nrp-1)]*dr,re,hp,ht,lrl=lrl)(-,),ht],2);  // N pattern

    crsch= llh_to_sch(llhout,peg); // from ROI and track#1

    if (anyof(max(swsch(1,))<crsch(1,))&&verbose==1) write,"WARNING: cr beyond swath S";
    crsch(1,)= min(max(swsch(1,)),crsch(1,));   // cr beyond swath S

    if (anyof(min(swsch(1,))>crsch(1,))&&verbose==1) write,"WARNING: cr before swath S";
    crsch(1,)= max(min(swsch(1,)),crsch(1,));   // cr before swath S

    if (anyof(max(swsch(2,))<crsch(2,))&&verbose==1) write,"WARNING: cr eyond swath Rg";
    crsch(2,)= min(max(swsch(2,)),crsch(2,));   // cr eyond swath Rg

    if (anyof(min(swsch(2,))>crsch(2,))&&verbose==1) write,"WARNING: cr before swath Rg";
    crsch(2,)= max(min(swsch(2,)),crsch(2,));   // cr before swath Rg

    llhout= sch_to_llh(crsch,peg);
  }

  if (verbose==1) {
    write,"Valid proc area S length [m]: ",pr1([min(crsch(1,..)),max(crsch(1,..))]);
    write,"Conbined aperture upper/lower bounds [m]: ",pr1([sapmm,sapMM]);
    write,"Conbined aperture length [m]: ",pr1(sapMM-sapmm);
  }
  // compute the outmost data take borders
  smin= min(crsch(1,))+sapmm;
  smax= max(crsch(1,))+sapMM;
  llhaprt= sch_to_llh([[smin-pad,0,hp],[smax+pad,0,hp]],peg);   //!! last peg

  return llhout;
}

/*-----------------------------------------------------------------------*/

func rtisarpatch (llh,peg,dwp,azres,dr,ds,re,hp,hmoc,sinp,nrsp,&rs1p,&nrs,&nstot,&npatch,\
                  nazap=,tvpdcm=,r1p=,ir1p=,lrl=,verbose=,llhaprt=,rgpadfact=)
/* DOCUMENT
 */
{
  sch= llh_to_sch(llh,peg);

  nrp= nrsp(1);
  nsp= nrsp(2);

  if (is_void(ir1p))
    if (!is_void(r1p))
      ir1p= long((r1p-dwp)/dr+0.5)+1;
    else
      ir1p= 1;

  if (is_void(r1p)) r1p= dwp+(ir1p-1)*dr;
  r0p= r1p + (nrp-1)*dr;

  // now in the imaging domain (sinp)
  rscr= rstarloc(sch, sinp, re, hp);

  if (r1>min(rscr(1,))) write,"Warning: part of requested swath is  out of range";

  // compute all patch boundaries here
  s1p= min(rscr(2,*));
  nstot= max(nsp,long(abs(rscr(2,*)(ptp))/ds));  // NSP requested swath length
  npatch= max(1,long(nstot*1.0/nsp));  // chopping ? ... but cannot do nint
  nstot= npatch*nsp;

  s0p= s1p + (nrsp(2)-1)*ds;

  // required unfocused data domain computation
  rs0= rs0_rssq([[r1p,s1p],[r1p,s0p],[r0p,s1p],[r1p,s1p]],sinp);
  local r10,s10;
  nrs= getdatabnd(rs0,sinp,k0,azres,dr,ds,re,hp,hmoc,nazap,r10,s10,box=box,\
                             dntl=dm2,pow2=pow2,rgpadfact=rgpadfact);

  // snap to grid
  is1p= long((s1p-s10(1))/ds + 0.5)+1;
  s1= s1p-(is1p-1)*ds;   // such that s1p= s1+(is1p-1)*ds

  ir1= max(0,long((r10(1)-dwp)/dr+0.5)) + 1;
  r1= dwp+(ir1-1)*dr;

  rs1p= [r1p,s1p];  //output, with nrs

  // info stuff beyond ...
  if (!is_void(llhaprt)) {  //Check presum boundaries
    schaprt= llh_to_sch(llhaprt,peg);
    ss0= s10(2)+(nstot-nrsp(2))*ds;
    if (verbose==1) write,"TVP extent: ",pr1(schaprt(1,)),"required: ",pr1([s1,ss0]);
    if (min(schaprt(1,))>s1) error,"inconsistent s1";
    if (max(schaprt(1,))<ss0) error,"inconsistent s0";
  }

  if (verbose==1) {
    write,"uavprocound...";
    write,"re [m]: ",pr1(re);
    write,"dr [m]: ",pr1(dr);
    write,"ds [m]: ",pr1(ds);
    write,"hp [m]: ",pr1(hp);
    write,"ht [m]: ",pr1(hmoc);
    write,"ypr [deg]: ",pr1(ypr*180/pi);
    write,"esa [deg]: ",pr1(esa*180/pi);
    write,"squint [deg]: ",asin(sinp)*180/pi;
    write,"S ROI extent [m]/px: ",(x=abs(rscr(2,*)(ptp))),long(x/ds)+1;
    write,"Rg ROI extent [m]/px: ",(x=abs(rscr(1,*)(ptp))),long(x/dr)+1;
    write,"S proc start/end [m]: ",pr1([s1,s1+(nrs(2)-1)*ds]);
    write,"Rg proc DWP, start, end [m]: ",pr1([dwp, r1,r1+(nrs(1)-1)*dr]);
    write,"total number of presum recs: ",nstot;
    write,"number of patches: ",npatch;
  }

  return [r1,s1];

}

/*-----------------------------------------------------------------------*/

func rtisar (trck, llhroi=, schroi=, chan=, toposmooth=, noproc=, dodkx=, dodkr=, atm=, lrl=,\
             azdown=, docom=, mkpeg=, verbose=, ir1p=, ir0p=, nsp=, f0=,\
             azlant=, ellant=, radbw=, bw=, azfracbw=, kernl=, pedh=,\
             framefrac=, decim=, nazap=, azres=, tvpdc=, rgpadfact=, ovlm2=, dm2=, box=,\
             azhamw=, rghamw=, read_azc=, azcnm=, rtisarparf=,procall=)
/* DOCUMENT
 */
{
  if (is_void(trck))
      error,"provide data links (trackS structure in uavsar.i)";

  ntrck= numberof(trck);
  for (i=1;i<=ntrck;i++) {
    mkdirp, trck(i).wrkdir;
    check_file,trck(i).datnm,trck(i).auxnm,trck(i).tvpnm,trck(i).awgnm,trck(i).demnm,\
      trck(i).dehnm,trck(i).crnnm,trck(i).tmpldir+"/"+trck(i).rctmpl,trck(i).rcxnm;
    check_dir,trck(i).tmpldir,trck(i).wrkdir;
  }

  if (is_void(chan))     chan= 1;
  if (is_void(framefrac))framefrac= 0.0;
  if (is_void(toposmooth))toposmooth= 0;
  if (is_void(noproc))   noproc= 0;
  if (is_void(dodkx))    dodkx= 0;
  if (is_void(dodkr))    dodkr= 0;
  if (is_void(atm))      atm= 1;
  if (is_void(lrl))      lrl= 1;
  if (is_void(azdown))   azdown= 1;
  if (is_void(docom))    docom= 0;
  if (is_void(mkpeg))    mkpeg= 1;
  if (is_void(verbose))  verbose= 1;
  if (is_void(ir1p))     ir1p= 1001;          // the swath of slant range to process
  if (is_void(ir0p))     ir0p= 7000;
  if (is_void(nsp))      nsp= 4000;
  if (is_void(re))       re= 0.0;          // re==0 for cubic earth, otherwise local sphere reius (m)
  if (is_void(f0))       f0= 1.2575e9;       // Center frequency of radar (Hz)
  if (is_void(azlant))   azlant= 1.5;
  if (is_void(ellant))   ellant= 1.0;
  if (is_void(radbw))    radbw= 80.0e6;         // Radar bandwidth (Hz)
  if (is_void(bw))       bw= 90.0e6;
  if (is_void(azfracbw)) azfracbw= 0.9;
  if (is_void(azhamw))   azhamw= 1.0;         // 1.0 is no filtering (this needs to be in tune with azfracbw)
  if (is_void(rghamw))   rghamw= 1.0;         // 1.0 is no filtering (this needs to be in tune with azfracbw)
  if (is_void(kernl))    kernl= 8;
  if (is_void(pedh))     pedh= 0.001;
  if (is_void(decim))    decim= 10000;
  if (is_void(nazap))    nazap= 1.2;        // determines data extent
  if (is_void(azres))    azres= azlant*0.5;
  if (is_void(tvpdc))    tvpdc= 50;
  if (is_void(rgpadfact))rgpadfact= 0.4;
  if (is_void(ovlm2))    ovlm2= [20,2];
  if (is_void(dm2))      dm2= [128,128];
  if (is_void(box))      box= [32,32];

  rtisarpartxt= rtisarpardump(rtisarparf);

  lam= SOL/f0;
  k0= 4*pi/lam;
  dr= SOL/(2*bw);

  elfracbw= radbw/bw;     // range bandwidth fraction in proc (>1.0)

  nrp= ir0p-ir1p+1;
  nrsp= [nrp,nsp];

  usefftw= 1;
  fcplx= 1;

  if (numberof((w=where(strlen(trck.crnnm))))) {
    it= w(1);
    crnnm= trck(it).crnnm;
  } else {
    crnnm= [];
  }

  // focusing domain
  local ds;  // first def.
  local llhaprt;  // first def.: aperture boundaries for tvp
  local r1;  //first guess

  llhroi0= llhroi;
  llhroi= rtisarroi (trck.tvpnm,k0,dr,nrsp,ds,llhaprt,r1,nazap=nazap,llh=llhroi,sch=schroi,crnnm=crnnm,\
                ir1p=ir1p,lrl=lrl,verbose=verbose,pad=500.0,procall=procall);

  // global peg tvp
  local peg, pegco, sinp, hp, ht;
  compegtvp,trck.tvpnm,trck.tvptmpnm,pegco,llh=llhaprt;

  if (verbose==1) {
    write,format="ROI points (Lat, Lon, Height) [deg,deg,m] %8.3f %8.3f %8.3f\n",\
       llhroi(1,)*DEGRA,llhroi(2,)*DEGRA,llhroi(3,);
    sch= llh_to_sch(llhroi,pegco);
    write,format="ROI points (s, c, h) [m,] %8.3f %8.3f %8.3f\n",sch(1,),sch(2,),sch(3,);
    sch= llh_to_sch(llhaprt,pegco);
    write,format="Track bounds (s, c, h) [m,] %8.3f %8.3f %8.3f\n",sch(1,),sch(2,),sch(3,);
  }

  // accumulate stats on proc parameters from each tvp
  ds_t= hp_t= esa_t= sinp_t= hmoc_t= dwp_t= array(double,ntrck);
  ypr_t= array(double, [2,3,ntrck]);
  for (it=1;it<=ntrck;it++) {
    ftvp= read_uav_tvp(trck(it).tvptmpnm,peg);  //peg is now common peg pegco
    tvp= ftvp.tvp;   //read
    re= peg.rad;
    ds_t(it)= tvp.posSch(1,2)-tvp.posSch(1,1);  // should be generalized
    hp_t(it)= tvp.posSch(3,avg);
    hmoc_t(it)= tvp.refh(avg);
    dwp_t(it)= tvp.dwp(min);
    ypr_t(,it)= tvp.attitude(,avg);
    esa_t(it)= tvp.esa(1,avg);
    lkdc= 50;     // look comp decimation rg&az
    sinp_t(it)= lkatesarg(tvp.attitude(,::lkdc),tvp.esa(1,::lkdc),tvp.posSch(3,::lkdc),\
        tvp.refh(::lkdc),(r1+indgen(0:nrsp(1)/lkdc)*dr*lkdc)(-,),re,lrl=lrl)(1,avg,avg);
  }
  dwp=  dwp_t(max);  // if min(dwp) then r1 might be less than another dwp ... thus max
  ds=   ds_t(min);
  hp=   hp_t(avg);
  ypr=  ypr_t(,avg);
  esa=  esa_t(avg);
  sinp= sinp_t(avg);
  hmoc= hmoc_t(avg);

  // set up patching & first one
  local rs1p, nrs, nstot, npatch;
  rs1= rtisarpatch (llhroi,pegco,azres,dwp,dr,ds,re,hp,hmoc,sinp,nrsp,rs1p,nrs,nstot,npatch,\
         ir1p=ir1p,lrl=lrl,nazap=nazap,verbose=verbose,llhaprt=llhaprt,rgpadfact=rgpadfact);

  // snap to grid to enable chopping
  irs1p= long((rs1p-rs1)/[dr,ds]+0.5)+1;
  rs1p= rs1+(irs1p-1)*[dr,ds];

  write,"SAR image size: ",pr1(nrs);
  write,"SAR comp size: ",pr1(nrsp);
  write,"SAR first post rs : ",pr1(rs1);
  write,"SAR comp first post rs : ",pr1(rs1p);
  nr= nrs(1);
  ns= nrs(2);
  r1= rs1(1);
  s1= rs1(2);
  r1p= rs1p(1);
  s1p= rs1p(2);
  rs0= rs1+(nrs-1)*[dr,ds];
  rs0p= rs1p+(nrsp-1)*[dr,ds];
  r0= rs0(1);
  s0= rs0(2);
  r0p= rs0p(1);
  s0p= rs0p(2);

  irs10= long([rs1p-rs1,rs0p-rs1]/[dr,ds]+.5)+1;
  if (anyof(nrsp!=irs10(,dif)+1)) error,"incompatible dims."

  // -- FFTW init --
  dd= fcplx!=1? [2,nr,ns]:[3,2,nr,ns];
  write,"FFTW init size: ",pr1(dd);
  if (!noproc)
    fftop= save(anl=offt(usefftw=usefftw,inplace=1,ljdir= 1,dims=dd,fcplx=fcplx),\
                syn=offt(usefftw=usefftw,inplace=1,ljdir=-1,dims=dd,fcplx=fcplx));

  // -- get topo --
  rsbnd= rs0_rssq([[r1p,s1p],[r1p,s1p+(nstot-1)*ds],[r0p,s1p],[r0p,s1p+(nstot-1)*ds]],sinp);
  if (strlen(trck(1).demnm)&&strlen(trck(1).dehnm)) {
    topo= getuavdem(trck(1).demnm,trck(1).dehnm,rsbnd,hp,hmoc,pegco,\
        pad=2*dm2*[dr,ds],schinterp=1);
    hmoc= avg(topo(3,..));
    topo(3,..)= hmoc+gaussm(topo(3,..)-hmoc, toposmooth);
  } else {
    topo= [];
  }

  // -- sinc interp init --
  osinc= save(rg=osincint(decim,kernl,elfracbw,pedh),\
              az=osincint(decim,kernl,0.98,0.5)); // rg: mocomp1, pulse simulation, az: mocomp and presum

  // ... processing ...
  for (ip=1;ip<=npatch;ip++) {

    s1= rs1(2)+(ip-1)*nsp*ds;
    s1p= rs1p(2)+(ip-1)*nsp*ds;
    s0= rs0(2)+(ip-1)*nsp*ds;
    s0p= rs0p(2)+(ip-1)*nsp*ds;

    for (it=1;it<=ntrck;it++) {
      ftvp= read_uav_tvp(trck(it).tvptmpnm,peg);  //peg is common peg pegco

      re= peg.rad;
      dwp= min(ftvp.tvp.dwp);

      faux = read_uav_aux(trck(it).auxnm);
      auxframe1= faux.aux.frame(1);

      ntvp= numberof(ftvp.tvp);
      tvprec= min(ntvp,max(1,digitize([s1,s0],ftvp.tvp.posSch(1,))-1+2*kernl*[-1,1]));
      if1= long(ftvp.tvp(tvprec(1)).frame)-auxframe1+1+kernl;  // +buffer for tvp interpolation @ frame
      if0= long(ftvp.tvp(tvprec(2)).frame)-auxframe1+1-kernl;

      ptvp= getuavtvp(ftvp.tvp,peg,tvprec(1),tvprec(2));

      dssol= ftvp.tvp.velSch(1,avg)*(r1p+r0p)/(2*SOL);
      lkdc= 50;     // look comp decimation rg&az

      antxyz= interp(ptvp.antSch(,chan,..),ptvp.frame-auxframe1+1-framefrac,indgen(if1:if0),2);   //antenna pos @ raw rec
      antxyz(1,..)+= dssol;
      //
      ks0= ks0p= k0*sinp;
      if (crnnm) {
        crsch= readcrfile(crnnm,peg);
        crrg= rz_c(crsch(2,),re,hp,crsch(3,));
        crrssq= rssq_rs0(transpose([crrg,crsch(1,*)]),sinp);
        if (numberof((w=where(crrssq(1,)<r1p|crrssq(1,)>r0p))))
          write,"Warning: corners "+pr1(w)+" out of range";
        if (numberof((w=where(crrssq(2,)<s1p|crrssq(2,)>s0p))))
          write,"Warning: corners "+pr1(w)+" out of azimuth";
      } else {
        crsch= []; // prevent tracking
      }
      moffset= [0.,antxyz(2,avg),antxyz(3,avg)-hp];  // by construct

      ir1= long((r1-dwp)/dr+1.5);  //track DWP
      ir0= long((r0-dwp)/dr+1.5);

      /*--------------------------------------------------------------*/
      if (verbose==1) {
        write,"SAR data\nProcessing track# ",pr1(it),"patch# :",pr1(ip);
        write,"motion stats are:  [Max, Min, PTP, AVG, RMS]";
        write,"ant S = ",pr1(statarr(antxyz(1,)));
        write,"ant C = ",pr1(statarr(antxyz(2,)));
        write,"ant dH= ",pr1(statarr(antxyz(3,)-hp));
        write,"Mocomp1 Ref Height: ",hmoc;
        write,"RC Starting Ranges bin & before/after azcomp: ",pr1(ir1), pr1(dwp+(ir1-1)*dr),pr1(r1);
      }

      if (!noproc) {
        if (!read_azc) {
          if (!is_void(rgpadfact))
            ir0rc= max(ir1+nrp,long(ir1+(ir0-ir1)/(1+rgpadfact))); // KLUDGE
          else
            ir0rc= ir0;
          sar= rcuav(trck(it).datnm,trck(it).auxnm,1,ir0rc,if1,if0,trck(it).awgnm,\
              chan=chan,track=1,\
              rcnm=trck(it).wrkdir+"/"+trck(it).rctmpnm,\
              rctmpl=trck(it).tmpldir+"/"+trck(it).rctmpl,rm=1);

          if (ir1>1) sar= sar(,ir1:,..)/2^14;
          if (ir0>ir0rc) {
            sarrc= sar;
            sar= array(0.0f,[3,2,ir0-ir1+1,if0-if1+1]);
            sar(,:ir0rc-ir1+1,)= sarrc;
          }
        }

        if (verbose==1) write,"RC done";  //_write,open("rc.dat","wb"),0,sar;

        sar= sarproc (sar,irs10,antxyz,dr,ds,dwp+(ir1-1)*dr,s1,k0,sinp,ns,hmoc,re,ypr,esa,osinc(rg),osinc(az),r1p,s1p,\
            tldat, r1out=r1, hp=hp, moffset=moffset, topo=topo, atm=atm, lrl=lrl, azdown=azdown, fcplx=fcplx, rghamw=rghamw,\
            crsch=crsch, peg=peg, docom=docom, phstrack=trck(it).phtrknm, fftop=fftop, azreswf=[azres,azhamw],\
            dbgmc2=dbgmc2, dodkr=dodkr, dodkx=dodkx, dm2=dm2, ovlm2=ovlm2, read_azc=read_azc, azcnm=trck(it).azcnm,\
            verbose=verbose);

        if (strlen(trck(it).mc2nm)>0) writeFlat,sar,trck(it).mc2nm,append=ip>1;

        if (strlen(trck(it).procdatnm)>0) {
          writeFlat,(x=tldat.rs0),trck(it).procdatnm+"_"+typeof(x)+("X"+totxt(dimsof(x)))(2:)(sum)+".rs0",append=ip>1;
          writeFlat,(x=tldat.ireg),trck(it).procdatnm+"_"+typeof(x)+("X"+totxt(dimsof(x)))(2:)(sum)+".ireg",append=ip>1;
          writeFlat,(x=tldat.h),trck(it).procdatnm+"_"+typeof(x)+("X"+totxt(dimsof(x)))(2:)(sum)+".tlh",append=ip>1;
          writeFlat,(x=tldat.bl),trck(it).procdatnm+"_"+typeof(x)+("X"+totxt(dimsof(x)))(2:)(sum)+".bsl",append=ip>1;
        }

        if (ip==1) {
          save,(f=createb(trck(it).pdbnm)),trackS,trck,it,rs1,rs1p,moffset,antxyz;
          save,f,dr,ds,dwp,s1,k0,sinp,nrs,nrsp,hmoc,re,hp,ypr,esa,peg;
          save,f,rtisarpartxt;
          if (!is_void(topo)) save,f,topo;
        }

        if (crnnm) {
          crr= crnanl (sar, crsch, r1p, s1p, dr, ds, k0, sinp, re, hp, hmoc, peg,\
              azdown=azdown, verbose=verbose,box=box,prox=prox,osf=osf,islrwidth=4*[dr,ds],\
              advance=advance,color=color,legend=legend,landscape=landscape,\
              hcroot=hcroot,winpic=winpic,winanl=winanl,asclog=asclog,fcplx=fcplx);
          if (ip==1) save,f,crr,crsch,crrssq;
        }
        close,f;
      }
    }
  }

  if (!noproc) {
    fftop, anl, reset=1;
    fftop, syn, reset=1;
  }

}

/*-----------------------------------------------------------------------*/

func rtisarpardump(rtisarparf)
{
  if (rtisarparf&&strlen(rtisarparf))
    f= open(rtisarparf,"w");
  else
    return;

  prettyprint,"trck",trck,fd=f;
  write,f,"chan= ",       pr1(chan),";";
  write,f,"framefrac= ",  pr1(framefrac),";";
  write,f,"toposmooth= ", pr1(toposmooth),";";
  write,f,"noproc= ",     pr1(noproc),";";
  write,f,"dodkx= ",      pr1(dodkx),";";
  write,f,"dodkr= ",      pr1(dodkr),";";
  write,f,"read_azc= ",   pr1(read_azc),";";
  write,f,"atm= ",        pr1(atm),";";
  write,f,"lrl= ",        pr1(lrl),";";
  write,f,"azdown= ",     pr1(azdown),";";
  write,f,"docom= ",      pr1(docom),";";
  write,f,"mkpeg= ",      pr1(mkpeg),";";
  write,f,"verbose= ",    pr1(verbose),";";
  write,f,"ir1p= ",       pr1(ir1p),";";
  write,f,"ir0p= ",       pr1(ir0p),";";
  write,f,"nsp= ",        pr1(nsp),";";
  write,f,"f0= ",         pr1(f0),";";
  write,f,"azlant= ",     pr1(azlant),";";
  write,f,"ellant= ",     pr1(ellant),";";
  write,f,"radbw= ",      pr1(radbw),";";
  write,f,"bw= ",         pr1(bw),";";
  write,f,"azfracbw= ",   pr1(azfracbw),";";
  write,f,"azhamw= ",     pr1(azhamw),";";
  write,f,"rghamw= ",     pr1(rghamw),";";
  write,f,"kernl= ",      pr1(kernl),";";
  write,f,"pedh= ",       pr1(pedh),";";
  write,f,"decim= ",      pr1(decim),";";
  write,f,"nazap= ",      pr1(nazap),";";
  write,f,"azres= ",      pr1(azres),";";
  write,f,"tvpdc= ",      pr1(tvpdc),";";
  write,f,"rgpadfact= ",  pr1(rgpadfact),";";
  write,f,"ovlm2= ",      pr1(ovlm2),";";
  write,f,"dm2= ",        pr1(dm2),";";
  write,f,"box= ",        pr1(box),";";
  if (!is_void(schroi)) write,f,"schroi= ",pr1(schroi),";";
  if (!is_void(llhroi)) write,f,"llhroi= ",pr1(llhroi),";";
  close,f;
  write,"";
  write,"UAVRTI input dump";
  write,"-----------------";
  write,rdfile(rtisarparf),format="%s\n";
  write,"";
  write,"UAVRTI start";
  write,"------------";

  return rdfile(rtisarparf);
}

func rtisarresdump (res,f,dna=,dir=)
{
  if (is_string(res))
    fb= openb(res);
  else if (is_stream(res))
    fb= res;
  else
    error,"expecting string or stream.";

  gv= *(get_vars(fb)(1));
  if (anyof(strmatch(gv,"topo")))
    restore,fb, topo;
  else
    topo= [];

  if (is_string(f))
    f= open(f,"w");

  if (is_void(dir)) dir= "";
  na= sizeof(open(dir+fb.trck(1).mc2nm,"rb"))/(8*fb.nrsp(1));

  if (is_void(dna)) dna= 0;
  write,f,"MC2 dimensions (r,a): (1st dim: rec length) ",pr1([fb.nrsp(1),na]);
  write,f,"MC2 post spacings (r,a) [m]: ",pr1([fb.dr,fb.ds]);
  write,f,"MC2",prpeg(fb.peg);
  write,f,"MC2 ref track height (hp) [m]: ",pr1(fb.hp);
  write,f,"MC2 sinus(squint) (sinp): ",pr1(fb.sinp);
  write,f,"MC2 first post coordinates (rs1p) (r,a) [m]: ",pr1(fb.rs1p);
  write,f,"MC2 common track lever (moffset) (s,c,h) [m]: ",pr1(fb.moffset);
  write,f,"MC2 carrier wavelength (4*pi/k0) [m]: ",pr1(4*pi/fb.k0);
  write,f,"MC2 average terrain height (hmoc) [m]: ",pr1(fb.hmoc);

  if (!is_void(topo))
    toporg= rz_c(fb.topo(2,..),fb.re,fb.hp,fb.topo(3,..));
  rar= fb.rs1p(1)+[0,fb.nrsp(1)-1]*fb.dr;
  sar= fb.rs1p(2)+[0-dna,na+dna-1](-,)*fb.ds;
  rs0= rs0_rssq(transpose([rar,sar],2),fb.sinp);
  if (!is_void(topo))
    ht= interp2(rs0(1,..),rs0(2,..),fb.topo(3,..),toporg,fb.topo(1,..),outside=fb.hmoc);
    //ht= interp2(rs0(2,..),rs0(1,..),fb.topo(3,..),fb.topo(1,..),toporg,outside=fb.hmoc);
  else
    ht= hmoc;
  sch= tgt_refrg(rs0(1,..),transpose([rs0(2,..),0,fb.hp],2),fb.re,ht); // lrl=lrl !!! FIX ME
  ll= sch_to_llh(sch,fb.peg);
  write,f,"MC2 corner SAR coordinates (s, c, h) [m]: ",sch(1,),sch(2,),sch(3,);
  write,f,"MC2 corner geo coordinates (Lat, Lon, Height) [deg,deg,m]: ",ll(1,)*DEGRA,ll(2,)*DEGRA,ll(3,);

  ftvp= read_uav_tvp (dir+fb.trck(1).tvptmpnm);
  dname= string(&ftvp.h1.data_name);
  ddate= string(&ftvp.h1.data_date);
  dtime= 0.0;
  if(!sread(string(&ftvp.h1.data_time),dtime)) write,"WARNING: header 1 asc IO error: data time";
  local year, month, day, hour, minute, secnds, frac;
  seccal, dtime, year, month,day,hour,minute,secnds,frac;
  write,f,"Data Take Name: "+dname;
  write,f,"Data Take Date: "+ddate;
  write,f,dtime,format=" Data Take GPS Time [s]: %12.2f\n";
  write,f,year,month,day,hour,minute,secnds,frac,\
       format="Data Take GPS Time (y-m-d h:m:s sdec):  %ld-%ld-%ld %ld:%ld:%ld %g\n";

  write,f,"Related data files: "
  prettyprint(,fb.trck,fd=f);

  write,f,"Y_VERSION: "+Y_VERSION;
  write,f,"Y_HOME: "+Y_HOME;
  write,f,"get_path: "+get_path();

  return fb;
}
