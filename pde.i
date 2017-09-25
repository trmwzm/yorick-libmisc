// A collection of Yorick routines for integrating various PDEs.
// All programs use finite-difference techniques.
// Intended for in-class demonstrations, etc.
// Used for Math Physics II, Jan - Mar 2000.
// D. Holmgren,
// Dept. of Physics & Astronomy,
// Brandon University,
// Brandon, Manitoba, Canada
// R7A 6A9
// holmgren@brandonu.ca
// ----------------------------------------------------------------

// typical usage:
// window,0
// wave, 20, 500

require, "movie.i";
pldefault, marks=0;

// ----------------------------------------------------------------
// Finite difference solution for 4th order beam problem.
// This technique from Ames' book.
// This simulates a beam fixed at both ends:
//  u_tt + u_xxxx = 0
//  u(0) = 0 = u(1) and u_x(0) = 0 = u_x(1)
//  u_t = 0 at t = 0 and u(x,0) = sin(pi*x)-pi*x*(1-x)
// DH 13.2.00  Appears to work correctly.
func beam2(nx,nt)
{
// Keep time step much less than stability limit:
	dx = 1./nx;
	dt = 0.25*dx^2
	p = array(0.,nx);
        x = span(0.,1.,nx);
	q = p;
// Initial displacement, fixed at both ends:
	p = sin(pi*x) - pi*x*(1.-x);
	p1 = p;
	c = (dt/dx^2)^2;
// Enforce BCs; displacement and first derivatives at ends are zero.
        p(1:2)= p(-1:0)= 0.;
        limits, min(x), max(x), -1.1*max(abs(p)), 1.1*max(abs(p))
	movie, beam2_step, time_limit, min_interframe;
        beam2_step, nt;
}

func beam2_step(j)
{
  q(3:-2)= 2.*p(3:-2) - p1(3:-2) - c*p(dif)(dif)(dif)(dif);
  // Must keep present and past two time levels:
  p1 = p;
  p = q;

  plg, p, x;
  return (j < nt);
}

// ---------------------------------------------------------------
// Simple FD transmission line program.
// This program solves:
// u_tt - lambda^2 * u_xx + 2*c*u_t = 0
// with fixed ends, etc.
// DH 23.2.00
func tl(nx,nt)
{
 x = span(0.,1.,nx); dx = x(2)-x(1);
 dt = dx; c = 1.0; lambda = 1.;
 m = (dt/dx)^2; a = 1./(1.+dt*c);
 u = array(0.,dimsof(x)); v = u; w = u;
// f = (x-0.5)*exp(-100.*(x-0.5)^2);
 f = u; f(3:10)=1.;
 g = u;
 u = f;
 bc_left = 0.; bc_right = 0.;
  u(1)= bc_left;  u(0)= bc_right;
  limits, min(x), max(x), -1.5*max(abs(u)), 4.*max(abs(u))
 movie, tl_step, time_limit, min_interframe;
 tl_step, nt;
}

func tl_step(n)
{
  v(2:-1)= a*(2.*u(2:-1) - w(2:-1) + m*u(dif)(dif) +
              dt*c*w(2:-1));
  w= u;
  u= v;

  plg, u, x;
  return (n < nt);
}

// ----------------------------------------------------------------
// Simple FD transmission line program.
// This version solves:
// u_tt - lambda^2*u_xx + 2*c*u_t + u = 0
// with fixed ends, etc.
// Use large nx and nt to see TL effects.
// DH 23.2.00
func tl2(nx,nt)
{
 x = span(0.,1.,nx); dx = x(2)-x(1);
 dt = dx; c = 1.0; lambda = 1.;
 m = (dt/dx)^2; a = 1./(1.+dt*c);
 u = array(0.,dimsof(x)); v = u; w = u;
// f = (x-0.5)*exp(-100.*(x-0.5)^2);
 f = u; f(3:15)=1.;
 g = u;
 u = f;
 bc_left = 0.; bc_right = 0.;
  u(1)= bc_left;  u(0)= bc_right;
  limits, min(x), max(x), -3.*max(abs(u)), 7.*max(abs(u))
 movie, tl2_step, time_limit, min_interframe;
 tl2_step, nt;
}

func tl2_step(n)
{
  v(2:-1)= a*(2.*u(2:-1) - w(2:-1) + m*u(dif)(dif) +
              dt*c*w(2:-1) + dt^2*u(2:-1));
  w= u;
  u= v;

  plg, u, x;
  return (n < nt);
}

// ------------------------------------------------------------------
// Simple solution of a nonlinear conservation equation. DH 26.2.00
//  u_t + 3*u*u_x = 0
//  u(x,0) = 1 ( x < 0 )
//           1-x ( 0 < x < 1 )
//           0 ( x > 1)
// This uses a leapfrog method which I think is stable
// for (dt/dx)*3u < 1.
// Use traffic(100,175) to see a shock wave develop.
// Before running program, do:
// 1. window,0
// 2. limits,"e","e",0.,1.05
// This sets up Yorick for animation.
func traffic(nx,nt)
{
	x=span(-2.,2.,nx);
	dx = x(nx)-x(nx-1);
	dt = 0.05*dx;
	u=array(0.,nx);
	v = u;
	u = 1.-x;
	i1 = where(u<0);
	u(i1)=0.;
	i2 = where(u>1.);
	u(i2)=1.;
	w = u;
        v(1)=1.; v(nx)=0.;
        limits, min(x), max(x), -0.1*max(abs(u)), 5.*max(abs(u))
	movie, traffic_step, time_limit, min_interframe;
        traffic_step, nt;
}

func traffic_step(k)
{
  v(2:-1)= w(2:-1) - (dt/dx)*6.*u(2:-1)*u(dif)(zcen);
  w= u;
  u= v;

  plg, u, x;
  return (k < nt);
}

// ------------------------------------------------------------------
// Simple FD routine to solve the 1D wave equation
// u_tt = u_xx
// with reflecting BC's.
// DH 23.2.00
func wave(nx,nt)
{
// To get standing waves, try this domain:
//      x = span(0.,1.,nx);
// This larger domain is nice for watching propagation:
	x = span(-10.,10.,nx);
	dx = x(2)-x(1); dt = dx;
	m = (dt/dx)^2;
	u = array(0.,dimsof(x)); v = u; w = u;
// An initial Gaussian distribution:
        f = exp(-50.*x^2);
// This initial distribution is for standing waves:
//      f = sin(pi*x);
// Allow both position u and velocity w to be Gaussian:
	u = f; w = f;
// Enforce BCs:
	 v(1) = 0.; v(nx) = 0.;
        limits, min(x), max(x), -1.2*max(abs(u)), 1.2*max(abs(u))
         movie, wave_step, time_limit, min_interframe;
         wave_step, nt;
}

func wave_step(j)
{
  v(2:-1)= 2.*u(2:-1) - w(2:-1) + m*u(dif)(dif);
  w= u;
  u= v;

  plg, u, x;
  return (j < nt);
}

// ----------------------------------------------------------------
// Simple FD routine to solve the 3D wave equation
// u_tt = u_xx + u_yy + u_zz   (x,y,z) in R^3
// as an initial value problem.
// This appears to work correctly.
// DH 24.2.00
func wave3d(nx,nt)
{
	require,"plwf.i";
	orient3;
        limits;
// Define a cube:
	x = span(-10.,10.,nx);
	y = x; z = x;
	dx = x(2)-x(1); dt = dx; dy = dx; dz = dx;
	m = (dt/dx)^2;
	u = array(0., nx,nx,nx); v = u; w = u;
	u(nx/2,nx/2,nx/2) = 1.;
	movie, wave3d_step, time_limit, min_interframe;
        wave3d_step, nt;
}

func wave3d_step(n)
{
  uxx= u(dif,2:-1,2:-1)(dif,,);
  uyy= u(2:-1,dif,2:-1)(,dif,);
  uzz= u(2:-1,2:-1,dif)(,,dif);
  v(2:-1,2:-1,2:-1)= 2.*u(2:-1,2:-1,2:-1) - w(2:-1,2:-1,2:-1) +
    m*(uxx+uyy+uzz);
  w= u;
  u= v;

  plwf, u(,,nx/2);
  draw3;
  return (n < nt);
}

// ----------------------------------------------------------------
// Jacobi iteration for Laplace or Poisson equations.
// This problem is 11.16 from DuChateau & Zachmann:
// u_xx + u_yy = 0 , 0 <= x,y <= 1
// u(x,y) = exp(2*pi*x)*sin(2*pi*y) on bdy.
// DH 14.3.00
func sor(nx,ny,iter)
{
// Nonzero f gives the Poisson equation.
 u = array(0., nx,ny); f = u;
 dx = 1./nx; dy = 1./ny;
 x = span(0.,1.,nx); y = x;
 u(1,)=exp(2.*pi*x(1))*sin(2.*pi*y);
 u(nx,)=exp(2.*pi*x(nx))*sin(2.*pi*y);
 u(,1)=exp(2.*pi*x)*sin(2.*pi*y(1));
 u(,ny)=exp(2.*pi*x)*sin(2.*pi*y(ny));
 for( k = 1; k <= iter; ++k ){
  for( n = 2; n <= nx-1; ++n ){
   for( m = 2; m <= ny-1; ++m ){
    u(n,m)=(u(n,m-1)+u(n-1,m)+u(n,m+1)+u(n+1,m)-dx*dy*f(n,m))/4.;
   }
  }
 }
 palette,"heat.gp";
 pli,u,0.,0.,1.,1.;
}

// ----------------------------------------------------------------
// Jacobi iteration for Laplace or Poisson equations.
// This problem is 11.16 from DuChateau & Zachmann:
// u_xx + u_yy = f , 0 <= x,y <= 1
// u(x,y) = 0 on bdy.
// This version computed the Green's function.
// DH 16.3.00
func gsor(nx,ny,iter)
{
 require, "plwf.i";
 orient3;
// Nonzero f gives the Poisson equation.
 u = array(0., nx,ny); f = u;
 dx = 1./nx; dy = 1./ny;
 x = span(0.,1.,nx); y = x;
 f(nx/2,ny/2)=-1.;
 for( k = 1; k <= iter; ++k ){
  for( n = 2; n <= nx-1; ++n ){
   for( m = 2; m <= ny-1; ++m ){
    u(n,m)=(u(n,m-1)+u(n-1,m)+u(n,m+1)+u(n+1,m)-dx*dy*f(n,m))/4.;
   }
  }
 }
 plwf,u;
}

// ----------------------------------------------------------------
// Routine for solving biharmonic equation.  DH 20.3.00
// Appears to work correctly.
func bihar(nx,ny,iter)
{
 require, "plwf.i";
 orient3;
 u = array(0., nx,ny); f = u;
 dx = 1./nx; dy = 1./ny;
 x = span(0.,1.,nx); y = span(0.,1.,ny);
 u(nx/2,ny/2)=1.;
 for( k = 1; k <= iter; ++k ){
  for( i = 3; i <= nx-2; ++i ){
   for( j = 3; j <= ny-2; ++j ){
    u(i,j)=(8.*(u(i+1,j)+u(i-1,j)+u(i,j-1)+u(i,j+1))-2.*(u(i-1,j-1)+u(i+1,j+1)+u(i+1,j-1)+u(i-1,j+1))-u(i+2,j)-u(i-2,j)-u(i,j-2)-u(i,j+2)+dx^2*dy^2*f(i,j))/20.;
   }
  }
 }
 plwf,u;
}

// ----------------------------------------------------------------
// Jacobi iteration for Helmholtz equation.
// k = wavenumber of mode
// DH 21.3.00 - appears to work correctly.
func helmholtz(k,nx,ny,iter)
{
 require, "plwf.i";
 orient3;
 u = array(0., nx,ny);
 dx = 1./nx; dy = 1./ny;
 x = span(0.,1.,nx);
 y = span(0.,1.,ny);
 u(nx/2,) = sin(pi*y);
 for( i = 1; i <= iter; ++i ){
  for( n = 2; n <= nx-1; ++n ){
   for( m = 2; m <= ny-1; ++m ){
    u(n,m)=(u(n,m-1)+u(n-1,m)+u(n,m+1)+u(n+1,m)-dx*dy*k^2*u(n,m))/4.;
   }
  }
 }
 plwf,u;
}

// ----------------------------------------------------------------
// Simple FD routine to solve the 1D Klein-Gordon wave equation
// u_tt = c^2*u_xx - zeta*u
// with reflecting BC's.
// DH 6.4.00
func kg(nx,nt)
{
// To get standing waves, try this domain:
        x = span(0.,1.,nx);
// This larger domain is nice for watching propagation:
//      x = span(-10.,10.,nx);
	dx = x(2)-x(1); dt = dx;
	m = (dt/dx)^2;
	u = array(0.,dimsof(x)); v = u; w = u;
        c=0.25; zeta=0.1;
// An initial Gaussian distribution:
//      f = exp(-50.*x^2);
// This initial distribution is for standing waves:
        f = sin(pi*x);
// Allow both position u and velocity w to be Gaussian:
	u = f; w = f;
// Enforce BCs:
        v(1)= v(0)= 0.;
        limits, min(x), max(x), -1.2*max(abs(u)), 1.2*max(abs(u))
	movie, kg_step, time_limit, min_interframe;
        kg_step, nt;
}

func kg_step(j)
{
  v(2:-1)= 2.*u(2:-1)-w(2:-1) + m*c^2*u(dif)(dif) - zeta*u(2:-1)*dt^2;
  w= u;
  u= v;

  plg, u, x;
  return (j < nt);
}
