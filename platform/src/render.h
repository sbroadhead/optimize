/*
 * render.h
 * Interface to the rendering functions.
 */

#ifndef _RENDER_H_
#define _RENDER_H_

#if defined __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#else
#include <gl/gl.h>
#include <gl/glu.h>
#endif

void draw_vector(double x, double y, double length, double angle);
void fill_polygon(double *pts, int count);
void draw_polygon(double *pts, int count);
void draw_circle(double x, double y, double radius);

#endif

