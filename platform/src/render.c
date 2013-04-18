/*
 * render.c
 * Implementations of rendering functions.
 */

#include <math.h>
#include "render.h"

/*
 * Draw an arrowhead representing a vector
 */
void draw_vector(double x, double y, double length, double angle) 
{
    glPushMatrix();
    glTranslated(x, y, 0.0);
    glRotated(angle, 0.0, 0.0, 1.0);
    glScaled(length, length, 0.0);

    glBegin(GL_LINE_LOOP);
        glVertex2d(-0.5, 0.25);
        glVertex2d(0.5, 0.0);
        glVertex2d(-0.5, -0.25);
    glEnd();

    glPopMatrix();
}

/*
 * Draw a filled polygon
 */
void fill_polygon(double *pts, int count)
{
    int i;

    glPushMatrix();
    glBegin(GL_POLYGON);
        for (i = 0; i < count; i++) {
            glVertex2d(pts[2*i], pts[2*i+1]);
        }
    glEnd();
    glPopMatrix();
}

/*
 * Draw a wireframe polygon
 */
void draw_polygon(double *pts, int count)
{
    int i;

    glPushMatrix();
    glBegin(GL_LINE_LOOP);
        for (i = 0; i < count; i++) {
            glVertex2d(pts[2*i], pts[2*i+1]);
        }
    glEnd();
    glPopMatrix();
}

/*
 * Draw a circle
 */
void draw_circle(double x, double y, double radius)
{
    const int num_points = 16;
    double t;
    int i;

    glPushMatrix();
    glTranslated(x, y, 0.0);
    glBegin(GL_LINE_LOOP);
        for (i = 0; i < num_points; i++) {
            t = 2 * M_PI * ((double)i / num_points);
            glVertex2d(radius * cos(t), radius * sin(t));
        }
    glEnd();
    glPopMatrix();
}