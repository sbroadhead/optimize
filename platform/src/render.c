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
 * Draw a polygon
 */
void draw_polygon(double *pts, int count)
{
    double color[4];
    int i;

    glPushMatrix();

    glBegin(GL_LINE_LOOP);
        for (i = 0; i < count; i++) {
            glVertex2d(pts[2*i], pts[2*i+1]);
        }
    glEnd();

    glGetDoublev(GL_CURRENT_COLOR, color);
    glColor4d(color[0], color[1], color[2], 0.25);
    
    glBegin(GL_POLYGON);
        for (i = 0; i < count; i++) {
            glVertex2d(pts[2*i], pts[2*i+1]);
        }
    glEnd();

    glColor4d(color[0], color[1], color[2], color[3]);
    glPopMatrix();
}
