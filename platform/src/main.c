#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>
#include <SDL_ttf.h>
#include "render.h"
#include "problem.h"
#include "parser.h"

#define WINDOW_WIDTH 1024
#define WINDOW_HEIGHT 768

/*
 * Initialize the renderer.
 */
void init_render(problem_state_t *state)
{
    double x1, y1, x2, y2;
    double h, w;
    double extra;
    double aspect = (double)WINDOW_WIDTH / (double)WINDOW_HEIGHT;

    glViewport(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT);

    /* Fit the whole problem on screen */
    get_render_extents(state, &x1, &y1, &x2, &y2);
    
    /* Fix aspect ratio */
    if ((x2 - x1) / (y2 - y1) < aspect) {
        w = (y2 - y1) * aspect;
        extra = w - (x2 - x1);
        x1 -= extra / 2;
        x2 += extra / 2;
    } else {
        h = (x2 - x1) / aspect;
        extra = h - (y2 - y1);
        y1 -= extra / 2;
        y2 += extra / 2;
    }

    printf("Setting render extents to:\n    %.2f, %.2f, %.2f, %.2f\n",
            x1 - 1.0, y1 - 1.0, x2 + 1.0, y2 + 1.0);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(x1 - 1.0, x2 + 1.0, y1 - 1.0, y2 + 1.0, -1.0, 1.0);

    state->x1 = x1 - 1.0;
    state->x2 = x2 + 1.0;
    state->y1 = y1 - 1.0;
    state->y2 = y2 + 1.0;
    
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

/*
 * Render the current problem state.
 */
void render(problem_state_t *state)
{
    int i;
    double j;

    glClearColor(0.0, 0.0, 0.0, 1);
    glClear(GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

    /* Draw the current position */
    glColor3d(1.0, 0.0, 1.0);
    draw_vector(state->current_pos.x, state->current_pos.y, 0.25, state->current_angle);
    draw_circle(state->current_pos.x, state->current_pos.y, state->problem->radius);

    /* Draw the destination */
    glColor3d(0.0, 1.0, 1.0);
    draw_vector(state->problem->end_pos.x, state->problem->end_pos.y, 0.25, state->problem->end_angle);

    /* Draw the polygons */
    glColor3d(1.0, 0.0, 0.0);
    for (i = 0; i < state->problem->polygon_count; i++) {
        draw_polygon((double *)state->problem->polygons[i].points, state->problem->polygons[i].point_count);
    }
    
    /* Stencil the polygons */
    glClearStencil(0);
    glColorMask(0, 0, 0, 0);
    glEnable(GL_STENCIL_TEST);
    glStencilFunc(GL_ALWAYS, 1, 1);
    glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
    for (i = 0; i < state->problem->polygon_count; i++) {
        fill_polygon((double *)state->problem->polygons[i].points, state->problem->polygons[i].point_count);
    }
    glStencilFunc(GL_EQUAL, 1, 1);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    glColorMask(1, 1, 1, 1);

    /* Draw the pattern in the polygons */
    glColor3d(1.0, 0.0, 0.0);
    glLineStipple(1, 0x2222);
    glEnable(GL_LINE_STIPPLE);
    glPushMatrix();
    for (j = state->x1; j < state->x2; j += 0.25) {
        glBegin(GL_LINES);
            glVertex2d(j, state->y1);
            glVertex2d(j, state->y2);
        glEnd();
    }
    for (j = state->y1; j < state->y2; j += 0.25) {
        glBegin(GL_LINES);
            glVertex2d(state->x1, j);
            glVertex2d(state->x2, j);
        glEnd();
    }
    glDisable(GL_LINE_STIPPLE);
    glPopMatrix();

    glDisable(GL_STENCIL_TEST);
}

/*
 * Step forward in time.
 */
void step(problem_state_t *state)
{
}

/*
 * Process SDL events.
 */
int process_event(SDL_Event *evt)
{
    switch (evt->type) {
    case SDL_KEYDOWN:
        switch (evt->key.keysym.sym) {
        case SDLK_q:
            return 0;
            break;
        default:
            break;
        }
        break;
    case SDL_QUIT:
        return 0;
    default:
        break;
    }

    return 1;
}

/*
 * Create the window and launch the main loop.
 */
int run(problem_state_t *state)
{
    SDL_Surface *window;
    SDL_Event evt;
    int running = 1;

    /* Initialize SDL and create the window */
    if (!SDL_Init(SDL_INIT_EVERYTHING) == -1) {
        printf("Could not initialize SDL\n");
        return 1;
    }

    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 1);

    /* Create and initialize the window */
    window = SDL_SetVideoMode(WINDOW_WIDTH, WINDOW_HEIGHT, 32, SDL_HWSURFACE | SDL_DOUBLEBUF | SDL_OPENGL);
    if (window == NULL) {
        printf("Could not create SDL surface\n");
        return 1;
    }
    SDL_WM_SetCaption("Iceburg Solver", NULL);

    init_render(state);

    while (running) {
        while (SDL_PollEvent(&evt)) {
            running = process_event(&evt);
        }
        render(state);
        step(state);
        SDL_GL_SwapBuffers();
    }

    printf("Quitting...\n");
    return 0;
}

/*
 */
int main(int argc, char *argv[])
{
    int failed;
    problem_t problem;
    problem_state_t state;
    FILE *f;

    printf("Iceburg Solver\n\n");

    if (argc != 2) {
        printf("Usage: iceburg [input]\n");
        return 1;
    }

    /* Parse the definition file */
    f = fopen(argv[1], "r");
    if (!f) {
        printf("Failed to open file: %s\n", argv[1]);
        return 1;
    }
    failed = parse_problem_file(f, &problem);
    fclose(f);

    if (failed) {
        printf("Aborting due to parse error...\n");
        return 1;
    }

    printf("Initializing problem:\n");
    printf("    Start position: %.2f, %.2f\n", problem.start_pos.x, problem.start_pos.y);
    printf("    Destination position: %.2f, %.2f\n", problem.end_pos.x, problem.end_pos.y);
    printf("    Read %d polygons\n", problem.polygon_count);

    state.problem = &problem;
    state.tick = 0;
    state.current_pos = problem.start_pos;
    state.current_angle = problem.start_angle;

    return run(&state);
}
