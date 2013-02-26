#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>
#include <SDL_ttf.h>
#include "render.h"
#include "problem.h"
#include "parser.h"

/*
 * Render the current problem state.
 */
void render(problem_state_t *state)
{
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

    /* Create and initialize the window */
    window = SDL_SetVideoMode(1024, 768, 32, SDL_SWSURFACE);
    if (window == NULL) {
        printf("Could not create SDL surface\n");
        return 1;
    }
    SDL_WM_SetCaption("Iceburg Solver", NULL);

    while (running) {
        while (SDL_PollEvent(&evt)) {
            running = process_event(&evt);
            render(state);
            step(state);
        }
    }

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

    if (argc != 2) {
        printf("Usage: iceburg [input]\n");
        return 1;
    }

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
    state.problem = &problem;
    state.tick = 0;
    state.current_pos = problem.start_pos;
    state.current_angle = problem.start_angle;

    return run(&state);
}
