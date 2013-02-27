project(ICEBURG)

if(NOT SDL_FOUND)
    message("SDL not found!")
endif(NOT SDL_FOUND)

if (NOT SDL_TTF_FOUND)
    message("SDL_ttf not found!")
endif(NOT SDL_TTF_FOUND)

set(ICEBURG_SRCS
    main.c
    parser.c
    problem.c
    render.c
    scanner.c
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_executable(iceburg ${ICEBURG_SRCS})
target_link_libraries(iceburg ${SDL_LIBRARY} ${SDL_TTF_LIBRARY} ${OPENGL_LIBRARIES} ${BOX2D_LIBRARY})

