
add_executable(arguments
    arguments.cpp
)
target_link_libraries(arguments
    anno::anno
)
target_compile_options(arguments PRIVATE
    "-Wall"
)
