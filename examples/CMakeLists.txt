
add_executable(json
    json.cpp
)
target_link_libraries(json
    anno::anno
)
target_compile_options(json PRIVATE
    "-Wall"
)

add_executable(arguments
    arguments.cpp
)
target_link_libraries(arguments
    anno::anno
)
target_compile_options(arguments PRIVATE
    "-Wall"
)
