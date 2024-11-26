
add_executable(anno_snippets
  anno_snippets.cpp
)
target_link_libraries(anno_snippets PRIVATE
  anno::anno
)
