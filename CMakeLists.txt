cmake_minimum_required(VERSION "3.18.0")

project("dramatic" LANGUAGES CXX)

add_library("dramatic" INTERFACE)

target_include_directories("dramatic" INTERFACE
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)

target_compile_features("dramatic" INTERFACE "cxx_std_20")

add_executable("demo"
  "main.cpp"
)

target_link_libraries("demo" PRIVATE "dramatic")