cmake_minimum_required(VERSION 3.2)
project(elpcre)

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(PkgConfig REQUIRED)
pkg_check_modules(PCRE8 REQUIRED libpcre2-8)

file(GLOB_RECURSE LIB_SRC_FILES ${PROJECT_SOURCE_DIR}/src/*.c)
add_library(${PROJECT_NAME} SHARED ${LIB_SRC_FILES})

target_include_directories(${PROJECT_NAME} PUBLIC
  ${PROJECT_SOURCE_DIR}/include
  ${PCRE8_INCLUDE_DIRS})

target_link_libraries(${PROJECT_NAME} ${PCRE8_LIBRARIES})
target_compile_options(${PROJECT_NAME} PUBLIC ${PCRE8_CFLAGS_OTHER})
