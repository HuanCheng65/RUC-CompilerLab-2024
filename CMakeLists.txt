cmake_minimum_required(VERSION 3.10)

# 项目信息
project(SysYCompiler VERSION 1.0 LANGUAGES CXX C)

# 设置C++标准
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# 编译选项
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -g -fPIC")

# 查找Bison和Flex
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)

find_package(fmt CONFIG REQUIRED)
find_package(range-v3 CONFIG REQUIRED)
find_package(CLI11 CONFIG REQUIRED)

# 查找LLVM
find_package(LLVM REQUIRED CONFIG)

if (LLVM_FOUND)
    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
    message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
else()
    message(FATAL_ERROR "LLVM not found")
endif()

# 设置LLVM相关的编译选项
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# 指定Bison和Flex源文件
set(BISON_SRC ${CMAKE_SOURCE_DIR}/src/frontend/SysY.ypp)
set(FLEX_SRC ${CMAKE_SOURCE_DIR}/src/frontend/SysY.lpp)

# 生成Bison和Flex输出文件
BISON_TARGET(Parser ${BISON_SRC} ${CMAKE_BINARY_DIR}/parser.cpp DEFINES_FILE ${CMAKE_BINARY_DIR}/parser.hpp)
FLEX_TARGET(Lexer ${FLEX_SRC} ${CMAKE_BINARY_DIR}/lexer.cpp DEFINES_FILE ${CMAKE_BINARY_DIR}/lexer.hpp)
ADD_FLEX_BISON_DEPENDENCY(Lexer Parser)

# 包含生成的头文件目录
include_directories(${CMAKE_BINARY_DIR})
# 包含项目的头文件目录
include_directories(${CMAKE_SOURCE_DIR}/src/frontend)
include_directories(${CMAKE_SOURCE_DIR}/src/backend)
include_directories(${CMAKE_SOURCE_DIR}/src)

file(GLOB_RECURSE SOURCES "${CMAKE_SOURCE_DIR}/src/*.cpp")
file(GLOB_RECURSE HEADERS "${CMAKE_SOURCE_DIR}/src/*.hpp")

# 源文件列表
set(SOURCE_FILES
    ${BISON_Parser_OUTPUTS}
    ${FLEX_Lexer_OUTPUTS}
    ${SOURCES}
)

# 包含文件列表
set(HEADER_FILES
    ${HEADERS}
)

# 可执行文件
add_executable(SysY ${SOURCE_FILES} ${HEADER_FILES})

# 链接LLVM库
llvm_map_components_to_libnames(LLVM_LIBS
    support
    core
    irprinter
    irreader
    passes
    x86asmparser
    x86codegen
    x86desc
    x86disassembler
    x86info
    x86targetmca
    mc
    mcjit
    orcjit
    native
    nativecodegen
    mirparser
    # 添加其他需要的LLVM组件
)

target_link_libraries(SysY ${LLVM_LIBS} fmt::fmt-header-only range-v3::meta range-v3::concepts range-v3::range-v3 CLI11::CLI11)

# 清理目标
add_custom_target(clean-all
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_BINARY_DIR}/clean-all.cmake
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

# 生成clean-all.cmake脚本
file(WRITE ${CMAKE_BINARY_DIR}/clean-all.cmake
"file(REMOVE_RECURSE ${CMAKE_BINARY_DIR}/parser.cpp ${CMAKE_BINARY_DIR}/parser.hpp ${CMAKE_BINARY_DIR}/lexer.cpp ${CMAKE_BINARY_DIR}/lexer.hpp ${CMAKE_BINARY_DIR}/SysY position.hh stack.hh location.hh parser.output *.o)\n"
)

# 启用测试
include(CTest)
enable_testing()

# 查找测试文件夹中的所有.c文件
file(GLOB TEST_FILES "${CMAKE_SOURCE_DIR}/tests/*/*.c")

foreach(TEST_FILE ${TEST_FILES})
    get_filename_component(TEST_DIR ${TEST_FILE} DIRECTORY)
    get_filename_component(TEST_BASENAME ${TEST_FILE} NAME_WE)

    # 创建一个运行测试的脚本
    set(TEST_SCRIPT ${CMAKE_BINARY_DIR}/${TEST_BASENAME}_test.cmake)
    file(WRITE ${TEST_SCRIPT}
        "message(\"Running test ${TEST_BASENAME}\")\n"
        "execute_process(COMMAND ${CMAKE_COMMAND} -E echo \"Generating ASM for ${TEST_FILE}\")\n"
        "execute_process(COMMAND ./SysY -o ${TEST_DIR}/out ${TEST_FILE})\n"
        "if(NOT EXISTS ${TEST_DIR}/out.s)\n"
        "    message(FATAL_ERROR \"Failed to generate ASM for ${TEST_FILE}\")\n"
        "endif()\n"
        "execute_process(COMMAND ${CMAKE_C_COMPILER} ${TEST_DIR}/out.s -o ${TEST_DIR}/out -g)\n"
        "if(EXISTS ${TEST_DIR}/${TEST_BASENAME}.out)\n"
        "    if(EXISTS ${TEST_DIR}/${TEST_BASENAME}.in)\n"
        "        execute_process(\n"
        "            COMMAND ${TEST_DIR}/out\n"
        "            INPUT_FILE ${TEST_DIR}/${TEST_BASENAME}.in\n"
        "            OUTPUT_FILE ${TEST_DIR}/result.out\n"
        "            RESULT_VARIABLE EXECUTE_RESULT\n"
        "        )\n"
        "    else()\n"
        "        execute_process(\n"
        "            COMMAND ${TEST_DIR}/out\n"
        "            OUTPUT_FILE ${TEST_DIR}/result.out\n"
        "            RESULT_VARIABLE EXECUTE_RESULT\n"
        "        )\n"
        "    endif()\n"
        "    if(NOT EXECUTE_RESULT EQUAL 0)\n"
        "        file(APPEND ${TEST_DIR}/result.out \"\$\{EXECUTE_RESULT\}\")\n"
        "    endif()\n"
        "    execute_process(COMMAND ${CMAKE_COMMAND} -E echo \"Test ${TEST_BASENAME} output:\")\n"
        "    execute_process(COMMAND ${CMAKE_COMMAND} -E cat ${TEST_DIR}/result.out)\n"
        "    execute_process(COMMAND ${CMAKE_COMMAND} -E echo \"Expected output:\")\n"
        "    execute_process(COMMAND ${CMAKE_COMMAND} -E cat ${TEST_DIR}/${TEST_BASENAME}.out)\n"
        "    execute_process(\n"
        "        COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${TEST_DIR}/result.out ${TEST_DIR}/${TEST_BASENAME}.out\n"
        "        RESULT_VARIABLE COMPARE_RESULT\n"
        "    )\n"
        "    if(COMPARE_RESULT EQUAL 1)\n"
        "        message(FATAL_ERROR \"Test ${TEST_BASENAME} failed\")\n"
        "    endif()\n"
        "endif()\n"
        "file(REMOVE ${TEST_DIR}/out.s ${TEST_DIR}/out ${TEST_DIR}/result.out)\n"
    )

    # 添加CTest测试
    add_test(
        NAME ${TEST_BASENAME}
        COMMAND ${CMAKE_COMMAND} -P ${TEST_SCRIPT}
    )
endforeach()
