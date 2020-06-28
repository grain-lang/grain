#/bin/bash -ex

if command -v cmake3; then
    cmake_cmd=cmake3
elif command -v cmake; then
    cmake_cmd=cmake
else
    exit 1
fi

echo "cmd: \"$cmake_cmd\"" >> conf-cmake.config
