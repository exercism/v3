## For maintainers

### Helper script for creating Fortran tests: create\_fortran\_test.py

An easy way to create an exercise test is to use the script
`bin/create\_fortran\_test.py`

Use this script to create an initial <exercise>\_test.f90 file which can be used as a template for your test.
Typically, you will have to replace the 'response'-function in the generated file with the correct function call.

Also note that Fortran has issues with special characters such as `\n` and `\t` so take special care handling these.

#### Prerequsites
- Working CMake and Fortran compiler
- Python3.x (You can make it may work with Python2, but I have not made the
effort to make it backwards compatible)
- latest version of https://github.com/exercism/problem-specifications.git

#### Work flow for creating a new test
- pull latest changes from `exercism/problem-specifications`
- run this script for the example you want to create
- copy `config/CMakeLists.txt` to exercise directory (You can use `bin/update-cmake-files` for this)
- implement working exercise
- fix potential problematic tests (see eg. exercise/bob "Test 20" and "Test 24")
- ensure `ctest` command validates without errors
- open a pull request with your changes

For bob example:

```bash
$ python3 config/create_fortran_test.py -j ../../../exercism/problem-specifications/exercises/bob/canonical-data.json -t exercises/bob/bob_test.f90
Namespace(json='../../../exercism/problem-specifications/exercises/bob/canonical-data.json', target='exercises/bob/bob_test.f90')
Wrote : exercises/bob/bob_test.f90
$ cp config/CMakeLists.txt exercises/bob/.
$ cd exercises/bob
$ touch bob.f90
$ mkdir build
$ cd build
$ cmake ..
$ make
$ ctest -V
```

#### Changing `CMakeLists.txt`
When changing the cmake file, update the master copy in `config/` and use `bin/update-cmake-files` to copy it to all exercise directories.
