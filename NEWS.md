# hySpc.read.txt 0.0.x 2020-xx-xx

## User-Visible Changes

* Moved functions from **hyperSpec** to **hySpc.read.txt**:
  - `hyperSpec::read.txt.Witec()` -> `hySpc.read.txt::read_txt_Witec()`
  - `hyperSpec::read.dat.Witec()` -> `hySpc.read.txt::read_dat_Witec()`
  - `hyperSpec::read.txt.Witec.Graph()` -> `hySpc.read.txt::read_txt_Witec_Graph()`
* New function `read_txt_Witec_TrueMatch()` to import text exports from Witec TrueMatch.

## New Features
* When new versions of the data are pushed to this repo, the newly created `.tar.gz` is automatically deployed to `pkg-repo`.
