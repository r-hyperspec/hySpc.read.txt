# hySpc.read.txt 0.0.x 2020-xx-xx

## User-visible changes

* moved functions from **hyperSpec** to **hySpc.read.txt**: 
  - `hyperSpec::read.txt.Witec()` -> `hySpc.read.txt::read_txt_Witec()`
  - `hyperSpec::read.dat.Witec()` -> `hySpc.read.txt::read_dat_Witec()`
  - `hyperSpec::read.txt.Witec.Graph()` -> `hySpc.read.txt::read_txt_Witec_Graph()`
* new function `read_txt_Witec_TrueMatch()` to import text exports from Witec TrueMatch
