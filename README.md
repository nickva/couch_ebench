Description
===========

Silly benchmark to see the speed / accuracy tradeoff for various ways of
computing an encoded json document sizes.

Main code is in [couch_ebench.erl](src/couch_ebench.erl).

There are a few json encoders used as well as a 2 custom modules.
One [module](src/couch_ejson_size2.erl) tries to match the encoded size of
jiffy:encode, the [other](src/couch_ejson_size1.erl) doesn't and is trying to
be on the conservaitve side, that is, it reports a smaller sizse usually but is
faster.

To fetch deps & build:

```
$ ./build.sh
```

To run:

```
$ ./run.sh [Inputsize1] [Inputsize2] ...

```

Where `Inputsize` is the size of the generated inputs (how deep to make the
nested objects, how long to make the string, how many copies of the json
documents to use, etc.

Example
=======

```
 ./build.sh && ./run.sh 10 1000 10000
==> jiffy (get-deps)
==> jsx (get-deps)
==> jsone (get-deps)
==> couch_ebench (get-deps)
==> jiffy (compile)
==> jsx (compile)
==> jsone (compile)
==> couch_ebench (compile)
Generating inputs...
Running benchmark 4 encoders x 42 tests. Input size(s): [10,1000,10000]
Finished running benchmark.
Enc.  | Input size | Input type   | USec       | Bytes
------+------------+--------------+------------+----------

jiffy
      |         10 |       file_1 |      170.7 |      13031
      |         10 |    float_17d |        3.2 |        201
      |         10 | float_1d_arr |        2.8 |         41
      |         10 | float_large_ |        2.9 |         71
      |         10 | int_large_ar |       15.7 |       1031
      |         10 | int_small_ar |        2.4 |         31
      |         10 |   nested_arr |        1.7 |         22
      |         10 |   nested_obj |        2.1 |         69
      |         10 |    str_basic |        0.6 |         12
      |         10 |     str_cent |        0.5 |         22
      |         10 |     str_ctr1 |        0.6 |         62
      |         10 |     str_escn |        0.5 |         22
      |         10 |     str_euro |        0.8 |         32
      |         10 |    str_hwair |        0.8 |         42
      |       1000 |       file_1 |    18171.8 |    1303001
      |       1000 |    float_17d |      280.3 |      20001
      |       1000 | float_1d_arr |      262.5 |       4001
      |       1000 | float_large_ |      233.9 |       7001
      |       1000 | int_large_ar |     1864.8 |     103001
      |       1000 | int_small_ar |      196.1 |       3001
      |       1000 |   nested_arr |      133.1 |       2002
      |       1000 |   nested_obj |      175.8 |       6009
      |       1000 |    str_basic |        2.9 |       1002
      |       1000 |     str_cent |       10.9 |       2002
      |       1000 |     str_ctr1 |        8.7 |       6002
      |       1000 |     str_escn |        6.0 |       2002
      |       1000 |     str_euro |       16.0 |       3002
      |       1000 |    str_hwair |       28.8 |       4002
      |      10000 |       file_1 |   185333.1 |   13030001
      |      10000 |    float_17d |     3046.8 |     200001
      |      10000 | float_1d_arr |     2168.3 |      40001
      |      10000 | float_large_ |     2182.0 |      70001
      |      10000 | int_large_ar |    27650.3 |    1030001
      |      10000 | int_small_ar |     2384.8 |      30001
      |      10000 |   nested_arr |     1259.9 |      20002
      |      10000 |   nested_obj |     1832.3 |      60009
      |      10000 |    str_basic |       29.1 |      10002
      |      10000 |     str_cent |       97.6 |      20002
      |      10000 |     str_ctr1 |       62.6 |      60002
      |      10000 |     str_escn |       43.7 |      20002
      |      10000 |     str_euro |      140.1 |      30002
      |      10000 |    str_hwair |      172.2 |      40002

erlfast
      |         10 |       file_1 |       43.7 |      13011
      |         10 |    float_17d |        2.2 |         41
      |         10 | float_1d_arr |        1.8 |         41
      |         10 | float_large_ |       14.5 |        221
      |         10 | int_large_ar |        1.4 |       1031
      |         10 | int_small_ar |        0.4 |         31
      |         10 |   nested_arr |        0.4 |         22
      |         10 |   nested_obj |        0.8 |         69
      |         10 |    str_basic |        0.0 |         12
      |         10 |     str_cent |        0.0 |         22
      |         10 |     str_ctr1 |        0.0 |         12
      |         10 |     str_escn |        0.0 |         12
      |         10 |     str_euro |        0.0 |         32
      |         10 |    str_hwair |        0.0 |         42
      |       1000 |       file_1 |     4403.1 |    1301001
      |       1000 |    float_17d |       96.4 |       4001
      |       1000 | float_1d_arr |      104.8 |       4001
      |       1000 | float_large_ |     1355.6 |      22001
      |       1000 | int_large_ar |      172.1 |     103001
      |       1000 | int_small_ar |       29.9 |       3001
      |       1000 |   nested_arr |       34.8 |       2002
      |       1000 |   nested_obj |       64.6 |       6009
      |       1000 |    str_basic |        0.0 |       1002
      |       1000 |     str_cent |        0.0 |       2002
      |       1000 |     str_ctr1 |        0.0 |       1002
      |       1000 |     str_escn |        0.0 |       1002
      |       1000 |     str_euro |        0.0 |       3002
      |       1000 |    str_hwair |        0.0 |       4002
      |      10000 |       file_1 |    43863.5 |   13010001
      |      10000 |    float_17d |      956.9 |      40001
      |      10000 | float_1d_arr |      949.8 |      40001
      |      10000 | float_large_ |    13652.1 |     220001
      |      10000 | int_large_ar |     1421.1 |    1030001
      |      10000 | int_small_ar |      360.9 |      30001
      |      10000 |   nested_arr |      358.8 |      20002
      |      10000 |   nested_obj |      606.8 |      60009
      |      10000 |    str_basic |        0.0 |      10002
      |      10000 |     str_cent |        0.0 |      20002
      |      10000 |     str_ctr1 |        0.0 |      10002
      |      10000 |     str_escn |        0.1 |      10002
      |      10000 |     str_euro |        0.1 |      30002
      |      10000 |    str_hwair |        0.0 |      40002

erlexact
      |         10 |       file_1 |      346.7 |      13031
      |         10 |    float_17d |        8.0 |        201
      |         10 | float_1d_arr |        7.6 |         41
      |         10 | float_large_ |        8.2 |         71
      |         10 | int_large_ar |        0.9 |       1031
      |         10 | int_small_ar |        1.4 |         31
      |         10 |   nested_arr |        0.5 |         22
      |         10 |   nested_obj |        1.4 |         69
      |         10 |    str_basic |        0.3 |         12
      |         10 |     str_cent |        0.5 |         22
      |         10 |     str_ctr1 |        0.3 |         62
      |         10 |     str_escn |        0.3 |         22
      |         10 |     str_euro |        0.4 |         32
      |         10 |    str_hwair |        0.3 |         42
      |       1000 |       file_1 |    35026.9 |    1303001
      |       1000 |    float_17d |      919.0 |      20001
      |       1000 | float_1d_arr |      821.8 |       4001
      |       1000 | float_large_ |      809.0 |       7001
      |       1000 | int_large_ar |       99.8 |     103001
      |       1000 | int_small_ar |       54.6 |       3001
      |       1000 |   nested_arr |       38.2 |       2002
      |       1000 |   nested_obj |      127.4 |       6009
      |       1000 |    str_basic |       26.0 |       1002
      |       1000 |     str_cent |       43.8 |       2002
      |       1000 |     str_ctr1 |       21.1 |       6002
      |       1000 |     str_escn |       24.1 |       2002
      |       1000 |     str_euro |       36.1 |       3002
      |       1000 |    str_hwair |       28.6 |       4002
      |      10000 |       file_1 |   363273.3 |   13030001
      |      10000 |    float_17d |     8589.2 |     200001
      |      10000 | float_1d_arr |     8709.6 |      40001
      |      10000 | float_large_ |     8299.4 |      70001
      |      10000 | int_large_ar |      949.6 |    1030001
      |      10000 | int_small_ar |      513.5 |      30001
      |      10000 |   nested_arr |      414.5 |      20002
      |      10000 |   nested_obj |     1216.9 |      60009
      |      10000 |    str_basic |      355.7 |      10002
      |      10000 |     str_cent |      615.7 |      20002
      |      10000 |     str_ctr1 |      261.1 |      60002
      |      10000 |     str_escn |      253.7 |      20002
      |      10000 |     str_euro |      322.0 |      30002
      |      10000 |    str_hwair |      332.6 |      40002

jsone
      |         10 |       file_1 |      699.0 |      13411
      |         10 |    float_17d |       11.2 |        271
      |         10 | float_1d_arr |        9.7 |        271
      |         10 | float_large_ |       17.8 |        281
      |         10 | int_large_ar |       11.0 |       1031
      |         10 | int_small_ar |        3.1 |         31
      |         10 |   nested_arr |        1.8 |         22
      |         10 |   nested_obj |        4.5 |         69
      |         10 |    str_basic |        0.8 |         12
      |         10 |     str_cent |        1.5 |         62
      |         10 |     str_ctr1 |        1.0 |         62
      |         10 |     str_escn |        0.5 |         22
      |         10 |     str_euro |        1.5 |         62
      |         10 |    str_hwair |        2.7 |        122
      |       1000 |       file_1 |    93229.8 |    1341001
      |       1000 |    float_17d |      940.6 |      27001
      |       1000 | float_1d_arr |      974.1 |      27001
      |       1000 | float_large_ |     1857.5 |      28001
      |       1000 | int_large_ar |      987.7 |     103001
      |       1000 | int_small_ar |      137.0 |       3001
      |       1000 |   nested_arr |      112.4 |       2002
      |       1000 |   nested_obj |      295.7 |       6009
      |       1000 |    str_basic |       50.7 |       1002
      |       1000 |     str_cent |      111.1 |       6002
      |       1000 |     str_ctr1 |       70.3 |       6002
      |       1000 |     str_escn |       32.9 |       2002
      |       1000 |     str_euro |      113.2 |       6002
      |       1000 |    str_hwair |      302.7 |      12002
      |      10000 |       file_1 |   972721.8 |   13410001
      |      10000 |    float_17d |    10365.8 |     270001
      |      10000 | float_1d_arr |     9820.9 |     270001
      |      10000 | float_large_ |    18326.3 |     280001
      |      10000 | int_large_ar |    12271.9 |    1030001
      |      10000 | int_small_ar |     1423.4 |      30001
      |      10000 |   nested_arr |     1395.8 |      20002
      |      10000 |   nested_obj |     3795.6 |      60009
      |      10000 |    str_basic |      636.1 |      10002
      |      10000 |     str_cent |     1077.9 |      60002
      |      10000 |     str_ctr1 |      666.1 |      60002
      |      10000 |     str_escn |      304.4 |      20002
      |      10000 |     str_euro |     1130.7 |      60002
      |      10000 |    str_hwair |     2607.8 |     120002
```
