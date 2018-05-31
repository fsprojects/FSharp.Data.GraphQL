``` ini

BenchmarkDotNet=v0.10.9, OS=Windows 10.0.17134
Processor=Intel Core i7-2600 CPU 3.40GHz (Sandy Bridge), ProcessorCount=8
.NET Core SDK=2.1.201
  [Host] : .NET Core 2.0.7 (Framework 4.6.26328.01), 64bit RyuJIT DEBUG
  Core   : .NET Core 2.0.7 (Framework 4.6.26328.01), 64bit RyuJIT
  Mono   : Mono 5.12.0 (Visual Studio), 32bit 


```
 |                         Method |  Job | Runtime |      Mean |      Error |     StdDev |       Min |       Max |     Op/s |
 |------------------------------- |----- |-------- |----------:|-----------:|-----------:|----------:|----------:|---------:|
 |   BenchmarkSimpleQueryUnparsed | Core |    Core |  70.38 us |  1.4118 us |  1.8847 us |  68.37 us |  75.13 us | 14,208.6 |
 |     BenchmarkSimpleQueryParsed | Core |    Core |  32.10 us |  0.5598 us |  0.4963 us |  31.42 us |  33.15 us | 31,153.7 |
 |    BenchmarkSimpleQueryPlanned | Core |    Core |  25.55 us |  0.5096 us |  0.7144 us |  24.75 us |  27.34 us | 39,145.9 |
 |     BenchmarkFlatQueryUnparsed | Core |    Core | 684.94 us |  6.8459 us |  6.4036 us | 676.80 us | 701.67 us |  1,460.0 |
 |       BenchmarkFlatQueryParsed | Core |    Core | 623.55 us |  3.7850 us |  2.9551 us | 619.72 us | 628.48 us |  1,603.7 |
 |      BenchmarkFlatQueryPlanned | Core |    Core | 613.85 us |  4.8045 us |  4.4941 us | 608.82 us | 621.94 us |  1,629.1 |
 |   BenchmarkNestedQueryUnparsed | Core |    Core | 512.57 us | 10.1313 us | 14.2027 us | 498.33 us | 544.54 us |  1,951.0 |
 |     BenchmarkNestedQueryParsed | Core |    Core | 361.61 us |  1.7930 us |  1.4972 us | 359.53 us | 364.88 us |  2,765.4 |
 |    BenchmarkNestedQueryPlanned | Core |    Core | 352.86 us |  8.1333 us | 12.1736 us | 341.14 us | 390.51 us |  2,834.0 |
 | BenchmarkFilteredQueryUnparsed | Core |    Core | 180.70 us |  1.2709 us |  1.1888 us | 179.06 us | 183.18 us |  5,534.1 |
 |   BenchmarkFilteredQueryParsed | Core |    Core |  92.51 us |  0.6357 us |  0.5946 us |  91.21 us |  93.51 us | 10,809.3 |
 |  BenchmarkFilteredQueryPlanned | Core |    Core |  80.48 us |  0.7093 us |  0.6635 us |  79.78 us |  81.83 us | 12,426.2 |
 |   BenchmarkSimpleQueryUnparsed | Mono |    Mono |  74.38 us |  1.4549 us |  1.4289 us |  72.59 us |  76.79 us | 13,444.5 |
 |     BenchmarkSimpleQueryParsed | Mono |    Mono |  43.24 us |  0.3037 us |  0.2692 us |  42.83 us |  43.70 us | 23,125.4 |
 |    BenchmarkSimpleQueryPlanned | Mono |    Mono |  35.40 us |  0.2841 us |  0.2657 us |  34.88 us |  35.82 us | 28,249.2 |
 |     BenchmarkFlatQueryUnparsed | Mono |    Mono | 808.01 us |  3.7499 us |  3.3242 us | 802.62 us | 815.69 us |  1,237.6 |
 |       BenchmarkFlatQueryParsed | Mono |    Mono | 766.00 us |  5.1742 us |  4.0397 us | 758.99 us | 771.95 us |  1,305.5 |
 |      BenchmarkFlatQueryPlanned | Mono |    Mono | 757.02 us |  4.1596 us |  3.8909 us | 750.66 us | 762.52 us |  1,321.0 |
 |   BenchmarkNestedQueryUnparsed | Mono |    Mono | 733.36 us |  2.9784 us |  2.6403 us | 728.92 us | 737.69 us |  1,363.6 |
 |     BenchmarkNestedQueryParsed | Mono |    Mono | 579.53 us |  6.1527 us |  5.7553 us | 573.14 us | 592.08 us |  1,725.5 |
 |    BenchmarkNestedQueryPlanned | Mono |    Mono | 565.22 us |  4.3185 us |  3.8283 us | 558.93 us | 572.56 us |  1,769.2 |
 | BenchmarkFilteredQueryUnparsed | Mono |    Mono | 216.07 us |  5.0906 us |  7.1363 us | 209.37 us | 240.12 us |  4,628.0 |
 |   BenchmarkFilteredQueryParsed | Mono |    Mono | 140.25 us |  1.4424 us |  1.3492 us | 138.28 us | 142.42 us |  7,130.3 |
 |  BenchmarkFilteredQueryPlanned | Mono |    Mono | 126.45 us |  1.6673 us |  1.3923 us | 124.77 us | 129.90 us |  7,908.1 |
