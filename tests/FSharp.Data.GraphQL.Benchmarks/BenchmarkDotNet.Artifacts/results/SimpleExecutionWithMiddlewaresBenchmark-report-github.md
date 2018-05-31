``` ini

BenchmarkDotNet=v0.10.9, OS=Windows 10.0.17134
Processor=Intel Core i7-2600 CPU 3.40GHz (Sandy Bridge), ProcessorCount=8
.NET Core SDK=2.1.201
  [Host] : .NET Core 2.0.7 (Framework 4.6.26328.01), 64bit RyuJIT DEBUG
  Core   : .NET Core 2.0.7 (Framework 4.6.26328.01), 64bit RyuJIT
  Mono   : Mono 5.12.0 (Visual Studio), 32bit 


```
 |                         Method |  Job | Runtime |       Mean |      Error |     StdDev |        Min |        Max |      Op/s |
 |------------------------------- |----- |-------- |-----------:|-----------:|-----------:|-----------:|-----------:|----------:|
 |   BenchmarkSimpleQueryUnparsed | Core |    Core |  70.222 us |  1.3893 us |  1.3645 us |  68.482 us |  73.643 us |  14,240.5 |
 |     BenchmarkSimpleQueryParsed | Core |    Core |  31.415 us |  0.3700 us |  0.3090 us |  30.959 us |  31.832 us |  31,831.7 |
 |    BenchmarkSimpleQueryPlanned | Core |    Core |  25.489 us |  0.5543 us |  0.7010 us |  24.734 us |  27.211 us |  39,233.0 |
 |     BenchmarkFlatQueryUnparsed | Core |    Core | 710.705 us | 14.0351 us | 26.3614 us | 677.630 us | 773.004 us |   1,407.1 |
 |       BenchmarkFlatQueryParsed | Core |    Core | 627.403 us |  5.0556 us |  4.7290 us | 620.780 us | 636.468 us |   1,593.9 |
 |      BenchmarkFlatQueryPlanned | Core |    Core | 616.673 us |  6.0117 us |  5.3292 us | 610.028 us | 626.901 us |   1,621.6 |
 |   BenchmarkNestedQueryUnparsed | Core |    Core | 150.129 us |  1.1027 us |  1.0314 us | 148.794 us | 152.328 us |   6,660.9 |
 |     BenchmarkNestedQueryParsed | Core |    Core |  15.417 us |  0.1597 us |  0.1416 us |  15.146 us |  15.616 us |  64,862.4 |
 |    BenchmarkNestedQueryPlanned | Core |    Core |   4.044 us |  0.0870 us |  0.1275 us |   3.909 us |   4.389 us | 247,302.5 |
 | BenchmarkFilteredQueryUnparsed | Core |    Core | 183.466 us |  3.7612 us |  5.3942 us | 178.835 us | 197.409 us |   5,450.6 |
 |   BenchmarkFilteredQueryParsed | Core |    Core |  92.881 us |  0.6615 us |  0.5864 us |  91.973 us |  94.113 us |  10,766.5 |
 |  BenchmarkFilteredQueryPlanned | Core |    Core |  79.968 us |  0.7689 us |  0.7192 us |  79.020 us |  81.899 us |  12,505.0 |
 |   BenchmarkSimpleQueryUnparsed | Mono |    Mono |  72.967 us |  0.4540 us |  0.4024 us |  72.159 us |  73.640 us |  13,704.8 |
 |     BenchmarkSimpleQueryParsed | Mono |    Mono |  43.347 us |  0.3269 us |  0.3058 us |  42.828 us |  43.859 us |  23,069.7 |
 |    BenchmarkSimpleQueryPlanned | Mono |    Mono |  36.891 us |  0.7255 us |  0.9686 us |  35.831 us |  38.821 us |  27,106.6 |
 |     BenchmarkFlatQueryUnparsed | Mono |    Mono | 825.092 us |  7.6628 us |  7.1678 us | 815.492 us | 838.577 us |   1,212.0 |
 |       BenchmarkFlatQueryParsed | Mono |    Mono | 799.586 us | 19.6811 us | 21.0586 us | 782.516 us | 857.978 us |   1,250.6 |
 |      BenchmarkFlatQueryPlanned | Mono |    Mono | 781.850 us |  2.9210 us |  2.4392 us | 776.687 us | 786.387 us |   1,279.0 |
 |   BenchmarkNestedQueryUnparsed | Mono |    Mono | 135.812 us |  2.6401 us |  3.1429 us | 132.374 us | 143.884 us |   7,363.1 |
 |     BenchmarkNestedQueryParsed | Mono |    Mono |  21.007 us |  0.2646 us |  0.2475 us |  20.624 us |  21.473 us |  47,602.3 |
 |    BenchmarkNestedQueryPlanned | Mono |    Mono |   5.221 us |  0.0228 us |  0.0213 us |   5.187 us |   5.254 us | 191,542.0 |
 | BenchmarkFilteredQueryUnparsed | Mono |    Mono | 211.628 us |  0.8910 us |  0.7898 us | 210.571 us | 213.310 us |   4,725.3 |
 |   BenchmarkFilteredQueryParsed | Mono |    Mono | 140.005 us |  1.2816 us |  1.0006 us | 138.854 us | 142.521 us |   7,142.6 |
 |  BenchmarkFilteredQueryPlanned | Mono |    Mono | 128.832 us |  1.4877 us |  1.2423 us | 127.452 us | 131.460 us |   7,762.0 |
