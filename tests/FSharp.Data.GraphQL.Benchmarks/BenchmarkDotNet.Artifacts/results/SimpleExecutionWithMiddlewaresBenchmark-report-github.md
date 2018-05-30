``` ini

BenchmarkDotNet=v0.10.9, OS=Windows 10.0.17134
Processor=Intel Core i7-2600 CPU 3.40GHz (Sandy Bridge), ProcessorCount=8
.NET Core SDK=2.1.201
  [Host] : .NET Core 2.0.7 (Framework 4.6.26328.01), 64bit RyuJIT DEBUG
  Core   : .NET Core 2.0.7 (Framework 4.6.26328.01), 64bit RyuJIT
  Mono   : Mono 5.12.0 (Visual Studio), 32bit 


```
 |                         Method |  Job | Runtime |      Mean |      Error |     StdDev |    Median |       Min |         Max |     Op/s |
 |------------------------------- |----- |-------- |----------:|-----------:|-----------:|----------:|----------:|------------:|---------:|
 |   BenchmarkSimpleQueryUnparsed | Core |    Core |  76.60 us |  1.5280 us |  2.5530 us |  76.00 us |  73.30 us |    82.32 us | 13,054.1 |
 |     BenchmarkSimpleQueryParsed | Core |    Core |  33.66 us |  0.6531 us |  0.8021 us |  33.66 us |  32.30 us |    35.45 us | 29,710.7 |
 |    BenchmarkSimpleQueryPlanned | Core |    Core |  30.73 us |  0.7795 us |  2.2491 us |  30.52 us |  26.82 us |    36.21 us | 32,541.3 |
 |     BenchmarkFlatQueryUnparsed | Core |    Core | 900.58 us | 18.3552 us | 52.3684 us | 896.38 us | 792.65 us | 1,032.14 us |  1,110.4 |
 |       BenchmarkFlatQueryParsed | Core |    Core | 854.29 us | 30.0516 us | 84.2679 us | 836.55 us | 743.60 us | 1,088.82 us |  1,170.6 |
 |      BenchmarkFlatQueryPlanned | Core |    Core | 827.91 us | 20.9742 us | 58.8140 us | 821.74 us | 717.97 us | 1,000.19 us |  1,207.9 |
 |   BenchmarkNestedQueryUnparsed | Core |    Core | 680.94 us | 13.8163 us | 40.7376 us | 680.06 us | 609.97 us |   787.93 us |  1,468.6 |
 |     BenchmarkNestedQueryParsed | Core |    Core | 483.91 us |  9.6487 us | 25.9207 us | 477.41 us | 441.50 us |   567.40 us |  2,066.5 |
 |    BenchmarkNestedQueryPlanned | Core |    Core | 487.98 us | 17.6577 us | 52.0640 us | 467.93 us | 417.48 us |   630.07 us |  2,049.2 |
 | BenchmarkFilteredQueryUnparsed | Core |    Core | 242.80 us |  6.3875 us | 18.7333 us | 238.78 us | 210.15 us |   289.09 us |  4,118.6 |
 |   BenchmarkFilteredQueryParsed | Core |    Core | 115.17 us |  1.9730 us |  1.8456 us | 115.06 us | 112.52 us |   119.65 us |  8,682.8 |
 |  BenchmarkFilteredQueryPlanned | Core |    Core |  99.48 us |  1.9300 us |  2.8887 us |  98.97 us |  95.03 us |   106.69 us | 10,052.6 |
 |   BenchmarkSimpleQueryUnparsed | Mono |    Mono |  83.16 us |  1.6436 us |  2.1371 us |  83.52 us |  78.96 us |    87.16 us | 12,025.2 |
 |     BenchmarkSimpleQueryParsed | Mono |    Mono |  50.50 us |  0.9728 us |  1.3316 us |  50.40 us |  47.93 us |    53.47 us | 19,803.4 |
 |    BenchmarkSimpleQueryPlanned | Mono |    Mono |  43.09 us |  0.8555 us |  1.5206 us |  43.12 us |  39.74 us |    46.78 us | 23,206.5 |
 |     BenchmarkFlatQueryUnparsed | Mono |    Mono | 713.98 us | 14.2067 us | 19.9158 us | 714.69 us | 676.67 us |   753.52 us |  1,400.6 |
 |       BenchmarkFlatQueryParsed | Mono |    Mono | 678.13 us | 13.3445 us | 24.0629 us | 678.28 us | 629.88 us |   725.78 us |  1,474.6 |
 |      BenchmarkFlatQueryPlanned | Mono |    Mono | 652.01 us | 12.9937 us | 16.8954 us | 654.50 us | 616.83 us |   679.44 us |  1,533.7 |
 |   BenchmarkNestedQueryUnparsed | Mono |    Mono | 860.99 us | 17.1646 us | 21.0797 us | 862.83 us | 811.27 us |   896.27 us |  1,161.5 |
 |     BenchmarkNestedQueryParsed | Mono |    Mono | 729.72 us | 14.5426 us | 20.8565 us | 730.02 us | 683.94 us |   774.08 us |  1,370.4 |
 |    BenchmarkNestedQueryPlanned | Mono |    Mono | 704.88 us | 14.0843 us | 33.1985 us | 700.36 us | 646.89 us |   812.02 us |  1,418.7 |
 | BenchmarkFilteredQueryUnparsed | Mono |    Mono | 259.00 us |  5.0930 us |  6.7989 us | 259.00 us | 246.82 us |   273.54 us |  3,861.1 |
 |   BenchmarkFilteredQueryParsed | Mono |    Mono | 172.18 us |  3.5925 us |  7.2570 us | 170.64 us | 159.66 us |   192.16 us |  5,807.7 |
 |  BenchmarkFilteredQueryPlanned | Mono |    Mono | 158.15 us |  3.0537 us |  4.4760 us | 157.62 us | 149.48 us |   168.22 us |  6,323.0 |
