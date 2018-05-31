``` ini

BenchmarkDotNet=v0.10.9, OS=Windows 10.0.17134
Processor=Intel Core i7-2600 CPU 3.40GHz (Sandy Bridge), ProcessorCount=8
.NET Core SDK=2.1.201
  [Host] : .NET Core 2.0.7 (Framework 4.6.26328.01), 64bit RyuJIT DEBUG
  Core   : .NET Core 2.0.7 (Framework 4.6.26328.01), 64bit RyuJIT
  Mono   : Mono 5.12.0 (Visual Studio), 32bit 


```
 |                       Method |  Job | Runtime |      Mean |      Error |     StdDev |    Median |       Min |       Max |     Op/s |
 |----------------------------- |----- |-------- |----------:|-----------:|-----------:|----------:|----------:|----------:|---------:|
 | BenchmarkSimpleQueryUnparsed | Core |    Core |  70.61 us |  1.2678 us |  1.0587 us |  70.58 us |  69.43 us |  72.92 us | 14,161.4 |
 |   BenchmarkSimpleQueryParsed | Core |    Core |  29.29 us |  0.1040 us |  0.0922 us |  29.28 us |  29.10 us |  29.42 us | 34,144.8 |
 |  BenchmarkSimpleQueryPlanned | Core |    Core |  23.84 us |  0.1595 us |  0.1414 us |  23.83 us |  23.62 us |  24.09 us | 41,938.3 |
 |   BenchmarkFlatQueryUnparsed | Core |    Core | 677.40 us |  9.0777 us |  8.4913 us | 673.82 us | 668.15 us | 696.07 us |  1,476.2 |
 |     BenchmarkFlatQueryParsed | Core |    Core | 633.78 us |  9.8231 us |  8.7079 us | 635.11 us | 617.74 us | 647.35 us |  1,577.8 |
 |    BenchmarkFlatQueryPlanned | Core |    Core | 629.42 us | 12.5782 us | 18.8264 us | 623.48 us | 599.89 us | 660.47 us |  1,588.8 |
 | BenchmarkNestedQueryUnparsed | Core |    Core | 579.11 us | 18.1092 us | 18.5968 us | 574.11 us | 558.88 us | 616.71 us |  1,726.8 |
 |   BenchmarkNestedQueryParsed | Core |    Core | 360.04 us |  3.4169 us |  3.0290 us | 358.93 us | 354.63 us | 366.52 us |  2,777.4 |
 |  BenchmarkNestedQueryPlanned | Core |    Core | 347.80 us |  4.9268 us |  4.6086 us | 345.91 us | 342.66 us | 357.99 us |  2,875.2 |
 | BenchmarkSimpleQueryUnparsed | Mono |    Mono |  69.27 us |  0.4717 us |  0.4413 us |  69.17 us |  68.74 us |  70.29 us | 14,436.2 |
 |   BenchmarkSimpleQueryParsed | Mono |    Mono |  38.82 us |  0.2182 us |  0.1935 us |  38.78 us |  38.55 us |  39.15 us | 25,758.7 |
 |  BenchmarkSimpleQueryPlanned | Mono |    Mono |  34.47 us |  0.2483 us |  0.2073 us |  34.49 us |  34.18 us |  34.94 us | 29,011.3 |
 |   BenchmarkFlatQueryUnparsed | Mono |    Mono | 575.04 us |  6.9764 us |  6.1844 us | 573.79 us | 568.51 us | 589.01 us |  1,739.0 |
 |     BenchmarkFlatQueryParsed | Mono |    Mono | 529.85 us |  2.9890 us |  2.6497 us | 529.42 us | 525.88 us | 535.42 us |  1,887.3 |
 |    BenchmarkFlatQueryPlanned | Mono |    Mono | 517.76 us |  3.4877 us |  3.0918 us | 517.31 us | 513.31 us | 524.77 us |  1,931.4 |
 | BenchmarkNestedQueryUnparsed | Mono |    Mono | 690.36 us |  4.0268 us |  3.7666 us | 690.33 us | 684.63 us | 698.87 us |  1,448.5 |
 |   BenchmarkNestedQueryParsed | Mono |    Mono | 611.72 us | 22.3732 us | 61.6223 us | 583.89 us | 562.85 us | 812.03 us |  1,634.7 |
 |  BenchmarkNestedQueryPlanned | Mono |    Mono | 557.39 us |  4.3306 us |  4.0509 us | 557.47 us | 550.87 us | 566.38 us |  1,794.1 |
