``` ini

BenchmarkDotNet=v0.11.3, OS=Windows 10.0.17134.523 (1803/April2018Update/Redstone4)
Intel Core i7-7700HQ CPU 2.80GHz (Kaby Lake), 1 CPU, 8 logical and 4 physical cores
.NET Core SDK=2.2.100
  [Host] : .NET Core 2.2.0 (CoreCLR 4.6.27110.04, CoreFX 4.6.27110.04), 64bit RyuJIT DEBUG
  Core   : .NET Core 2.2.0 (CoreCLR 4.6.27110.04, CoreFX 4.6.27110.04), 64bit RyuJIT

Job=Core  Runtime=Core  

```
|          Method |         Mean |       Error |      StdDev |        Median |           Min |          Max |     Op/s | Gen 0/1k Op | Gen 1/1k Op | Gen 2/1k Op | Allocated Memory/Op |
|---------------- |-------------:|------------:|------------:|--------------:|--------------:|-------------:|---------:|------------:|------------:|------------:|--------------------:|
|     SimpleQuery |     1.692 ms |   0.5130 ms |   1.5047 ms |     0.8638 ms |     0.5520 ms |     5.489 ms | 590.9996 |           - |           - |           - |            22.38 KB |
|       FlatQuery |     1.593 ms |   0.1143 ms |   0.3149 ms |     1.5009 ms |     1.1316 ms |     2.582 ms | 627.8552 |           - |           - |           - |            38.01 KB |
|   LongListQuery | 1,394.840 ms |  27.7700 ms |  70.1784 ms | 1,383.1667 ms | 1,296.3221 ms | 1,584.532 ms |   0.7169 |  75000.0000 |  19000.0000 |   2000.0000 |            39.12 KB |
| LongStreamQuery | 6,247.535 ms | 126.1488 ms | 224.2294 ms | 6,217.0907 ms | 5,910.6571 ms | 6,733.767 ms |   0.1601 | 142000.0000 |  38000.0000 |   3000.0000 |          2633.83 KB |
