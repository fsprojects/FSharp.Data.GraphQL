``` ini

BenchmarkDotNet=v0.11.3, OS=Windows 10.0.17134.523 (1803/April2018Update/Redstone4)
Intel Core i7-7700HQ CPU 2.80GHz (Kaby Lake), 1 CPU, 8 logical and 4 physical cores
.NET Core SDK=2.2.100
  [Host] : .NET Core 2.2.0 (CoreCLR 4.6.27110.04, CoreFX 4.6.27110.04), 64bit RyuJIT DEBUG
  Core   : .NET Core 2.2.0 (CoreCLR 4.6.27110.04, CoreFX 4.6.27110.04), 64bit RyuJIT

Job=Core  Runtime=Core  

```
|          Method |         Mean |       Error |      StdDev |       Median |           Min |          Max |     Op/s | Gen 0/1k Op | Gen 1/1k Op | Gen 2/1k Op | Allocated Memory/Op |
|---------------- |-------------:|------------:|------------:|-------------:|--------------:|-------------:|---------:|------------:|------------:|------------:|--------------------:|
|     SimpleQuery |     2.238 ms |   0.7627 ms |   2.2248 ms |     1.032 ms |     0.7647 ms |     8.564 ms | 446.7835 |           - |           - |           - |            22.38 KB |
|       FlatQuery |     1.433 ms |   0.0954 ms |   0.2691 ms |     1.355 ms |     1.0211 ms |     2.175 ms | 697.8076 |           - |           - |           - |            38.01 KB |
|   LongListQuery | 1,590.288 ms |  31.4637 ms |  72.9220 ms | 1,583.767 ms | 1,438.3572 ms | 1,825.454 ms |   0.6288 |  76000.0000 |  19000.0000 |   3000.0000 |            39.12 KB |
| LongStreamQuery | 6,299.963 ms | 125.7192 ms | 275.9567 ms | 6,234.619 ms | 5,924.5985 ms | 6,991.723 ms |   0.1587 | 150000.0000 |  39000.0000 |   4000.0000 |             50.5 KB |
