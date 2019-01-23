``` ini

BenchmarkDotNet=v0.11.3, OS=Windows 10.0.17134.523 (1803/April2018Update/Redstone4)
Intel Core i7-7700HQ CPU 2.80GHz (Kaby Lake), 1 CPU, 8 logical and 4 physical cores
.NET Core SDK=2.2.100
  [Host] : .NET Core 2.2.0 (CoreCLR 4.6.27110.04, CoreFX 4.6.27110.04), 64bit RyuJIT DEBUG
  Core   : .NET Core 2.2.0 (CoreCLR 4.6.27110.04, CoreFX 4.6.27110.04), 64bit RyuJIT

Job=Core  Runtime=Core  

```
|                            Method |      Mean |     Error |    StdDev |     Median |        Min |       Max |   Op/s | Gen 0/1k Op | Gen 1/1k Op | Gen 2/1k Op | Allocated Memory/Op |
|---------------------------------- |----------:|----------:|----------:|-----------:|-----------:|----------:|-------:|------------:|------------:|------------:|--------------------:|
|                       SingleMovie |  2.046 ms | 0.6497 ms | 1.9157 ms |  0.9003 ms |  0.5736 ms |  6.357 ms | 488.76 |           - |           - |           - |            24.11 KB |
|       SingleMovieSingleUserRating |  1.511 ms | 0.0719 ms | 0.1979 ms |  1.4612 ms |  1.1930 ms |  2.166 ms | 661.78 |           - |           - |           - |            41.68 KB |
|                   MovieTagsDirect | 32.058 ms | 0.7612 ms | 2.1346 ms | 31.5728 ms | 28.3333 ms | 37.816 ms |  31.19 |   1000.0000 |           - |           - |            48.18 KB |
|                 MovieTagsStreamed | 67.306 ms | 1.3853 ms | 4.0410 ms | 66.6163 ms | 59.7592 ms | 76.686 ms |  14.86 |   2000.0000 |   1000.0000 |           - |            83.96 KB |
|                 MovieTagsDeferred | 44.227 ms | 0.8798 ms | 2.5243 ms | 44.1868 ms | 38.9480 ms | 50.599 ms |  22.61 |   1000.0000 |           - |           - |             59.1 KB |
|     MovieTagsDirectAndLinksDirect | 31.890 ms | 0.6293 ms | 0.9798 ms | 31.9148 ms | 29.7126 ms | 33.809 ms |  31.36 |   1000.0000 |           - |           - |            60.31 KB |
|   MovieTagsStreamedAndLinksDirect | 67.420 ms | 1.4543 ms | 2.6957 ms | 66.7677 ms | 63.1727 ms | 74.908 ms |  14.83 |   2000.0000 |   1000.0000 |           - |             92.8 KB |
| MovieTagsStreamedAndLinksStreamed | 66.878 ms | 1.3958 ms | 2.8512 ms | 66.7963 ms | 61.6435 ms | 74.211 ms |  14.95 |   2000.0000 |   1000.0000 |           - |            94.13 KB |
| MovieTagsDeferredAndLinksDeferred | 45.175 ms | 0.9025 ms | 2.3458 ms | 45.2692 ms | 40.5602 ms | 51.800 ms |  22.14 |   1000.0000 |           - |           - |            72.16 KB |
| MovieTagsStreamedAndLinksDeferred | 68.978 ms | 1.5421 ms | 4.3747 ms | 67.9400 ms | 62.0436 ms | 80.226 ms |  14.50 |   2000.0000 |   1000.0000 |           - |            93.71 KB |
