# mimalloc - Haskell Bindings (Experimental)

⚠️ **EXPERIMENTAL**: This library contains a **modified version of mimalloc** that has been adapted to work with GHC's green thread model. While thoroughly tested, this is an experimental approach. Use with caution in production.

## Why Use This Library?

### Performance That Plays Nice With GHC

If you're building high-performance Haskell applications with significant memory allocation, you might want mimalloc because:

**🚀 Speed**: mimalloc is consistently [one of the fastest allocators](https://github.com/microsoft/mimalloc#performance) across a wide range of benchmarks, often outperforming system malloc by 20-30% in multi-threaded workloads.

**🧵 Green Thread Safe**: Unlike most malloc replacements, this version is specifically designed for GHC's green threads. Your threads can migrate between OS threads without memory corruption or crashes.

**🔒 Low Contention**: Capability-based heaps mean threads running in parallel on different cores have their own independent heaps - no fighting over locks.

**💾 Low Fragmentation**: mimalloc's design minimizes memory fragmentation, keeping your long-running services lean.

**📊 Built-in Statistics**: Query allocation statistics, RSS, and performance metrics without external tools - perfect for monitoring production systems.

**🎯 GC Integration**: ForeignPtr-based memory management integrates naturally with Haskell's GC. No manual free(), just let the garbage collector handle it.

### When You Might Want This

- Building a high-throughput web server or API
- Processing large amounts of data in parallel
- Long-running services where fragmentation matters
- Applications with heavy `ByteString` or `Vector` usage
- When you're already tuning allocation performance with `-N` flags

### When You Might Not

- Simple scripts or short-lived programs (overhead not worth it)
- Single-threaded applications (less benefit)
- Applications that barely allocate (no problem to solve)
- When you need rock-solid stability over performance (this is experimental!)

## About This Library

This package provides Haskell bindings to the [mimalloc](https://github.com/microsoft/mimalloc) allocator, with **critical modifications** to make it work correctly with GHC's runtime system.

### What Makes This Different?

**Standard mimalloc** uses OS thread-local storage (TLS) to maintain per-thread heaps. This doesn't work correctly with GHC because:
- GHC uses green threads (lightweight threads) that can migrate between OS threads
- A Haskell thread allocated on one OS thread might run on a different OS thread later
- This breaks mimalloc's thread-local heap assumptions

**This modified version** replaces thread-local storage with **capability-based heaps**:
- Each GHC capability (virtual processor, controlled by `-N` RTS flag) gets its own heap
- Multiple green threads on the same capability share that capability's heap
- Thread migration is safe because heaps are tied to capabilities, not OS threads
- Works correctly with `forkIO` - no need for `forkOS`

### Modifications to mimalloc Source

The C source code in `cbits/` has been modified:
- Replaced `mi_decl_thread` (TLS) with a capability-indexed array `_mi_heaps_by_capability[]`
- Modified heap lookup to use GHC capability number instead of OS thread ID
- Added integration layer (`cbits/ghc-rts-integration.c/h`) to bridge Haskell and C
- Compile-time flag `MI_USE_GHC_CAPABILITIES` controls these modifications

These changes maintain mimalloc's performance characteristics while ensuring correctness with GHC's green threads.

## Features

- **Capability-aware allocation**: Heaps scale with parallelism (`-N`), not thread count
- **Green thread safe**: Works correctly with `forkIO` and thread migration
- **GC integration**: ForeignPtr-based memory management with automatic cleanup
- **Multiple interfaces**: 
  - High-level `ForeignPtr` API (recommended)
  - Bracketed allocation (`withMalloc`)
  - Direct C FFI for advanced use
- **Data structure integration**: ByteString and Vector allocators
- **Statistics and introspection**: Query memory usage and performance metrics

## Installation

```bash
# Add to your package.yaml or .cabal file
dependencies:
  - mimalloc
```

## Quick Start

```haskell
import Mimalloc

main :: IO ()
main = do
  -- Allocate and use memory (ForeignPtr-based, GC-managed)
  ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
  withForeignPtr ptr $ \p -> poke p 42
  
  -- Or use bracket-style allocation
  result <- withMalloc $ \p -> do
    poke p (100 :: Int)
    peek p
  print result
```

## Thread Safety and Capabilities

All allocations are automatically routed to the appropriate capability's heap:

```haskell
import Control.Concurrent.Async
import Mimalloc

-- Allocate from multiple capabilities concurrently
-- Each capability uses its own heap (no contention)
main = forConcurrently [1..100] $ \i -> do
  ptr <- mallocForeignPtr :: IO (ForeignPtr Int)
  withForeignPtr ptr $ \p -> poke p i
```

## Documentation

See the module documentation for detailed information:
- `Mimalloc` - Main interface and capability explanation
- `Mimalloc.Capability` - Capability management and introspection  
- `Mimalloc.Heap` - Custom heap management (advanced)
- `Mimalloc.ByteString` - ByteString integration
- `Mimalloc.Vector` - Vector integration
- `Mimalloc.Stats` - Statistics and monitoring

## Performance

The capability-based approach maintains mimalloc's performance characteristics:
- **No lock contention**: Each capability has its own independent heap
- **Good cache locality**: Threads on the same capability share heap structures
- **Scales with parallelism**: Use `-N` to control number of heaps (typically set to core count)

## Limitations and Caveats

1. **Custom heaps are capability-local**: If you create a custom heap with `mi_heap_new`, it can only be used for allocation on the same capability where it was created
2. **Dynamic capability changes**: If you change capabilities at runtime with `setNumCapabilities`, call `reinitializeCapabilities` 
3. **Not a drop-in replacement**: Because of the modifications, this isn't identical to standard mimalloc behavior

## Contributing

This is experimental software. Bug reports, testing, and feedback are very welcome!

## License

- Haskell bindings: BSD-3-Clause (see LICENSE)
- mimalloc (including modifications): MIT License (see cbits/LICENSE)

## Credits

- Original mimalloc by Microsoft Research: https://github.com/microsoft/mimalloc
- GHC modifications and Haskell bindings by Ian Duncan
