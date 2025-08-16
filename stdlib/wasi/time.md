---
title: Time
---

Access to system clocks.

```grain
from "wasi/time" include Time
```

## Types

Type declarations included in the Time module.

### Time.**Clock**

```grain
enum Clock {
  Realtime,
  Monotonic,
  ProcessCpuTime,
  ThreadCpuTime,
}
```

Variants:

```grain
Realtime
```

The clock measuring real time. Time value zero corresponds with
1970-01-01T00:00:00Z.

```grain
Monotonic
```

The store-wide monotonic clock, which is defined as a clock measuring
real time, whose value cannot be adjusted and which cannot have negative
clock jumps. The epoch of this clock is undefined. The absolute time
value of this clock therefore has no meaning.

```grain
ProcessCpuTime
```

The CPU-time clock associated with the current process.

```grain
ThreadCpuTime
```

The CPU-time clock associated with the current thread.

## Values

Functions and constants included in the Time module.

### Time.**realTime**

```grain
realTime: () => Result<Int64, Exception>
```

Get the current time, in nanoseconds.
Time value 0 corresponds with 1970-01-01T00:00:00Z.

Returns:

| type                       | description                                                                |
| -------------------------- | -------------------------------------------------------------------------- |
| `Result<Int64, Exception>` | `Ok(time)` of the current time if successful or `Err(exception)` otherwise |

### Time.**monotonicTime**

```grain
monotonicTime: () => Result<Int64, Exception>
```

Get the time of the system's high-resolution clock, in nanoseconds.
This system clock cannot be adjusted and cannot have negative time jumps.
The epoch of this clock is undefined, and thus time value 0 is meaningless.
Useful for calculation of precise time intervals.

Returns:

| type                       | description                                                                |
| -------------------------- | -------------------------------------------------------------------------- |
| `Result<Int64, Exception>` | `Ok(time)` of the current time if successful or `Err(exception)` otherwise |

### Time.**processCpuTime**

```grain
processCpuTime: () => Result<Int64, Exception>
```

Get the number of nanoseconds elapsed since the process began.

Returns:

| type                       | description                                                                          |
| -------------------------- | ------------------------------------------------------------------------------------ |
| `Result<Int64, Exception>` | `Ok(elapsed)` of the elapsed nanoseconds if successful or `Err(exception)` otherwise |

### Time.**threadCpuTime**

```grain
threadCpuTime: () => Result<Int64, Exception>
```

Get the number of nanoseconds elapsed since the thread began.

Returns:

| type                       | description                                                                          |
| -------------------------- | ------------------------------------------------------------------------------------ |
| `Result<Int64, Exception>` | `Ok(elapsed)` of the elapsed nanoseconds if successful or `Err(exception)` otherwise |

