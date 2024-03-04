---
title: Process
---

Utilities for accessing functionality and information about the Grain program's process.

This includes things like accessing environment variables and sending signals.

```grain
from "wasi/process" include Process
```

## Types

Type declarations included in the Process module.

### Process.**Signal**

```grain
enum Signal {
  HUP,
  INT,
  QUIT,
  ILL,
  TRAP,
  ABRT,
  BUS,
  FPE,
  KILL,
  USR1,
  SEGV,
  USR2,
  PIPE,
  ALRM,
  TERM,
  CHLD,
  CONT,
  STOP,
  TSTP,
  TTIN,
  TTOU,
  URG,
  XCPU,
  XFSZ,
  VTALRM,
  PROF,
  WINCH,
  POLL,
  PWR,
  SYS,
}
```

Signals that can be sent to the host system.

Variants:

```grain
HUP
```

Hangup.

```grain
INT
```

Terminate interrupt signal.

```grain
QUIT
```

Terminal quit signal.

```grain
ILL
```

Illegal instruction.

```grain
TRAP
```

Trace/breakpoint trap.

```grain
ABRT
```

Process abort signal.

```grain
BUS
```

Access to an undefined portion of a memory object.

```grain
FPE
```

Erroneous arithmetic operation.

```grain
KILL
```

Kill.

```grain
USR1
```

User-defined signal 1.

```grain
SEGV
```

Invalid memory reference.

```grain
USR2
```

User-defined signal 2.

```grain
PIPE
```

Write on a pipe with no one to read it.

```grain
ALRM
```

Alarm clock.

```grain
TERM
```

Termination signal.

```grain
CHLD
```

Child process terminated, stopped, or continued.

```grain
CONT
```

Continue executing, if stopped.

```grain
STOP
```

Stop executing.

```grain
TSTP
```

Terminal stop signal.

```grain
TTIN
```

Background process attempting read.

```grain
TTOU
```

Background process attempting write.

```grain
URG
```

High bandwidth data is available at a socket.

```grain
XCPU
```

CPU time limit exceeded.

```grain
XFSZ
```

File size limit exceeded.

```grain
VTALRM
```

Virtual timer expired.

```grain
SYS
```

Bad system call.

## Values

Functions and constants included in the Process module.

### Process.**argv**

```grain
argv : () => Result<Array<String>, Exception>
```

Access command line arguments.

Returns:

|type|description|
|----|-----------|
|`Result<Array<String>, Exception>`|`Ok(args)` of an array containing positional string arguments to the process if successful or `Err(exception)` otherwise|

### Process.**env**

```grain
env : () => Result<Array<String>, Exception>
```

Access environment variables.

Returns:

|type|description|
|----|-----------|
|`Result<Array<String>, Exception>`|`Ok(vars)` of an array containing environment variables supplied to the process if successful or `Err(exception)` otherwise|

### Process.**exit**

```grain
exit : (code: Number) => Result<Void, Exception>
```

Terminate the process normally.

Parameters:

|param|type|description|
|-----|----|-----------|
|`code`|`Number`|The value to exit with. An exit code of 0 is considered normal, with other values having meaning depending on the platform|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Err(exception)` if unsuccessful. Will not actually return a value if successful, as the process has ended|

### Process.**sigRaise**

```grain
sigRaise : (signal: Signal) => Result<Void, Exception>
```

Send a signal to the process of the calling thread.

Parameters:

|param|type|description|
|-----|----|-----------|
|`signal`|`Signal`|The signal to send|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### Process.**schedYield**

```grain
schedYield : () => Result<Void, Exception>
```

Yield execution to the calling thread.

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

