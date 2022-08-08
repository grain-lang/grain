---
title: Process
---

Utilities for accessing functionality and information about the Grain program's process.

This includes things like accessing environment variables and sending signals.

```grain
import Process from "sys/process"
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

## Values

Functions and constants included in the Process module.

### Process.**argv**

```grain
argv : () -> Result<Array<String>, Exception>
```

Access command line arguments.

Returns:

|type|description|
|----|-----------|
|`Result<Array<String>, Exception>`|`Ok(args)` of an array containing positional string arguments to the process if successful or `Err(exception)` otherwise|

### Process.**env**

```grain
env : () -> Result<Array<String>, Exception>
```

Access environment variables.

Returns:

|type|description|
|----|-----------|
|`Result<Array<String>, Exception>`|`Ok(vars)` of an array containing environment variables supplied to the process if successful or `Err(exception)` otherwise|

### Process.**exit**

```grain
exit : Number -> Result<Void, Exception>
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
sigRaise : Signal -> Result<Void, Exception>
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
schedYield : () -> Result<Void, Exception>
```

Yield execution to the calling thread.

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

