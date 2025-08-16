---
title: Event
---

Subscriptions to events.

```grain
import Event from "sys/event"
```

## Types

Type declarations included in the Event module.

### Event.**Userdata**

```grain
type Userdata = Int64
```

User-provided value that may be attached to objects that is retained when
extracted from the implementation.

### Event.**EventType**

```grain
enum EventType {
  Clock,
  FdRead,
  FdWrite,
}
```

Type of a subscription to an event or its occurrence.

### Event.**EventRWFlags**

```grain
enum EventRWFlags {
  FdReadWriteHangup,
}
```

The state of the file descriptor subscribed to with
`EventType.FdRead` or `EventType.FdWrite`.

### Event.**EventFdReadWrite**

```grain
record EventFdReadWrite {
  nbytes: Int64,
  flags: List<EventRWFlags>,
}
```

The contents of an `event` when type is `EventType.FdRead` or
`EventType.FdWrite`.

### Event.**Event**

```grain
record Event {
  userdata: Userdata,
  error: Number,
  eventType: EventType,
  fdReadWrite: Option<EventFdReadWrite>,
}
```

An event that occurred.

### Event.**SubClockFlags**

```grain
enum SubClockFlags {
  SubscriptionClockAbstime,
}
```

Flags determining how to interpret the timestamp provided in
`SubscriptionClock.timeout`

### Event.**SubscriptionClock**

```grain
record SubscriptionClock {
  clock: Time.Clock,
  timeout: Int64,
  precision: Int64,
  flags: List<SubClockFlags>,
}
```

The contents of a `Subscription` when type is `EventType.Clock`.

### Event.**SubscriptionFdReadwrite**

```grain
record SubscriptionFdReadwrite {
  fileDescriptor: File.FileDescriptor,
}
```

The contents of a `subscription` when type is type is
`EventType.FdRead` or `EventType.FdWrite`.

### Event.**SubscriptionContents**

```grain
enum SubscriptionContents {
  SubscriptionClock(SubscriptionClock),
  SubscriptionFdRead(SubscriptionFdReadwrite),
  SubscriptionFdWrite(SubscriptionFdReadwrite),
}
```

### Event.**Subscription**

```grain
record Subscription {
  userdata: Userdata,
  contents: SubscriptionContents,
}
```

Subscription to an event.

## Values

Functions and constants included in the Event module.

### Event.**pollOneoff**

```grain
pollOneoff :
  (subscriptions: List<Subscription>) => Result<List<Event>, Exception>
```

Concurrently poll for the occurrence of a set of events.

Parameters:

|param|type|description|
|-----|----|-----------|
|`subscriptions`|`List<Subscription>`|The events to which to subscribe.|

Returns:

|type|description|
|----|-----------|
|`Result<List<Event>, Exception>`|`Ok(events)` of the events which occured if successful or `Err(Exception)` otherwise|

