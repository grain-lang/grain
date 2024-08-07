---
title: Mimetypes
---

MIMETypes and file extensions database utility.

<details disabled>
<summary tabindex="-1">Added in <code>0.6.4</code></summary>
No other changes yet.
</details>

```grain
from "mimetypes" include Mimetypes
```

```grain
Mimetypes.getMimetype("mp3")
```

```grain
Mimetypes.getMimetype("dOcX")
```

```grain
Mimetypes.getMimetype("unexistentFormat")
```

```grain
Mimetypes.getExtension("application/json")
```

```grain
Mimetypes.getExtension("audio/mp4")
```

```grain
Mimetypes.getExtension("application/vnd.ms-excel")
```

## Values

Functions and constants included in the Mimetypes module.

### Mimetypes.**getMimetype**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.4</code></summary>
No other changes yet.
</details>

```grain
getMimetype : (extension: String) => String
```

Gets the MIMEType that matches the file extension.

Parameters:

|param|type|description|
|-----|----|-----------|
|`extension`|`String`|The file extension|

Returns:

|type|description|
|----|-----------|
|`String`|The matching MIMEType or text/plain if not|

Examples:

```grain
Mimetypes.getMimetype("mp4") == "video/mp4"
```

```grain
Mimetypes.getMimetype("doesnotexist") == "text/plain"
```

### Mimetypes.**getExtension**

<details disabled>
<summary tabindex="-1">Added in <code>0.6.4</code></summary>
No other changes yet.
</details>

```grain
getExtension : (mimetype: String) => String
```

Gets the file extension that matches the MIMEType.

Parameters:

|param|type|description|
|-----|----|-----------|
|`mimetype`|`String`|The MIMEType|

Returns:

|type|description|
|----|-----------|
|`String`|The matching file extension or txt if not|

Examples:

```grain
Mimetypes.getExtension("application/vnd.ms-excel") == "xls"
```

```grain
Mimetypes.getExtension("mimetype/thatdoesnotexist") == "txt"
```

