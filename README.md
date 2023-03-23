# reed_solomon_vb
Reed-Solomon encoder and decoder. 

Also realization of arithmetics in **Galois fields**.

## Usage examples

- Encoding:

```
Dim rs As New ReedSolomon(255, 246, 8, 285)
Dim messageToEncode As Integer() = {&H68, &H65, &H6C, &H6C, &H6F, &H20, &H77, &H6F, &H72, &H6C, &H64} 'ASCII "hello world"
Dim encodedMessage As Integer() = rs.Encode(messageToEncode)
```

- Decoding:

```
Dim decodedMessage as Integer() = rs.Decode(encodedMessage)
```
