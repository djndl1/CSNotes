- (C# 7.0) throw expression

```csharp
string[] digitTexts =
          { "zero", "one", "two", "three", "four",
              "five", "six", "seven", "eight", "nine" };

      int result = Array.IndexOf(
          digitTexts,
            // Leveraging C# 2.0’s null coelesce operator
          (textDigit??
            // Leveraging C# 7.0’s throw expression
            throw new ArgumentNullException(nameof(textDigit))
          ).ToLower());
```


- (C# 6.0) `when` conditional catch

```csharp
 catch(Win32Exception exception)
          when(exception.NativeErrorCode == 42)
      {
          // Handle Win32Exception where
          // ErrorCode is 42
      }
```

general catch block can used to catch unmanaged exception object.

- To throw existing exceptions without replacing stack information, use `System.Runtime.ExceptionServices.ExceptionDispatchInfo`'s `Capture` and `Throw`.

- If a new exception should be thrown in a catch block, set its `InnerException` to preserve the original exception's info, unless the original exception contains some private data that shouldn't be exposed.

- Try making custom exceptions serializable as they might be serialized in certain distributed communication technologies.
