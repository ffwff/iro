# 色の言語

A *color*ful programming language (a wip)

## Features

 * compiles to native code using cranelift
 * looks like ruby because thats what the cool languages do

## Examples

```ruby
extern def putchar(x: I32): I32

def print(s: &Substring): ISize =>
    mut i := 0is
    while i < s.len =>
        putchar(s[i])
        i += 1
    i

print("Hello World\n")
```

## License

MIT License