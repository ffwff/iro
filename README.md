# 色の言語

A *color*ful programming language (a wip)

## Features

 * compiles to native code using cranelift
 * looks like ruby because thats what the cool languages do

## Examples

```ruby
extern def print="print_i64"(n: I64): Nil

def fib(n): I64
    if n <= 1
        return 1i64
    end
    fib(n-1) + fib(n-2)
end

print(fib(46i64))
```

## License

MIT License