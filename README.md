# 211cc
A small C compiler, named 211cc. This compiler generated x86_64 code as stack machine emulated.

## Build

Run make to build:

```
$ make
```

This compiler can self hosting:

```
$ make 211cc-stage2
```

## Test
211cc comes with tests. To run the tests, give "test" as an argument:

```
$ make test
```

For self hosting test:

```
$ make test-stage2
```

You can check if the self hosting compiler is made correctly by comparing stage 2 and stage 3 compilers using diff command:  

```
$ make test-stage3
```

## Acknowlege
211cc is inspired by [9cc](https://github.com/rui314/9cc)
