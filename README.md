# mds-converter

Converter for our own `.mds` format file

## Common features

- Headings (from `#` to `######`)
- Text decorations (*italics*, **bold** ***and*** so ~~no~~ <u>on</u>)
- Horizontal dividers
- Lists of any type
- Checkboxes
- Links and hyperlinks
- Code blocks (without syntax highlighting)
- Tables
- Images

## How to run

If you have stack installed and GHC configured you can simply build:

```shell
mkdir build
mkdir runnable
make build
```

and run:

```shell
./runnable/parser <input file> <output file>
```

or if you want to use this parser in stream mode, use:

```shell
./runnable/parser -c
```

If you dont have stack installed or GHC configured, use [build](./build.sh) and [run](./run.sh) scripts

## Examples

You can see an examples in [this directory](./test/)

For the comparing i used `Markdown All in One` VS Code extension to render the same file, it renders it with not bad styles, that are provided [here](./test/styles.html)

You can combine file with styles and [output file](./test/output.html) to see some beauteful example render.

## Perfomance tests

Test data (Markdown): ~5000 lines

Output data (HTML): ~10000 lines

Total execution time: 0.0140266s (built-in-code measurements)

Total execution time: 0.0277915s (100 runs written in python)
