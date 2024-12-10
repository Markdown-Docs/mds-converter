# mds-converter

Converter for our own `.mds` format file

## Common features

- Headings (from `#` to `######`)
- Text decorations (*italics*, **bold** ***and*** so ~~no~~ <u>on</u>)
- Horizontal dividers
- Quotes
- Lists of any type
- Checkboxes
- Links and hyperlinks
- Code blocks (without syntax highlighting)
- Tables

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
./runnable/parser -s
```

If you dont have stack installed or GHC configured, use [build](./build.sh) and [run](./run.sh) scripts
