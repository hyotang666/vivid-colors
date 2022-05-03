# VIVID-COLORS.DISPATCH 0.0.0
## What is this?
The module to printer dispatching feature for vivid-colors.

## Usage

## From developer

### Product's goal

### License

### Developed with

### Tested with
* SBCL/2.1.7
* CCL/1.12.1
* CLISP/2.49
* ECL/21.2.1 ; See below.
* Allegro/10.1
* CMUCL/21D
* ABCL/1.9.0 ; See below.

### Known issue.
#### millet:type-specifier-p.
Vivid-colors.dispatch depends on `millet:type-specifier-p`.
It is used for type checking.
ECL and ABCL have an issue with `type-specifier-p`.
So in the worst case, type checking will have false positives.
If every parameter is fine, vivid-colors.queue works fine.

## Installation

