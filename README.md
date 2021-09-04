# VIVID-COLORS 2.0.0
## What is this?
Colored object printer.

## Alternatives and differences.
Please tell me if exists.

## Usage

```lisp
* (vprint '(:keyword :keyword
            :character #\A
	    :string "string"
	    :number 1/3
	    :pathname #P"foo/bar"))
```
![image of the command result.](img/vivid-colors.jpg)

## From developer

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with
* SBCL/2.1.7
* CCL/1.12.1
* ECL/21.2.1 ; Failed.
* CLISP/2.49

## Known issues.
### ECL
Currently we stop to support ECL due to its `SUBTYPEP` behavior.

```lisp
#+ecl
(subtypep '(cons (member quote)) 'list)
NIL
NIL

#-ecl
(subtypep '(cons (member quote)) 'list)
T
T
```

## Installation

