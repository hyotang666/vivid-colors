# VIVID-COLORS.CONTENT 0.0.0
## What is this?
The module of the printing appointments and its fulfills for vivid-colors.

## Usage

## From developer

### Product's goal

### License

### Developed with

### Tested with
* SBCL/2.1.7
* CCL/1.12.1
* CLISP/2.49
* ECL/21.2.1 ; See [vivid-colors.queue.](../queue/README.md)
* Allegro/10.1
* CMUCL/21D ; Failed.
* ABCL/1.8.0 ; See [vivid-colors.queue.](../queue/README.md)

## Known issue
### CMUCL
CMUCL violates ansi.

`&AUX` does not affect slot initialization.

```lisp
(defstruct (object (:constructor create
                      (&key content &aux
                       (firstp (print (not content))))))
    (firstp t))

(create :content t)
NIL
#S(OBJECT :FIRSTP T)
```
## Installation

