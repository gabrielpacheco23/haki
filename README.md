# Haki ⚓

**Haki** is a fast, embeddable, and lightweight Lisp dialect written entirely in Rust. 

Inspired by Peter Norvig's elegant `lis.py` tutorial, Haki started as a simple "toy project" to explore compiler design and Virtual Machines, and has since evolved into a robust engine. It marries the elegant minimalism of Scheme with the pragmatic, data-driven features of modern scripting languages like Clojure and JavaScript. It is designed to be a powerful general-purpose scripting language and a lightweight embedded rules engine.

## ✨ Key Features

* **Custom Garbage Collector:** A built-in Mark-and-Sweep GC utilizing a flat memory Arena. Replaces Rust's standard reference counting (`Rc`/`RefCell`) for blazing-fast allocations, zero memory fragmentation, and perfect circular reference resolution.
* **Bytecode Virtual Machine:** Code is compiled into optimized OpCodes and executed on a stack-based VM.
* **Tail Call Optimization (TCO):** Write infinite recursive loops without ever blowing up the call stack. Memory stays at O(1).
* **Compile-Time Optimizations:** Features Constant Folding (e.g., `(+ 1 (* 2 3))` is optimized to `7` before the VM even sees it).
* **Data Literals:** Native support for contiguous Vectors `[]` and HashMaps `{}` straight in the parser, making data manipulation a breeze.
* **Pattern Matching:** A powerful `match` macro for elegant control flow and variable binding.
* **The Pipe Operator (`|>`):** Thread data through multiple functions sequentially, avoiding the dreaded "Lisp parenthesis pyramid".
* **Native JSON Support:** Integration with Rust's `serde_json` to parse internet payloads directly into Lisp AST structures.
* **Interactive REPL:** A beautiful, colored terminal interface built with `rustyline`, featuring multi-line validation, persistent history, and real-time bracket matching.

## 🚀 Quick Start

### Installation
Clone the repository and install it globally using Cargo:

```bash
git clone https://github.com/gabrielpacheco23/haki.git
cd haki
cargo install --path .
```

### Running Haki
Launch the interactive REPL:

```bash
haki
```

Or run a script:

```bash
haki script.hk
```

## 📖 Language Tour

Haki looks and feels like a traditional Lisp, but it packs modern syntactic sugar for handling real-world data.

### 1. Vectors and HashMaps
Forget endless `(make-hash)` and `(vector-set!)`. Haki supports native brackets and braces that compile down to fast structural data stored directly in the VM's Arena.

```scheme
;; Vector Literal
(define inventory ["Sword" "Potion" "Shield"])
(displayln (vector-ref inventory 1)) ;; Output: Potion

;; HashMap Literal (Great for JSON-like data)
(define player 
  { 
    "name" "Zoro"
    "class" "Swordsman"
    "stats" { "str" 20 "agi" 15 } 
  })

(displayln (hash-ref player "name")) ;; Output: Zoro
```

### 2. Pattern Matching
Clean up your logic with the built-in `match` macro.

```scheme
(define action '("equip" "Sword"))

(match action
  (("attack" target) (displayln (string-append "Attacking " target)))
  (("equip" item)    (displayln (string-append "Equipped " item)))
  (_                 (displayln "Unknown action")))
```

### 3. The Pipe Operator
Read your functional data transformations from top to bottom, not inside out.

```scheme
;; Using the pipe operator |>
(define sum-of-evens
  (|> [1 2 3 4 5 6]
      (vector->list)
      (filter even?)
      (map (lambda (x) (* x 10)))
      (fold + 0)))

(displayln sum-of-evens) ;; Output: 120
```

### 4. JSON & Web Requests
Haki is ready for the web. Use the native shell bridge and JSON parser to interact with APIs.

```scheme
;; Fetch data from an API and parse it natively
(define raw-response (shell "curl -s https://dummyjson.com/products/1"))
(define product (parse-json raw-response))

(display "Product Name: ")
(displayln (hash-ref product "title"))
```

## 🧠 Architecture

Even as a toy project, Haki implements a full-fledged compilation pipeline rather than a simple tree-walking interpreter:

1. **Lexer/Tokenizer:** Reads the `.hk` source code and breaks it down.
2. **Parser:** Builds the Abstract Syntax Tree (AST), translating `[]` and `{}` into Lisp function calls.
3. **Macro Expander:** A powerful hygienic pass that resolves `defmacro`, translating constructs like `cond`, `let`, `match`, and `|>` into core Lisp expressions.
4. **Optimizer:** Bottom-up AST traversal to perform Constant Folding.
5. **Compiler:** Translates the optimized AST into linear Bytecode (`Chunk` of `OpCodes`).
6. **Virtual Machine (VM) & GC:** Executes instructions using a pre-allocated stack. Memory is strictly managed by a custom flat-array Arena and a Mark-and-Sweep Garbage Collector, keeping the runtime exceptionally fast and memory-safe.

## 🚧 Future Roadmap (TODOs)

Haki is pretty much functional, but its not yet a full mature programming language. Here are the main goals for the future:

- [ ] **Expand the Standard Library:** Grow `std` and native Rust functions to include more robust file I/O operations, advanced math utilities, regex support, and deep string manipulation.
- [x] **Error Reporting:** Add line numbers, column tracking, and stack traces to make debugging Haki scripts even more developer-friendly.
- [x] **NaN Tagging (Optimization):** Optimize the memory footprint of `LispExp` down to 8 bytes by packing pointers and booleans inside the unused bits of an `f64` float.

## 🤝 Contributing
Contributions, issues, and feature requests are welcome! Feel free to check the [issues page](https://github.com/gabrielpacheco23/haki/issues).

---
*Developed with willpower.* ⚓
