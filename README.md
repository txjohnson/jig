jig
===

C++ templates for Parsing Expression Grammars

Overview
--------

**jig** is inspired by a previous project to rewrite Ian Piumarta's PEG/LEG [2] parser generator using C++. Although BNF syntax is quite easy to read, once actions and new features are introduced, the enhanced language starts to look more and more like the target language. Additionally, a generator adds another level of indirection when checking actions and parser behavior. But C++'s verbosity makes writing a parser directly, tedious. So can C++11's new features help? This project attempts to see how far we can go in approaching BNF's simplicity.

Usage
-----
*jig* is provided as a header only library. To use, simply include the single header file *jig.h* in your project. Templates are defined in the *jig* namespace. 

Within the file, elements that are meant for client use have the *JIG_PUBLIC* tag. Anything else is an implementation detail and subject to change.

###User Provided Types
A user must provide a source for characters. The interface for this source is given in the header file.

###Convenience Functions
Convenience template functions are used to create patterns. These functions deduce their types and instantiate a pattern accordingly. With these functions, it is not necessary to instantiate template patterns directly.

Architecture and Notes
----------------------
*jig* implements a recursive descent parser entirely in templates. A grammar is a collection of productions, each of which is a custom type based on its right hand side. The patterns on the RHS, in turn, are custom types based on their own respective arguments. Obviously, compiling a grammar may introduce a noticeable increase in compile times.

Grammar -> Productions -> Patterns

where patterns are a mixture of sequences, choices, repetition, tests, actions, and lexical matching.

Literals and scalars can be used in the specification of a pattern. *jig* attempts to convert these values to the correct pattern. Actions may be expressed using lambda expressions, std::functions or pointers to normal functions.

The resulting parser is not a packrat parser[2] since it does no memoization. The parser is able to handle direct and indirect left recursion but cannot partition the input to satisfy adjacent calls to the same recursive production. Like many packrat parsers, *jig* suffers from prefix capture.[3] 

Examples
--------
See the examples folder. Until a proper tutorial can be written, the examples will have to do. The first example implements PEG/LEG's BASIC interpreter in terms of jig. The second implements a modified version of Ghosh's stock order DSL.[4]

Preprocessor
------------
*jig* uses some preprocessor symbols to control code emission.

### MFPE_NO_CONSTRAINT_CHECKS 
Preliminary support for constraint checks on template parameters. The constraint checks insure compatibility between template arguments. The impact on performance is dependent on the compiler, but it should be negligible.

Define this symbol to disable constraint checks.

### JIG_LOGGING
Enables a logging interface on the grammar. See the header for the interface a compliant logger needs to support. When used, the grammar will write messages to the logger detailing its activity.

See Also
--------
[1] Ian Piumarta, peg/leg man page
[2] Bryan Ford, "Packrat Parsing: Simple, Powerful, Lazy, Linear Time"
[3] Roman R. Redziejowski, "Applying Classical Concepts to Parsing Expression Grammar∗"
[4] Debasish Ghosh, "DSL's in Action" 2011

fin