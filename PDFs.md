PDFs and reference material
---------------------------

Someone recently asked for a listing of the redacted reference
material and PDFs in this repository, as though I maybe had deep or
interesting influences I haven't enumerated elsewhere. I wish! It was
just a handful of interesting papers (mostly related to the design of
module systems) as well as specs for various systems level stuff
needed to write rustboot.

Keep in mind that rustboot started with an AST interpreter, then a
stack-machine interpreter, but then I decided I was at risk of being
constrained by platform tooling and/or at risk of fooling myself about
implementation requirements a native compiler was going to face, so I
decided to do a for-real native compiler, and started at the very
bottom: byte-at-a-time 32-bit x86 instruction encoding.

The compiler then grew _backwards_ from the instruction emitter, back
through a custom object file emitter for ELF and PE (and later
Mach-O), and finally an IR, register allocator and lowering pass from
the parser. This also included both its own DWARF emitter _and parser_
which it used to serialize and deserialize all crate-level metadata,
as well as resolve any runtime dynamic linking between crates. So I
actually spent a lot of the early days just groveling around in the
internals of low level formats, and needed all the documentaiton I
could get.

Here is a list from the last directory I have kicking around:

  - The Intel 64 and IA-32 Architectures Software Developers Manual

  - Apple Developer Connection's Mac OS X ABI Mach-O File Format Reference.
  
  - The Objective Caml system release 3.10 Documentation and user's manual.
  
  - IEEE Std 1003.1, 2004 Edition a.k.a. the Single Unix Standard v3.
  
  - Program Fragments, Linking and Modularization. Luca Cardelli, DEC SRC
    Research Report 144, 1997.

  - A Framework for Reducing the Cost of Instrumented Code. Matthew Arnold
    and Barbara G. Ryder, both of Rutgers University but working at IBM
    T.J. Watson Research Center, PLDI 2001.

  - Mixin' Up the ML Module System. Derek Dreyer and Andreas Rossberg,
    MPI-SWS, preprint, 2008.

  - How To Write Shared Libraries. Ulrich Drepper, Red Hat, Inc. 2006.

  - DWARF Debugging Information Format Specification versions 2 (Tool
    Interface Standards Committee, 1995) and 3 (Free Standards Group,
    2005).

  - Efficient and Flexible Value Sampling. Burrows et al. Compaq SRC
    Research Report 166, 2000.

  - Executable and Linking Format (ELF) Specification version 1.2.
    Tool Interface Standards Committee, 1995.

  - Programming Languages for Reusable Software Components. Matthew Flatt,
    PhD Thesis, Rice University, 1999.

  - Programming Languages -- C. ISO WG14/N843 Committee Draft, 1998.

  - Mac OS X Mach-O File Format Reference. Apple, 2007.

  - Mach-O Programming Topics. Apple, 2006.

  - Relocatable Object Module Format (OMF) Specification version 1.1. Tool
    Interface Standards Committee, 1995.

  - Portable Executable Formats (PE). Tool Interface Standards. Undated.

  - Visual Studio, Microsoft Portable Executable and Common Object
    File Format Specification, version 8.0. Microsoft, 2006.

  - A Primitive Calculus for Module Systems. Davide Ancona and Elena
    Zucca. University of Genoa, 2006.

  - Microsoft Symbol and Type Information. Tool Interface
    Standards. Undated.

  - From Structures and Functors to Modules and Units. Scott Owens and
    Matthew Flatt. University of Utah, 2006.

  - Value Profiling. Brad Calder, Peter Feller (UCSD) and Alan Eustace
    (DEC WRL). IEEE Micro-30, 1997.

  - Formats Specification for Windows, version 1.0. Tool Interface
    Standards Committee, 1993.

  - Alef Language Reference Manual. Phil Winterbottom. AT&T, 1995.
