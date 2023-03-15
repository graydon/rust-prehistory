Rust Prehistory Repo - November 2016
====================================

Hi,

This is a reconstruction -- extracted and very lightly edited -- of the
"prehistory" of Rust development, when it was a personal project between
2006-2009 and, after late 2009, a Mozilla project conducted in private.

The purposes of publishing this now are:

  1. It might encourage someone with a hobby project to persevere
  2. It might be of interest to language or CS researchers someday
  3. It might settle some arguments / disputed historical claims


Edits made
----------

I have edited it from its original archival forms only in the following
ways:

  1. Converted from Monotone and Mercurial revision control
  2. Knitted together history by date, sometimes a bit roughly
  3. Deleted PDFs and other reference material I had put in the
     repository for reference, but I have no right to redistribute.
  4. Deleted commits or files that are, by my own judgment,
     too embarrassing or too worthless to publish.

Thus there are probably a number of historical states that are not
bit-identical to the form they were at the moment of initial writing -- I
know at least a couple edits might have broken the build in intermediate
states -- but they're as close as I can reasonably make them. This is an
honest attempt at a final word on the prehistory (and has taken some time
to dig up!)


Caveats (i.e. "don't expect much")
----------------------------------

While reading this -- if you're foolish enough to try -- keep in mind that
I was balanced between near-total disbelief that it would ever come to
anything and minuscule hope that if I kept at experiments and fiddling long
enough, maybe I could do a thing.

I had been criticizing, picking apart, ranting about other languages for
years, and making doodles and marginalia notes about how to do one "right"
or "differently" to myself for almost as long. This lineage represents the
very gradual coaxing-into-belief that I could actually make something that
runs.

As such, there are long periods of nothing, lots of revisions of position,
long periods of just making notes, arguing with myself, several false
starts, digressions into minutiae that seem completely absurd from today's
vantage point (don't get me started on how long I spent learning x86 mod
r/m bytes and PE import table structures, why?) and self-important
frippery.

The significant thing here is that I had to get to the point of convincing
myself there was something _there_ before bothering to show anyone; the
uptick in work in mid-to-late 2009 is when Mozilla started funding me on
the clock to work on it, but it's significant that there were years and
years of just puttering around in circles, the kind of snowball-rolling
that's necessary to go from nothing to "well .. maybe .."

I'd encourage reading it in this light: delusional dreams very gradually
coming into focus, not any sort of grand plan being executed.

Linking history in Git
----------------------

In order to browse the full history of the Rust project, you can link this
history with that of the [active Rust repository].  To do so, clone the Rust
repository, add a remote for the prehistory repository, then replace the first
commit in the Rust repository with the last commit in the prehistory (the last
one before the introduction of this README, to be more precise).

    git clone https://github.com/rust-lang/rust.git
    cd rust
    git remote add prehistory https://github.com/graydon/rust-prehistory.git
    git remote update prehistory
    git replace c01efc669f09508b55eced32d3c88702578a7c3e c10ce161d8a58d78e10583fcea7e34eab0a518d0

Now you should be able to see, for example, the full history of what "hello,
world" looked like in Rust over the years:

    git log -p --follow src/test/ui/hello.rs

[active Rust repository]: https://github.com/rust-lang/rust/

License
-------

I'm publishing this work with, as far as I can tell, permission of its
copyright holders: myself independently, and Mozilla Foundation. If you
feel I shouldn't have published this for some reason, please contact me and
I will happily discuss further. I tried to keep ownership and authorship
straight while working.

The current work is thereby licensed under the terms of the MIT/ASL2 dual
license scheme that the rest of Rust is licensed under. I do not expect
anyone cares about the license status, but there it is. If you feel like
reviving it for use in some sort of exercise in self-punishment, knock
yourself out.


-Graydon Hoare, November 2016
