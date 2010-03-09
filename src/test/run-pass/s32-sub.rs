// -*- C -*-

fn main() {
    let s32 x = s32(-400);
    x = s32(0) - x;
    check(x == s32(400));
}

