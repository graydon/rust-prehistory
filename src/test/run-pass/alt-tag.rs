// -*- C -*-

type color = tag(
  rgb(int, int, int),
  rgba(int, int, int, int),
  hsl(int, int, int)
);

fn process(color c) -> () {
  alt (c) {
    case (rgb(r, g, b)) {
      log "rgb";
      log r;
    }
    case (rgba(r, g, b, a)) {
      log "rgba";
      log a;
    }
    case (hsl(h, s, l)) {
      log "hsl";
      log h;
    }
  }
}

fn main() -> () {
  color gray = rgb(127, 127, 127);
  color clear = rgba(50, 150, 250, 0);
  color red = hsl(0, 255, 255);
  process(gray);
  process(clear);
  process(red);
}

