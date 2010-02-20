// -*- C -*-

type color = tag(
  rgb(int, int, int),
  rgba(int, int, int, int),
  hsl(int, int, int)
);

fn process(color c) -> () {
  alt (c) {
    case (rgb(int r, int g, int b)) {
      log "rgb";
      log r;
    }
    case (rgba(int r, int g, int b, int a)) {
      log "rgba";
      log a;
    }
    case (hsl(int h, int s, int l)) {
      log "hsl";
      log h;
    }
  }
}

fn main() -> () {
  let color gray = rgb(127, 127, 127);
  let color clear = rgba(50, 150, 250, 0);
  let color red = hsl(0, 255, 255);
  process(gray);
  process(clear);
  process(red);
}

