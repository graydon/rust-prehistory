fn main() {
  let char yen = '¥';         // 0xa5
  let char c_cedilla = 'ç';   // 0xe7
  let char thorn = 'þ';       // 0xfe
  let char y_diaeresis = 'ÿ'; // 0xff
  let char pi = 'Π';          // 0x3a0

  check (int(yen) == 0xa5);
  check (int(c_cedilla) == 0xe7);
  check (int(thorn) == 0xfe);
  check (int(y_diaeresis) == 0xff);
  check (int(pi) == 0x3a0);

  let str bhutan = "འབྲུག་ཡུལ།";
  let str japan = "日本";
  let str uzbekistan = "Ўзбекистон";
  let str austria = "Österreich";
}