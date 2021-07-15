open Belt;

let input =
  Node_fs.readFileAsUtf8Sync("input/Year2020_Day4.txt")
  ->Js.String2.split("\n\n");

module NaivePassport = {
  type t = {
    byr: string,
    iyr: string,
    eyr: string,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option(string),
  };

  let parseExn = (s: string): option(t) => {
    let tokens = s->Js.String2.splitByRe([%re "/\s/"]);
    let kv =
      tokens
      ->Array.map(token => {
          let res = Js.String2.split(token->Option.getWithDefault(""), ":");
          (res->Array.getUnsafe(0), res->Array.getUnsafe(1));
        })
      ->Map.String.fromArray;
    Some({
      byr: kv->Map.String.getExn("byr"),
      iyr: kv->Map.String.getExn("iyr"),
      eyr: kv->Map.String.getExn("eyr"),
      hgt: kv->Map.String.getExn("hgt"),
      hcl: kv->Map.String.getExn("hcl"),
      ecl: kv->Map.String.getExn("ecl"),
      pid: kv->Map.String.getExn("pid"),
      cid: kv->Map.String.get("cid"),
    });
  };

  let parse = s =>
    try(parseExn(s)) {
    | Not_found => None
    };
};

module Passport = {
  type byr = int;
  type iyr = int;
  type eyr = int;
  type hgt =
    | In(int)
    | Cm(int);
  type hcl = string;
  type ecl =
    | AMB
    | BLU
    | BRN
    | GRY
    | GRN
    | HZL
    | OTH;
  type pid = string;
  type cid = option(string);

  type t = {
    byr,
    iyr,
    eyr,
    hgt,
    hcl,
    ecl,
    pid,
    cid,
  };

  let parseIntInRange = (s, min, max) => {
    switch (int_of_string(s)) {
    | v when v >= min && v <= max => v
    | _ => raise(Not_found)
    };
  };
  let parseByRe = (s, re) => {
    switch (Js.String2.match(s, re)) {
    | Some(a) when a->Array.getUnsafe(0) == s => s
    | _ => raise(Not_found)
    };
  };

  let parseByr = s => s->parseIntInRange(1920, 2002);
  let parseIyr = s => s->parseIntInRange(2010, 2020);
  let parseEyr = s => s->parseIntInRange(2020, 2030);
  let parseHgt = s => {
    switch (
      Js.String2.substr(s, ~from=-2),
      Js.String2.substrAtMost(s, ~from=0, ~length=Js.String2.length(s) - 2)
      ->int_of_string_opt,
    ) {
    | ("cm", Some(v)) when v >= 150 && v <= 193 => Cm(v)
    | ("in", Some(v)) when v >= 59 && v <= 76 => In(v)
    | _ => raise(Not_found)
    };
  };
  let parseHcl = s => s->parseByRe([%re "/(#)[0-9a-f]{6}/"]);
  let parseEcl = s => {
    switch (s) {
    | "amb" => AMB
    | "blu" => BLU
    | "brn" => BRN
    | "gry" => GRY
    | "grn" => GRN
    | "hzl" => HZL
    | "oth" => OTH
    | _ => raise(Not_found)
    };
  };
  let parsePid = s => s->parseByRe([%re "/[0-9]{9}/"]);

  let parseExn = (r: NaivePassport.t): option(t) => {
    Some({
      byr: r.byr->parseByr,
      iyr: r.iyr->parseIyr,
      eyr: r.eyr->parseEyr,
      hgt: r.hgt->parseHgt,
      hcl: r.hcl->parseHcl,
      ecl: r.ecl->parseEcl,
      pid: r.pid->parsePid,
      cid: r.cid,
    });
  };

  let parse = p =>
    try(parseExn(p)) {
    | Not_found => None
    };
};

// p1
input->Array.keepMap(NaivePassport.parse)->Array.length->Js.log;

// p2
input
->Array.keepMap(NaivePassport.parse)
->Array.keepMap(Passport.parse)
->Array.length
->Js.log;
