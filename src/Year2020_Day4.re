let input =
  Node_fs.readFileAsUtf8Sync("input/Year2020_Day4.txt")
  ->Js.String2.split("\n\n");

module Passport = {
  type unparsed_t = {
    byr: string,
    iyr: string,
    eyr: string,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option(string),
  };

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

  let read = str => {
    let readExn = (s: string): option(unparsed_t) => {
      let tokens = s->Js.String2.splitByRe([%re "/\s/"]);
      let kv =
        tokens
        ->Belt.Array.map(token => {
            let res =
              Js.String2.split(token->Belt.Option.getWithDefault(""), ":");
            (res->Belt.Array.getUnsafe(0), res->Belt.Array.getUnsafe(1));
          })
        ->Belt.Map.String.fromArray;

      Some({
        byr: kv->Belt.Map.String.getExn("byr"),
        iyr: kv->Belt.Map.String.getExn("iyr"),
        eyr: kv->Belt.Map.String.getExn("eyr"),
        hgt: kv->Belt.Map.String.getExn("hgt"),
        hcl: kv->Belt.Map.String.getExn("hcl"),
        ecl: kv->Belt.Map.String.getExn("ecl"),
        pid: kv->Belt.Map.String.getExn("pid"),
        cid: kv->Belt.Map.String.get("cid"),
      });
    };

    try(readExn(str)) {
    | Not_found => None
    };
  };

  let parseIntInRange = (s, min, max) => {
    switch (int_of_string(s)) {
    | v when v >= min && v <= max => v
    | _ => raise(Not_found)
    };
  };

  let parseByRe = (s, re) => {
    switch (Js.String2.match(s, re)) {
    | Some(a) when a[0] == s => s
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

  let parse = unparsed => {
    let parseExn = (r: unparsed_t): option(t) => {
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

    try(parseExn(unparsed)) {
    | Not_found => None
    };
  };
};

module Counter = {
  let countUnparsedPassports: array(Passport.unparsed_t) => int = Belt.Array.length;
  let countPassports: array(Passport.t) => int = Belt.Array.length;
};

// p1
let unparsedPassports = input->Belt.Array.keepMap(s => s->Passport.read);
unparsedPassports->Counter.countUnparsedPassports->Js.log;

// p2
let passports = unparsedPassports->Belt.Array.keepMap(r => r->Passport.parse);
passports->Counter.countPassports->Js.log;
