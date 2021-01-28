open Belt;

let input =
  Node_fs.readFileAsUtf8Sync("input/Year2020_Day4.txt")
  ->Js.String2.split("\n\n");

module Passport = {
  type unvalidated = {
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

  type validated = {
    byr,
    iyr,
    eyr,
    hgt,
    hcl,
    ecl,
    pid,
    cid,
  };

  let parseExn = (s: string): option(unvalidated) => {
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

  let validateIntInRange = (s, min, max) => {
    switch (int_of_string(s)) {
    | v when v >= min && v <= max => v
    | _ => raise(Not_found)
    };
  };
  let validateByRe = (s, re) => {
    switch (Js.String2.match(s, re)) {
    | Some(a) when a->Array.getUnsafe(0) == s => s
    | _ => raise(Not_found)
    };
  };

  let validateByr = s => s->validateIntInRange(1920, 2002);
  let validateIyr = s => s->validateIntInRange(2010, 2020);
  let validateEyr = s => s->validateIntInRange(2020, 2030);
  let validateHgt = s => {
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
  let validateHcl = s => s->validateByRe([%re "/(#)[0-9a-f]{6}/"]);
  let validateEcl = s => {
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
  let validatePid = s => s->validateByRe([%re "/[0-9]{9}/"]);

  let validateExn = (r: unvalidated): option(validated) => {
    Some({
      byr: r.byr->validateByr,
      iyr: r.iyr->validateIyr,
      eyr: r.eyr->validateEyr,
      hgt: r.hgt->validateHgt,
      hcl: r.hcl->validateHcl,
      ecl: r.ecl->validateEcl,
      pid: r.pid->validatePid,
      cid: r.cid,
    });
  };
  let validate = p =>
    try(validateExn(p)) {
    | Not_found => None
    };
};

module Counter = {
  let countUnvalidatedPassports: array(Passport.unvalidated) => int = Array.length;
  let countValidPassports: array(Passport.validated) => int = Array.length;
};

// p1
let unvalidatedPassports = input->Array.keepMap(Passport.parse);
unvalidatedPassports->Counter.countUnvalidatedPassports->Js.log;

// p2
let passports = unvalidatedPassports->Array.keepMap(Passport.validate);
passports->Counter.countValidPassports->Js.log;

passports[0]->Js.log;
