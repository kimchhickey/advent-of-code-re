module type Arithmetic = {
  type t;
  let (+): (t, t) => t;
  let fromString: string => t;
};

module Number = (A: Arithmetic) => {
  type t = A.t;
  let (+) = A.(+);
  let equal = (a, b) => a === b;
  let compare = ((a, b), c) => equal(a + b, c);
  let findFirstPair = (l, t) => {
    let rec compareItemToList = (value, list, target) => {
      switch (list) {
      | [] => Belt.Result.Error("Not Found")
      | [head, ...rest] =>
        if (compare((value, head), target)) {
          Belt.Result.Ok((value, head));
        } else {
          compareItemToList(value, rest, target);
        }
      };
    };
    let rec compareListToList = (list, originList, target) => {
      switch (list) {
      | [] => Belt.Result.Error("Not Found")
      | [head, ...rest] =>
        switch (compareItemToList(head, originList, target)) {
        | Belt.Result.Ok(answer) => Belt.Result.Ok(answer)
        | Belt.Result.Error(_) => compareListToList(rest, originList, target)
        }
      };
    };
    compareListToList(l, l, t);
  };
  let fromString = A.fromString;
};

module Int: Arithmetic with type t = int = {
  type t = int;
  let (+) = (+);
  let fromString = s => s->Belt.Int.fromString->Belt.Option.getWithDefault(0);
};

module Float: Arithmetic with type t = float = {
  type t = float;
  let (+) = (+.);
  let fromString = s =>
    s->Belt.Float.fromString->Belt.Option.getWithDefault(0.0);
};

module IntNumber = Number(Int);
module FloatNumber = Number(Float);
